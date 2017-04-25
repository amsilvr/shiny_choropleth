## CMAS Clean

require(googlesheets)
require(tidyverse)
require(lubridate)
require(stringr)


ss_new <- gs_key("1Xw4JefUCS4HHQ0KpvKhr-DjklqzhH3_CeA-zhoAuQfI", visibility = "private") #CMAS_Alerts_Processed
day_file_name <- paste0(today(),"-msgfile.csv")

load_msgs <- function() {
        msg <- gs_read(ss = ss_new
                       , col_names = c("rec_time", "cmac", "full_text")
                       , coltypes = "Tcc", skip = 1, trim_ws = TRUE) %>%
                mutate(rec_time = mdy_hms(gsub(" at ", " ", rec_time)
                                          , tz = "America/New_York"
                                          , truncated = 3) 
                ) %>%
                separate(full_text,
                         c("blank", "gateway_id" ,"msg_id"
                           ,"special_handling", "message_type" 
                           , "category", "response_type", "severity" 
                           , "urgency", "certainty", "expire_time"
                           , "text_language", "alert_message","dummy")
                         , sep = "CMAC_[:word:]*: "
                         , fill = "right" ## drops the warning for rows with too many records
                         , remove = TRUE
                ) 
        
        
        ## creates a table for fields with "update" records
        ######################
        # for working offline
        # msg <- read_csv("msgfile.csv")
        #######################
        updates <- filter(msg, nchar(special_handling) < 10) %>%
                select(rec_time, cmac, gateway_id, msg_id
                       , ref_id = special_handling
                       , special_handling = message_type
                       , message_type = category
                       , category = response_type
                       , response_type = severity
                       , severity = urgency 
                       , urgency = certainty 
                       , certainty = expire_time 
                       , text_language = alert_message 
                       , alert_message = dummy
                )
        
        msg <- filter(msg, nchar(special_handling) >= 10) %>%
                select(-blank, -dummy)
        
        ## puts all the records back into a single table and 
        ## uses two different separators to split out the alert 
        ## text from the plain English "area" field
        ## and finally removes the tcs boilerplate
        
        msg <- bind_rows(msg, updates) %>%
                mutate(expire_time = ymd_hms(expire_time)) %>%
                separate(alert_message, c("message_text","t2")
                         , sep = "Targeted Areas: "
                         , remove = TRUE) %>%
                separate(t2, c("areas"), sep = "[:punct:]{4}"
                         , extra = "drop", remove = TRUE) %>%
                mutate(threat_type = gsub("\\. .*","", cmac)
                        , msg_id = str_trim(msg_id)
                        , areas = str_trim(areas)) %>%
                dplyr::filter(!(gateway_id == "http://tcs.tsis.com\n") ) 
        
        msg <- msg[-grep(" test", msg$threat_type),] 
       write.csv(msg, file = day_file_name) 
       return(msg)
       
        } 


## Download local copy of FIPS lookup data and read into memory
load_fips <- function() {
        
  fips_lookup <- albersusa::counties_sf() %>%
        transmute(fips, name, lsad
                  , state, abb = iso_3166_2
                  , value = paste(abb, name))
}

## isolates the state and county, given column with 
## comma separated list in format "city (ST)"

area_find <- function(area_list) { 
        area_list <- str_replace_all(area_list
                     , pattern = "(([A-z]*) \\(([A-Z]{2})\\)), \\1"
                     , replacement = "\\2 city \\(\\3\\), \\2 \\(\\3\\)"
                     )

        m <- str_match_all(string = area_list
                             , pattern = "[A-z. ]{3,} ")
        
        n <- str_match_all(string = area_list
                           , pattern = "\\(?([A-Z]{2})\\)?")
        
        area_clean <- paste(n[[1]][,2]
                            , str_trim(m[[1]][,1], side = "both")) %>%
        
  # ## Clean TCS county names to match list from albersusa county map
  # ##       
        str_replace_all(pattern = "E\\.",replacement = "East") %>%
        str_replace_all(pattern = "W\\.",replacement = "West") %>%
        str_replace_all(pattern = "(IN La|IL La) (Porte|Salle)",replacement = "\\1\\2") %>%
        str_replace_all(pattern = "FL Dade", "FL Miami-Dade") %>%
        str_replace_all(pattern = "PR lsabela", "PR Isabela") %>%
        str_replace_all(pattern = "TX wall", "TX Wall") %>%
        str_replace_all(pattern = "TX hell", "TX Hall") %>%
        str_replace_all(pattern = "MT Lewis Clark", "MT Lewis and Clark")
       return(area_clean)

}

## Substitutes all counties in a state 
## For areas that include only state names

full_state <- function(areas_states) {
        if (!exists("fips_lookup")) fips_lookup <- load_fips()
         left_join(areas_states, fips_lookup, by = c("areas" = "abb")) %>%
         select(msg_id,fips) %>%
         return()
}

flatten_fips <- function(msg) {
  
  areas <- select(msg, msg_id, areas)
  #separate out alerts with full state areas, convert directly to fips
  areas_states <- filter(areas, str_length(areas) == 2) %>%
    full_state()
  #remove those alerts from the other areas
  areas <- filter(areas, str_length(areas) > 2)
  
  if (!exists("fips_lookup")) fips_lookup <- load_fips()
  
  # create a matrix of areas for each message id that has individual counties
  areas <- tapply(areas$areas, area_find
                  , INDEX = areas$msg_id
                  , simplify = TRUE) %>%
      as.data.frame.array() %>%
      unlist(recursive = TRUE) %>%
      as_tibble(validate = TRUE) %>%
      rownames_to_column() %>%
      transmute(msg_id = str_extract(rowname, "[[:alnum:]]{8}")
                                     , value) 

  fips_msg <- inner_join(areas, fips_lookup) %>%
          select(msg_id, fips) %>%
          rbind(areas_states)

}

# Classify message type - 
# Tornado, Flash Flood, 
# AMBER, Tsunami, or Other

classify_message <- function(msg) {
  
  mutate(msg, type = 
           case_when(
             grepl("Tornado", msg$message_text, ignore.case = TRUE) ~ "Tornado",
             grepl("Flash Flood", msg$message_text, ignore.case = TRUE) ~ "FlashFlood", 
             grepl("Amber", msg$message_text, ignore.case = TRUE) ~ "AMBER",
             grepl("Tsunami", msg$message_text, ignore.case = TRUE) ~ "Tsunami",
             TRUE ~ "Other")
  ) %>%
    transmute(msg_id
              , rec_time
              , expire_time
              , response = response_type
              , urgency
              , wea = message_text 
              , type = as.factor(type)
    ) 
}

#####################
## Run Functions  ###
#####################

# If msg isn't in memory, check to see if we have already 
# downloaded the data and cleaned it today. If so, get 
# it from the csv created on today's date.
# If we haven't created the msg df today, then load 
# from the google sheet

if (!exists("msg")) {
  #if (file.exists(day_file_name)) msg <- read.csv(day_file_name)
  #else
    msg <- load_msgs()
  }

if (!exists("fips_msg")){
   fips_msg <- flatten_fips(msg)
}

msg2 <- classify_message(msg) %>% unique()

alert_tally <- left_join(msg2, fips_msg) %>%
        select(msg_id, fips, type) %>%
        count(fips, type) %>%
        rename(WEATYPE = type, WEANUM = n) %>%
        spread(WEATYPE, WEANUM, fill = "0",drop = TRUE, convert = TRUE) 

        