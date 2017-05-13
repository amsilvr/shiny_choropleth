## CMAS Clean

library(googlesheets)
library(tidyverse)
library(lubridate)
library(stringr)
library(sf)
#library(rgdal)

ss_new <- gs_key("1Xw4JefUCS4HHQ0KpvKhr-DjklqzhH3_CeA-zhoAuQfI", visibility = "private") #CMAS_Alerts_Processed
day_file_name <- paste0(today(),"-msgfile.csv")

load_msgs <- function() {
        
   if (file.exists(day_file_name)) {
     msg <- read_csv(day_file_name) %>% select(-X1)
   } 
   
  else   
    msg <-  gs_read_csv(ss = ss_new
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
       # write.csv(msg, file = day_file_name) commented out for shinyapps version
       return(msg)
       
        } 
# State and Territory Lookup

map_states <- function() {
  state_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
} 

state_iso <- read_csv("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"
                      , col_names = c("iso_3166_2"
                                      , "STATEFP"
                                      , "COUNTYFP"
                                      , "name","x")) %>%
    select(2,1) %>%
    unique()


map_counties <- function() {
  # Download Shapefiles
  countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
  if (!dir.exists("data")) {dir.create("data")}
  if (!file.exists("data/county_shape_file.zip")) {
    download.file(countyshapes_url
                  , destfile = "data/county_shape_file.zip")
  }
    t <- unzip("data/county_shape_file.zip", exdir = "data")
   
 
   # Read the file with sf
tmp <- st_read(t[grep("shp$",t)]) %>%
  left_join(state_iso) %>%
  group_by(STATEFP, COUNTYFP)
  tmp$NAME <- str_replace_all(tmp$NAME, pattern = "Ã±",replacement = "n") %>%
  str_replace_all("Ã¡",replacement = "a") %>%
  str_replace_all("Ã¼",replacement = "u") %>%
  str_replace_all("Ã³",replacement = "o") %>%
  str_replace_all("Ã",replacement = "i")
  return(tmp)
}
  

## Download local copy of FIPS lookup data and read into memory
load_fips <- function() {
  counties_sf <- map_counties() %>%
    mutate(areaname = paste(iso_3166_2, NAME)) %>%
    return()
  
  }
## isolates the state and county, given column with 
## comma separated list in format "city (ST)"
# 
# right_join(areas) %>%
#   select(msg_id, GEOID) %>%
#   rbind(areas_states)

lsad_lookup <- function() {
  url <- "https://www.census.gov/geo/reference/lsad.html"
  lsad <- htmltab::htmltab(doc = url, which = "//th[text() = 'LSAD']/ancestor::table") %>%
    filter(grepl("06|04|12|05|03|00|15|25|13", LSAD) == TRUE) %>%
    transmute(LSAD,description = `LSAD Description`) %>%
    mutate(description = 
                str_extract(pattern = "^[^(]*",string = description) %>%
                str_trim()) %>%
    replace_na(list(LSAD = "", description = "")) %>%
    filter(!description == "Balance of County EC Place")
  
}

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
        
  # ## Clean TCS county names to match list from census county map
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
  
          left_join(areas_states, fips_lookup, by = c("areas" = "iso_3166_2")) %>%
        select(msg_id,GEOID) %>%
        return()
}

flatten_fips <- function(msg) {
  if (!exists("fips_lookup")) fips_lookup <- load_fips()
  areas <- transmute(msg
                     , msg_id = as.character(msg_id)
                     , areas)
  #separate out alerts with full state areas, convert directly to fips
  areas_states <- filter(areas, str_length(areas) == 2) %>%
    full_state()
  #remove those alerts from the other areas
  areas <- filter(areas, str_length(areas) > 2)
  
  # create a matrix of areas for each message id that has individual counties
  areas <- tapply(areas$areas, area_find
                  , INDEX = areas$msg_id
                  , simplify = TRUE) %>%
      as.data.frame.array() %>%
      unlist(recursive = TRUE) %>%
      as_tibble(validate = TRUE) %>%
      rownames_to_column() %>%
      transmute(msg_id = as.character(str_extract(rowname, "[[:alnum:]]{8}"))
                                     , areaname = value) %>%
  # Join messages with FIPS codes by matching areanames
  
    left_join(fips_lookup) %>%
    transmute(msg_id, areaname, GEOID = as.character(GEOID))
  
  # Fix the 18 that don't seem to match for whatever reason
  areas <-  mutate(areas, GEOID = 
             case_when(
                      grepl("MD Baltimore city",areas$areaname) ~ "24005",
                      grepl("SD Shannon",areas$areaname) ~ "46113",
                      grepl("TX Wall",areas$areaname) ~ "48473",
                      grepl("NV Carson",areas$areaname) ~ "32510",
                      grepl("PR Rio Grande",areas$areaname) ~ "72119",
                      grepl("PR Manati",areas$areaname) ~ "72091",
                      grepl("PR Juana Diaz",areas$areaname) ~ "72075",
                      grepl("PR Loiza",areas$areaname) ~ "72087",
                      grepl("LA La Salle",areas$areaname) ~ "22059",
                      grepl("SD Pennington city",areas$areaname) ~ "46103",
                      grepl("PR Las Marias",areas$areaname) ~ "72083",
                      grepl("CA Lake city",areas$areaname) ~ "06033",
                      grepl("PR Comerio",areas$areaname) ~ "72045",
                      grepl("KY Carter city",areas$areaname) ~ "21043",
                      grepl("VA Roanoke city",areas$areaname) ~ "51770",
                      grepl("MN McLeod city",areas$areaname) ~ "27085",
                      TRUE ~ areas$GEOID
                )
      )    %>%
    select(-areaname) %>%
    rbind(areas_states) %>%
    return()
 
  
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
    transmute(msg_id = as.character(msg_id)
              , rec_time
              , expire_time
              , response = response_type
              , urgency
              , wea = message_text 
              , type = as.factor(type)
              , areas
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
  if (file.exists(day_file_name)) {
      msg <- read.csv(day_file_name)
      }
  else
    msg <- load_msgs()
  }


msg2 <- classify_message(msg) %>% unique()

if (!exists("fips_msg")) {
   fips_msg <- flatten_fips(msg2)
}


alert_tally <- left_join(msg2, fips_msg) %>%
        select(msg_id, GEOID, type) %>%
        group_by(GEOID, type) %>%
        tally() %>%
        rename(WEATYPE = type, WEANUM = n) %>%
        spread(WEATYPE, WEANUM, fill = "0",drop = TRUE, convert = TRUE) %>%
        mutate(Total = AMBER + FlashFlood
               + Other + Tornado
               + Tsunami)

        