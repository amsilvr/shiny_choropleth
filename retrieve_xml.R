# Script for retrieving xml from text file
library(tidyverse)
library(stringr)
library(lubridate)

# Create new class for of CMAC Message
cmac_s3 <- function(msg){
  structure(
    list(protocol_version = msg[['CMAC_protocol_version']],
          sending_gateway = msg[['CMAC_sending_gateway_id']],
          id = msg[['CMAC_message_number']],
          cap_ref = msg[['CMAC_referenced_message_cap_identifier']],
          sender_email = msg[['CMAC_sender']],
          sent = msg[['CMAC_sent_date_time']],
          status = msg[['CMAC_status']],
          msg_type = msg[['CMAC_message_type']],
          resp_code = msg[['CMAC_response_code']],
          note = msg[['CMAC_note']],
          cap_url = msg[['CMAC_cap_alert_uri']],
          cap_id = msg[['CMAC_cap_identifier']],
          cap_sent = msg[['CMAC_cap_sent_date_time']],
          category = msg[['CMAC_alert_info']][['CMAC_category']],
          event = msg[['CMAC_alert_info']][['CMAC_event_code']],
          response = msg[['CMAC_alert_info']][['CMAC_response_type']],
          severity = msg[['CMAC_alert_info']][['CMAC_severity']],
          urgency = msg[['CMAC_alert_info']][['CMAC_urgency']],
          certainty = msg[['CMAC_alert_info']][['CMAC_certainty']],
          expires = msg[['CMAC_alert_info']][['CMAC_expires_date_time']],
          sender_name = msg[['CMAC_alert_info']][['CMAC_sender_name']],
          lang = msg[['CMAC_alert_info']][['CMAC_text_language']],
          lngth = msg[['CMAC_alert_info']][['CMAC_text_alert_message_length']],
          alert_msg = msg[['CMAC_alert_info']][['CMAC_text_alert_message']],
          description = msg[['CMAC_alert_info']][['CMAC_Alert_Area']][['CMAC_area_description']],
          polygon = msg[['CMAC_alert_info']][['CMAC_Alert_Area']][['CMAC_polygon']],
          geoid = msg[['CMAC_alert_info']][['CMAC_Alert_Area']][['CMAC_cmas_geocode']],
          cap_geoid = msg[['CMAC_alert_info']][['CMAC_Alert_Area']][['CMAC_cap_geocode)']]) %>%
      unlist(use.names = TRUE)
  , class = "cmac_s3")
}

# Set apply method for cmac_s3 class


library(XML)
# Clean up csv artifacts and parse xml
cmac <- read_file("data/cmacs_spool.txt") %>%
  {paste0(str_sub(., end = 56)
         #leave first xml tag
      , "<CMAC_Alerts>" #create root node
      #only call xml once    
      , str_replace_all( 
              str_sub(., start = 57),'<\\?xml[^>]*>','') %>% 
            #remove extra commas and quotes
              str_replace_all('",*"*','') %>%
            #remove all the other extra commas
              str_replace_all('>,+','>')
      , "</CMAC_Alerts>"
    )} %>%
  #remove extra double-quotes  
  str_replace('"','') %>%
  str_replace_all('xmlns=cmac:1.0', 'xmlns="cmac:1.0"') %>%
  
  read_xml(NSCLEAN) %>%
  as_list()
  
##################
# Create a data frame from the full CMAC output

cmac_df <- sapply(cmac, cmac_s3) %>%
  t() %>%
  as_data_frame() %>%
  transmute(id, sent = ymd_hms(sent),
            status, msg_type, cap_id, cap_url,
            cap_sent = ymd_hms(cap_sent),
            category, event, response, severity, urgency, certainty,
            expires = ymd_hms(expires), sender_name, sender_email, lang, 
            alert_msg, area_description = description, 
            geoid, polygon)

  
    
