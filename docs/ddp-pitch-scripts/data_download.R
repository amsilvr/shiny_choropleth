library(knitr)
library(tidyverse)
library(lubridate)
library(RCurl)
#create download and save-file URLs

## To analyze most recent data uncomment the following two lines
url <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
filenames <-  getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

## To speed up for testing, ftp docs to "data/" and uncomment the next two lines
## 
# url <- "data/"
# filenames <- dir("data/")

dld_base <- "StormEvents_details-ftp_v1.0_d"

if (!file.exists("data")) {
        dir.create("data")
}

if (!exists("weather_event_df")) {
  # If not, get to work
  
  filenames <- strsplit(filenames, "\r\n")
  filenames = unlist(filenames)
  
  # detail.files contains the list of the files that we actually
  # want to download. In this case 1997 - 2016
  
  detail.files <- filenames[grep(dld_base, filenames)]
  filenames = ""
  for (i in 1997:2016) {
    filenames[i - 1996] <-
      detail.files[grep(paste0(".*_d", i, "_.*"), detail.files)]
  }
  
  # read files into Weather Event Data Frame (wedf)
  weather_event_df <- data.frame()
  
  for (filename in filenames) {
    # message("downloading and parsing ", filename)
    weather_event_df  <- bind_rows(weather_event_df,
                      read_csv(
                        file = (paste0(url, filename))
                        , col_types =
                          cols(
                            BEGIN_YEARMONTH = "i",
                            BEGIN_DAY = "i",
                            BEGIN_TIME = "c",
                            END_YEARMONTH = "i",
                            END_DAY = "i",
                            END_TIME = "c",
                            EPISODE_ID = "i",
                            EVENT_ID = "i",
                            STATE = "c",
                            STATE_FIPS = "i",
                            YEAR = "i",
                            MONTH_NAME = "c",
                            EVENT_TYPE = "c",
                            CZ_TYPE = "c",
                            CZ_FIPS = "i",
                            CZ_NAME = "c",
                            WFO = "c",
                            BEGIN_DATE_TIME = "c",
                            CZ_TIMEZONE = "c",
                            END_DATE_TIME = "c",
                            INJURIES_DIRECT = "i",
                            INJURIES_INDIRECT = "i",
                            DEATHS_DIRECT = "i",
                            DEATHS_INDIRECT = "i",
                            DAMAGE_PROPERTY = "c",
                            DAMAGE_CROPS = "c",
                            SOURCE = "c",
                            MAGNITUDE = "d",
                            MAGNITUDE_TYPE = "c",
                            FLOOD_CAUSE = "c",
                            CATEGORY = "d",
                            TOR_F_SCALE = "c",
                            TOR_LENGTH = "d",
                            TOR_WIDTH = "d",
                            TOR_OTHER_WFO = "c",
                            TOR_OTHER_CZ_STATE = "c",
                            TOR_OTHER_CZ_FIPS = "i",
                            TOR_OTHER_CZ_NAME = "c",
                            BEGIN_RANGE = "i",
                            BEGIN_AZIMUTH = "c",
                            BEGIN_LOCATION = "c",
                            END_RANGE = "i",
                            END_AZIMUTH = "c",
                            END_LOCATION = "c",
                            BEGIN_LAT = "d",
                            BEGIN_LON = "d",
                            END_LAT = "d",
                            END_LON = "d",
                            EPISODE_NARRATIVE = "-",
                            EVENT_NARRATIVE = "-",
                            DATA_SOURCE = "c"
                          )
                      )
                    )
  }
  
}


library(stringr)

weather_event_tbl <- as.tbl(
  select(weather_event_df
         , id = EVENT_ID, st = STATE_FIPS, cz = CZ_FIPS 
         , type = EVENT_TYPE
         , begin = BEGIN_DATE_TIME
         , tz = CZ_TIMEZONE
         , INJURIES_DIRECT:DAMAGE_CROPS
         , fscale = TOR_F_SCALE
         , begin_lat = BEGIN_LAT
         , begin_lon = BEGIN_LON 
         , end_lat = END_LAT
         , end_lon = END_LON
  )) %>%
  mutate(type = tolower(type)) %>%
  mutate(type = gsub("heavy wind", "high wind", type)) %>%
  mutate(type = gsub("high snow", "heavy snow", type)) %>%
  mutate(type = gsub("^hurricane$", "hurricane (typhoon)", type)) %>%
  mutate(type = gsub("landslide", "avalanche", type)) %>%
  mutate(type = gsub("thunderstorm winds?.*", "thunderstorm wind", type)) %>%
  mutate(type = gsub("volcanic ashfall", "volcanic ash", type)) %>%
#  mutate(type = gsub("tornado/waterspout", "waterspout", type)) %>%
  mutate(type = str_to_title(type)) %>%
  mutate(fscale = as.factor(gsub("^E?F","F",fscale))) %>%
  dplyr::filter(type != "Northern Lights") %>%
  dplyr::filter(type != "Other") %>%
  mutate( type = as.factor(type)
          ,st = as.factor(sprintf("%02d",st))
          ,cz = as.factor(sprintf("%05d",cz))
          ,begin = dmy_hms(begin)
          ,tz = toupper(str_trunc(tz, 3, side = "r", ellipsis = ""))
  ) %>%
  arrange(begin) 

rm(weather_event_df)

