# Get all new WEA messages and move to 
# "CMAS_Alerts_Processed" sheet
# Suggested monthly run

library(googlesheets)
require(tidyverse)
require(lubridate)

##

NewCMASImport <- function() { #copies new messages into main sheet
        raw <- gs_key("1GnchiRm2TXgQ1TpTGcsCIGggIjEIsd6TeuVyY_s4a3U") #CMAS Alerts
        full <- gs_key("1Xw4JefUCS4HHQ0KpvKhr-DjklqzhH3_CeA-zhoAuQfI") #CMAS_Alerts_Processed
        msg <- gs_read(raw)
        msg <- tail(msg, 240)
        gs_add_row(ss = full, ws = 1, input = msg, verbose = TRUE) -> howmany 
        print(length(howmany))
    }    



