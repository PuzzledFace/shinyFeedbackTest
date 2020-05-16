library(tidyverse)

routeList <- c("oral"="PO", 
               "rectal"="PR", 
               "intramuscular"="IM", 
               "intravenous"="IV", 
               "intradermal"="ID", 
               "intranasal"="IN", 
               "topical"="TP", 
               "sublingual"="SL", 
               "buccal"="BUCC", 
               "intraperitoneal"="IP")
saveRDS(routeList %>% enframe(), "./codeFiles/routes.Rds")

freqList <- c("daily"="daily",
              "every other day"="every other day",
              "twice a day"="BID",
              "twice a day"="TID",
              "four times a day"="QID",
              "at bed time"="QHS",
              "every four hours"="Q4H",
              "every four to six hours"="Q4-6H",
              "every week"="QWK",
              "as required"="PRN"
             )
saveRDS(freqList %>% enframe(), "./codeFiles/frequencies.Rds")


unitList <- c("milligrams"="mg",
              "millilitres"="mL",
              "capsules"="caps",
              "tablets"="tabs",
              "international units"="IU"
)
saveRDS(unitList %>% enframe(), "./codeFiles/units.Rds")
