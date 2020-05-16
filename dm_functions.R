getQueryFileName <- function(id) {
  paste0("./sites/", id, "/queries.Rds")
}

runQuery <- function(siteID, queryID, data, text) {
  if (nrow(data) == 0) return()
  
  allQueries <- readQueries(siteID, session$userData$settings$testMode)
  openQueries <- allQueries %>% 
                       filter(
                         SiteID == siteID,
                         QueryID == queryID,
                         Status == "OPEN"
                       )
  # Create new queries
  queries <- data %>% 
               add_column(
                 QueryID=queryID,
                 FirstRaised=lubridate::today(),
                 Status="OPEN",
                 QueryText=ifelse(is.expression(text), enframe(eval(text)), text)
               ) %>% 
               select(SiteID, PatNo, QueryID, FirstRaised, QueryText, Status)
  # Remove the queries we already know about
  newQueries <- queries %>% 
                  anti_join(openQueries, by=c("SiteID", "PatNo", "QueryID"))
  # Identify queries which have been resolved...
  resolvedQueries <- openQueries %>% 
                       filter(Status == "OPEN") %>% 
                       anti_join(queries, by=c("SiteID", "PatNo", "QueryID"))
  # ...and update their Status
  if (nrow(resolvedQueries) > 0) {
    resolvedQueries <- resolvedQueries %>% 
                         mutate(Status="CLOSED") %>% 
                         select(SiteID, PatNo, QueryID, Status)
    allQueries <- allQueries %>% 
                         left_join(resolvedQueries, by=c("SiteID", "PatNo", "QueryID")) %>% 
                         mutate(Status=coalesce(Status.y, Status.x)) %>% 
                         select(-Status.x, -Status.y)
  }
  if (nrow(newQueries) == 0) return()
  # Add the new queries to the existing ones and save
  allQueries <- allQueries %>% bind_rows(newQueries)
  saveQueries(allQueries, siteID, session$userData$settings$testMode)
}

createQueryFile <- function(id) {
  queryFile <- getQueryFileName(id)
  if (file.exists(queryFile)) stop(paste0("Query file already exists for site ", id))
  t <- tibble(
    SiteID=character(),
    PatNo=character(),
    QueryID=numeric(),
    FirstRaised=structure(NA_real_, class = "Date"),
    QueryText=character(),
    SiteComment=character(),
    AdminComment=character(),
    Status=character()
  )
  saveRDS(t, queryFile)
}

# createQueryFile("001")
# createQueryFile("002")
# createQueryFile("003")
# createQueryFile("004")
# createQueryFile("005")
# createQueryFile("006")
# createQueryFile("007")
# createQueryFile("008")
# createQueryFile("009")

runDataQualityCheck <- function(id) {
  queryFile <- getQueryFileName(id)
  p <- readPatients(id, session$userData$settings$testMode)
  # Screening and demography
  runQuery(id,  1, p %>% filter(is.na(ICDate)), "Date of informed consent cannot be missing.")
  runQuery(id,  2, p %>% filter(ICDate > lubridate::today()), "Date of informed consent cannot be in the future.")
  runQuery(id,  3, p %>% filter(is.na(AdmissionDate)), "Date of hospital admission cannot be missing.")
  runQuery(id,  4, p %>% filter(AdmissionDate > lubridate::today()), "Date of hospital admission cannot be in the future.")
  runQuery(id,  5, p %>% filter(AdmissionDate > ICDate), "Date of hospital admission cannot be after the date of informed consent.")
  runQuery(id,  6, p %>% filter(Sex == ""), "Patient sex cannot be missing.")
  runQuery(id,  7, p %>% filter(Sex == "female" & !(Pregnancy %in% c("TRUE", "FALSE"))), "For female patients, pregnancy must be either 'Yes' or 'No'.")
  runQuery(id,  8, p %>% filter(Sex == "male" & Pregnancy != "NA"), "For male patients, pregnancy must be  'NA'.")
  runQuery(id,  9, p %>% filter(is.na(Age)), "Age cannot be missing.")
  runQuery(id, 11, p %>% filter(Ethnicity == ""), "Ethnicity cannot be missing.")
  runQuery(id, 12, p %>% filter(is.na(OnsetDate)), "Onset date cannot be missing.")
  runQuery(id, 13, p %>% filter(OnsetDate > AdmissionDate), "Onset date cannot be after the date of admission.")
  runQuery(id, 14, p %>% filter(PCRTest == ""), "PCR test result cannot be missing.")
  runQuery(id, 16, p %>% filter(Diarrhoea == ""), "Diarrhoea cannot be missing.")
  runQuery(id, 17, p %>% filter(Anosmia == ""), "Anosmia cannot be missing.")
  runQuery(id, 18, p %>% filter(Pneumonia == ""), "Pneumonia cannot be missing.")
  runQuery(id, 19, p %>% filter(Dyspnea == ""), "Dyspnea cannot be missing.")
  runQuery(id, 20, p %>% filter(Hypersensitivity == ""), "Hypersensitivity cannot be missing.")
  runQuery(id, 21, p %>% filter(MyastheniaGravis == ""), "Myasthenia gravis cannot be missing.")
  runQuery(id, 22, p %>% filter(Pregnancy == ""), "Pregnancy cannot be missing.")
  runQuery(id, 23, p %>% filter(Cirrhosis == ""), "Cirrhosis cannot be missing.")
  # Inclusion/exclusion violations
  runQuery(id, 10, p %>% filter(FinalStatus != "SCREEN_FAILURE" & (Age < 40 | Age > 90)), "If a patient's age is less than 40 or greater than 90, then final status must be 'Screen failure'.")
  runQuery(id, 15, p %>% filter(FinalStatus != "SCREEN_FAILURE" & PCRTest == FALSE), "If a patient's diagnosis is not confirmed by PCT test, then final status must be 'Screen failure'.")
  runQuery(id, 24, p %>% filter(FinalStatus != "SCREEN_FAILURE" & Dyspnea == FALSE), "If a patient does not have a new cough or dyspnea, then final status must be 'Screen failure'.")
  runQuery(id, 25, p %>% filter(FinalStatus != "SCREEN_FAILURE" & Hypersensitivity == TRUE), "If a patient has a known hypersensitivity to doxycycline, then final status must be 'Screen failure'.")
  runQuery(id, 26, p %>% filter(FinalStatus != "SCREEN_FAILURE" & MyastheniaGravis == TRUE), "If a patient has myasthenia gravis, then final status must be 'Screen failure'.")
  runQuery(id, 27, p %>% filter(FinalStatus != "SCREEN_FAILURE" & Pregnancy == "TRUE"), "If a patient is pregnant, then final status must be 'Screen failure'.")
  runQuery(id, 28, p %>% filter(FinalStatus != "SCREEN_FAILURE" & Cirrhosis == TRUE), "If a patient has cirrhosis of the liver, then final status must be 'Screen failure'.")

  # Missing rando and stratification info
  p <- p %>% filter(FinalStatus != "SCREEN_FAILURE")
  runQuery(id, 29, p %>% filter(StratumILD == ""), "ILD status cannot be missing.")
  runQuery(id, 30, p %>% filter(StratumCOPD == ""), "COPD status cannot be missing.")
  runQuery(id, 31, p %>% filter(StratumBronchiectasis == ""), "Bronchiectasis status cannot be missing.")
  runQuery(id, 32, p %>% filter(StratumAsthma == ""), "Asthma status cannot be missing.")
  runQuery(id, 33, p %>% filter(StratumOtherLung == ""), "Other lung disease status cannot be missing.")
  runQuery(id, 34, p %>% filter(StratumDiabetes == ""), "Diabetes status cannot be missing.")
  runQuery(id, 35, p %>% filter(StratumHeartDisease == ""), "Heart disease status cannot be missing.")
  runQuery(id, 36, p %>% filter(StratumHypertension == ""), "Hypertension status cannot be missing.")
  runQuery(id, 37, p %>% filter(StratumCancer == ""), "Cancer status cannot be missing.")
  runQuery(id, 38, p %>% filter(StratumOther == ""), "Other co-morbidity status cannot be missing.")
  # lubridate::interval performs an unnecessary cast...
  runQuery(id, 39, p %>% filter(FinalStatus == "" & Treatment == "" & (as.duration(lubridate::interval(as.Date.numeric(ICDate, origin="1970-01-01"), lubridate::today())) > ddays(10))), "Informed consent was taken 10 days ago and the patient has not yet been randomised.  Please clarify.")
  # Missing treatment records
  runQuery(id, 40, p %>% filter(TrtDoxycycline == ""), "Missing doxycycline treatment info.")
  runQuery(id, 41, p %>% filter(TrtAntibiotics == ""), "Missing antibiotic treatment info.")
  runQuery(id, 42, p %>% filter(TrtAntiHypertensives == ""), "Missing anti-hypertensive treatment info.")
  runQuery(id, 43, p %>% filter(TrtAntiDepressants == ""), "Missing anti-depressant treatment info.")
  runQuery(id, 44, p %>% filter(TrtAnalgesics == ""), "Missing analgesic treatment info.")
  runQuery(id, 45, p %>% filter(TrtDiabetes == ""), "Missing ddiabetic treatment info.")
  runQuery(id, 46, p %>% filter(TrtHeartFailure == ""), "Missing heart failure treatment info.")
  runQuery(id, 47, p %>% filter(TrtCancer == ""), "Missing anti-cancer treatment info.")
  runQuery(id, 48, p %>% filter(TrtImmunosuppressant == ""), "Missing immunosuppressant treatment info.")
  runQuery(id, 49, p %>% filter(TrtOther == ""), "Missing other treatment info.")
  runQuery(id, 50, p %>% filter(TrtDoxycycline == TRUE & Treatment == "SOC"), "Patient was randomised to SoC but was trreated with doxycycline.  Please clarify.")
  runQuery(id, 51, p %>% filter(TrtDoxycycline == FALSE & Treatment == "Active"), "Patient was randomised to doxycycline but did not receive any.  Please clarify.")
  # Outcome data
  p <- p %>% filter(Treatment != "")
  runQuery(id, 52, p %>% filter(ICUAdmissionNeeded & is.na(ICUAdmissionNeededDate)), "Inconsistent need for ICU admission and date of need")
  runQuery(id, 53, p %>% filter(!ICUAdmissionNeeded & !is.na(ICUAdmissionNeededDate)), "Inconsistent need for ICU admission and date of need")
  runQuery(id, 54, p %>% filter(is.na(ICUAdmissionNeededDate) & isTruthy(ICUAdmissionSuccess)), "Inconsistent need for ICU admission and admission status")
  runQuery(id, 55, p %>% filter(!is.na(ICUAdmissionNeededDate) & !isTruthy(ICUAdmissionSuccess)), "Inconsistent need for ICU admission and admission status")
  runQuery(id, 56, p %>% filter(!is.na(ICUAdmissionNeededDate) & ICUAdmissionNeededDate < AdmissionDate), "Patient was admitted to ICU before being admitted to hospital!")
  runQuery(id, 57, p %>% filter(ICUDischargeDate > HospitalDischargeDate), "Patient was discharged from ICU after being discharged from hospital!")
  runQuery(id, 58, p %>% filter(Alive & !is.na(DateOfDeath)), "Inconsistent overall survival and date of death")
  runQuery(id, 59, p %>% filter(!Alive & is.na(DateOfDeath)), "Inconsistent overall survival and date of death")
  runQuery(id, 60, p %>% filter(FinalStatus != "" & FinalStatus == ""), "Inconsistent final status and date of last follow-up")
  runQuery(id, 61, p %>% filter(FinalStatus == "" & FinalStatus != ""), "Inconsistent final status and date of last follow-up")
}

