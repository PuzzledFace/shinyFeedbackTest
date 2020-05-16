readSettings <- function(siteID) {
  fileName <- paste0("./sites/", siteID, "/settings.Rds")
  if (file.exists(fileName)) {
    s <- readRDS(fileName)
    z <- list()
    for (x in names(s)) z[[x]] <- s[[x]] 
  } else {
    z <- list(
      "testMode"=FALSE,
      "drugName"="Doxycycline"
    )
    saveSettings(siteID, z)
  }
  return(z)
}

saveSettings <- function(siteID, x) {
  fileName <- paste0("./sites/", siteID, "/settings.Rds")
  saveRDS(x, fileName)  
}

readRandoData <- function(id, stratum, testMode) {
  if (testMode) readRDS(paste0("./testData/sites/", id, "/rando_", stratum, ".Rds"))
  else readRDS(paste0("./sites/", id, "/rando_", stratum, ".Rds"))  
}

readQueries <- function(id, testMode) {
  if (testMode) readRDS(paste0("./testData/sites/", id, "/queries.Rds"))
  else readRDS(paste0("./sites/", id, "/queries.Rds"))  
}

saveQueries <- function(d, id, testMode) {
  if (testMode) saveRDS(d, paste0("./testData/sites/", id, "/queries.Rds"))
  else saveRDS(d, paste0("./sites/", id, "/queries.Rds"))  
}

readHospitalData <- function(id, testMode) {
  if (testMode) readRDS(paste0("./testData/sites/", id, "/hospitalData.Rds"))
  else readRDS(paste0("./sites/", id, "/hospitalData.Rds"))  
}

saveHospitalData <- function(h, id, testMode) {
  if (testMode) saveRDS(h, paste0("./testData/sites/", id, "/hospitalData.Rds"))
  else saveRDS(h, paste0("./sites/", id, "/hospitalData.Rds"))  
}

readPatients <- function(id, testMode) {
  req(testMode)
  if (testMode) readRDS(paste0("./testData/sites/", id, "/patients.Rds"))
  else readRDS(paste0("./sites/", id, "/patients.Rds"))
}

getPatientFileName <- function(id, testMode) {
  if (testMode) paste0("./testData/sites/", id, "/patients.Rds")
  else paste0("./sites/", id, "/patients.Rds")
}

savePatients <- function(p, id, testMode) {
  if (testMode) saveRDS(p, paste0("./testData/sites/", id, "/patients.Rds"))
  else saveRDS(p, paste0("./sites/", id, "/patients.Rds"))
}

readCredentials <- function(testMode) {
  if (testMode) rv <- readRDS("./testData/data/credentials.Rds")
  else rv <- readRDS("./data/credentials.Rds")
  # Somehow, Admin is getting converted to character.  This is a quick and dirty fix.  It doesn't address the root cause though.
  rv <- rv %>% mutate(Admin=as.logical(Admin))
  return(rv)
}

saveCredentials <- function(c, testMode) {
  if (testMode) saveRDS(c, "./testData/data/credentials.Rds")
  else saveRDS(c, "./data/credentials.Rds")
}


# Confirm if a patient, represented as a row from a patients tibble is eleigibel to enter the study
checkEligibility <- function(p) {
  if (is.null(p)) return (c("Current patient is NULL. [This shouldn't happen.]"))
  if (length(p) == 0) return (c("Current patient has length zero. [This shouldn't happen.]"))
  reasons <- c()
  if (!isTruthy(p$ICDate)) reasons <- append(reasons, "No date of informed consent.")
  else if (p$ICDate > lubridate::today()) reasons <- append(reasons, "Date of informed consent is in the future.")
  if (!isTruthy(p$PCRTest)) reasons <- append(reasons, "Diagnosis of COVID-19 not confirmed by PCR.")
  if (!isTruthy(p$Sex)) reasons <- append(reasons, "Patient's sex has not been provided.")
  else if (p$Sex == "female" & p$Pregnancy != FALSE) reasons <- append(reasons, "No pregnancy test for a female patient.")
  if (!isTruthy(p$Age)) reasons <- append(reasons, "Patient's age has not been provided.")
  else if (p$Age <40 | p$Age > 90) reasons <- append(reasons, "Patient is not between 40 and 90 years old.")
  if (!isTruthy(p$Temperature)) reasons <- append(reasons, "Patient's temperature has not been provided.")
  else if (p$Temperature < 37.6) reasons <- append(reasons, "Patient's temperature is not above 37.6 Celsius.")
  if (!isTruthy(p$OnsetDate)) reasosn <- append(reasons, "Date of onset has not been provided.")
  if (!isTruthy(p$AdmissionDate)) reasosn <- append(reasons, "Date of hospital admission has not been provided.")
  if (isTruthy(p$OnsetDate) & isTruthy(p$AdmissionDate)) {
    if (as.duration(interval(p$OnsetDate, p$AdmissionDate)) > ddays(10)) reasons <- append(reasons, paste0("Date of onset [", p$OnsetDate, "] is not less than ten days before date of hospital admission [", p$AdmissionDate, "]."))
  }
  if (p$Hypersensitivity == "") reasons <- append(reasons, "Absence of hypersensitivity to doxycycline has not been confirmed.")
  else if (p$Hypersensitivity == TRUE) reasons <- append(reasons, "Known hypersensitivity to docycycline is an exclusion criterion.")
  if (p$Dyspnea == "") reasons <- append(reasons, "Presence or absence of cough/dyspnea has not been confirmed.")
  else if (p$Dyspnea == FALSE) reasons <- append(reasons, "Absence of cough/dyspnea is an exclusion criterion.")
  if (p$MyastheniaGravis == "") reasons <- append(reasons, "Presence or absence of myasthenia gravis has not been confirmed.")
  else if (p$MyastheniaGravis == TRUE) reasons <- append(reasons, "Presence of myasthenia gravis is an exclusion criterion.")
  if (length(reasons) == 0) return(TRUE)
  else return(reasons)
}

# Translate a column name to the name of the corresponding input widget using a standard convention 
# with some exceptions
fieldToWidget <- function(x) {
  if (is.atomic(x) && length(x) == 1L) {
    if (substr(x, 1, 3) == "ICU") return(paste0("icu", substr(x, 4, nchar(x))))
    if (substr(x, 1, 3) == "PCR") return(paste0("pcr", substr(x, 4, nchar(x))))
    if (x == "ICDate") return("icDate")
    return(paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x))))
  } else {
    return(sapply(x, function(y) fieldToWidget(y)))
  }
}

# Translate an input widget name to the name of the corresponding data column using a standard convention 
# with some exceptions
widgetToField <- function(x) {
  if (is.atomic(x) && length(x) == 1L) {
    if (substr(x, 1, 3) == "icu") return(paste0("ICU", substr(x, 4, nchar(x))))
    if (substr(x, 1, 3) == "pcr") return(paste0("PCR", substr(x, 4, nchar(x))))
    if (x == "icDate") return("ICDate")
    return(paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
  } else {
    return(sapply(x, function(y) widgetToField(y)))
  }
}

#Source all files in a given folder
sourceFolder <- function(folder) {
  sapply(
    list.files(folder, full.names=TRUE),
    function(f) {
      print(paste0("Loading ", f, "..."))
      source(f)
    }
  )  
}

#Helper functions for titledWellPanel class
titledWellPanel <- function(title, ...) {
  shiny::wellPanel(div(class="h1", title), ...)
}

useTitledWellPanel <- function() {
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "titledWellPanel.css")
  )
}

#Wrapper function to prevent problems with undefined icons on shinyapps.io
feedbackDanger <- function(...) {
  shinyFeedback::feedbackDanger(icon=NULL, ...)
}

#Wrapper function to prevent problems with undefined icons on shinyapps.io
feedbackWarning <- function(...) {
  shinyFeedback::feedbackWarning(icon=NULL, ...)
}

updatePatient <- function(session) {
  # Unfortunately, the mutate implemenation below appears to save all values as character.  With a zero row destination tibble
  # (ie the first patient at the site), this appears to change the column type to character. Grrrrrr!!!!!!
  p <- session$userData$currentPatient
  if (nrow(p) == 0) {
    displayMessage("Error!", HTML("No patient selected. (Maybe because you haven't registered a patient yet?)<br><br>Please register a patient first."))
    return()
  }
  # Read the existing data for all patients, note the correct column types
  t <- readPatients(p$SiteID, session$userData$settings$testMode)
  columnTypes <- t %>% dplyr::summarise_all(class) %>% tidyr::gather(variable, class)
  # Get field names, ignoring PatNo and SiteID
  fieldNames <- names(p)
  fieldNames <- fieldNames[!(fieldNames %in% c("PatNo", "SiteID"))]
  
  asTypedValue <- function(value, type) {
    if (is.na(value)) return(NA)
    switch(
      type,
      "character"=as.character(value),
      "Date"=lubridate::as_date(value),
      "numeric"=as.numeric(value),
      "logical"=as.logical(value),
      stop(paste0("Don't know how to handle ", type, "s!"))
    )
  }
  
  # Update the current patient's data
  for (f in fieldNames) {
    colType <- columnTypes %>% filter(variable == f) %>% select(class) %>% deframe()
    if (!is.na(p[[f]])) {
      t <- t %>% 
        mutate(
          !!f :=  ifelse(
            PatNo == p$PatNo,
            asTypedValue(p[[f]], colType), 
            asTypedValue(!!f, colType)
          )
        )
    }
  }
  savePatients(t, p$SiteID, session$userData$settings$testMode)
  displayMessage("Information", "Changes saved.")
}

createPatient <- function(siteID, saveData=TRUE, testMode) {
  fileName <- getPatientFileName(siteID, testMode)
  if (file.exists(fileName)) {
    existingPatients <-   readPatients(siteID, testMode)
    if (nrow(existingPatients) == 0) newPatNo <- "0001"
    else newPatNo <- sprintf("%04i", max(as.numeric(existingPatients$PatNo)) + 1)
    
    newPatient <- existingPatients %>% 
      filter(FALSE) %>% 
      add_row(PatNo=newPatNo,
              SiteID=siteID)
    allPatients <- existingPatients %>% 
      bind_rows(newPatient)
  } else {
    newPatient <- tibble(
      PatNo="001",
      SiteID=siteID,
      Treatment=character(),
      ICDate=structure(NA_real_, class = "Date"),
      Sex=character(),
      Age=numeric(),
      Ethnicity=character(),
      Temperature=numeric(),
      OnsetDate=structure(NA_real_, class = "Date"),
      AdmissionDate=structure(NA_real_, class = "Date"),
      PCRTest=logical(),
      Hypersensitivity=logical(),
      MyastheniaGravis=logical(),
      Pregnancy=logical(),
      Cirrhosis=character(),
      Dyspnea=character(),
      Diarrhoea=character(),
      Anosmia=character(),
      Pneumonia=character(),
      TrtDoxycycline=logical(),
      TrtAntibiotics=logical(),
      TrtAnalgesics=logical(),
      TrtDiabetes=logical(),
      TrtHeartFailure=logical(),
      TrtAntiHypertensives=logical(),
      TrtAntiDepressants=logical(),
      TrtCancer=logical(),
      TrtImmunosuppressant=logical(),
      TrtOther=logical(),
      StratumILD=logical(),
      StratumCOPD=logical(),
      StratumBronchiectasis=logical(),
      StratumAsthma=logical(),
      StratumOtherLung=logical(),
      StratumDiabetes=logical(),
      StratumHeartDisease=logical(),
      StratumHypertension=logical(),
      StratumCancer=logical(),
      StratumOther=logical(),
      Stratum=character(),
      ICUAdmissionNeeded=logical(),
      ICUAdmissionSuccess=logical(),
      ICUAdmissionNeededDate=structure(NA_real_, class = "Date"),
      ICUNonAdmissionReason=character(),
      ICUDischargeDate=structure(NA_real_, class = "Date"),
      HospitalDischargeDate=structure(NA_real_, class = "Date"),
      Alive=logical(),
      DateOfDeath=numeric(),
      RelatedDeath=logical(),
      FollowUpDate=numeric(),
      FinalStatus=character()
    )
    allPatients <- newPatient
  } 
  saveRDS(allPatients, fileName)
  return(newPatient)
}

# createPatient("001")
# readRDS("./sites/001/patients.Rds")
# createPatient("002")
# createPatient("003")
# createPatient("004")

displayMessage <- function(title="Attention!", text) {
  showModal(
    modalDialog(
      title=title,
      text,
      footer = modalButton("OK")
    )
  )
}

validatePregnancy <- function(sex, preg) {
  msg <- c()
  if (sex == "male") {
    if (preg != "NA") msg <- append(msg, "Pregnancy status must be NA for males.")
  } else if (sex == "female") {
    if (preg == "NA") msg <- append(msg, "Pregnancy status must not be NA for females.")
  }
  return(msg)
}

createHospitalData <- function(id) {
  t <- tibble(
         SiteID=character(),
         PatNo=character(),
         HospitalID=character()
       )
  saveRDS(t, paste0("./sites/", id, "/hospitalData.Rds"))
}

# createHospitalData("001")
# createHospitalData("002")
# createHospitalData("003")
# createHospitalData("004")
# createHospitalData("005")
# createHospitalData("006")
# createHospitalData("007")
# createHospitalData("008")
# createHospitalData("009")

createNewSite <- function(id, updateCredentials=TRUE, testMode) {
  sitePath <- paste0("./sites/", id)
  if (dir.exists(sitePath)) stop(paste0("Unable to create new site directory: ", sitePath, " already exists"))
  dir.create(sitePath)
  dir.create(paste0(sitePath, "/dataQuality"))
  randomise(id, "NONE")
  randomise(id, "LUNG")
  randomise(id, "OTHER")
  createHospitalData(id)
  createQueryFile(id)
  if (updateCredentials) {
    credentials <<- credentials %>%   # Global assignment
      add_row(
        SiteID=id,
        EmailAddress=NA,
        Admin=FALSE
      )
    saveCredentials()
  }
  createPatient(id, saveData=FALSE, testMode=testMode)
}

# createNewSite("001", FALSE)
# createNewSite("002", FALSE)
# createNewSite("003", FALSE)
# createNewSite("004", FALSE)
# createNewSite("005", FALSE)
# createNewSite("006", FALSE)
# createNewSite("007", FALSE)
# createNewSite("008", FALSE)
# createNewSite("009", FALSE)

isValidEmailAddress <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

randomise <- function(site,
                      stratum, 
                      n=24, 
                      b=6) {
  #  Randomise
  t <- tibble(
    SiteID=site,
    Stratum=stratum,
    Allocated=FALSE,
    Treatment=rep(c("Active", "SOC"), times=n/2),
    Block=rep(1:(n/b), each=b),
    Temp=runif(n)
  ) %>%
    arrange(Block, Temp) %>% 
    select(-Block, -Temp)
  #Append to existing randomisation, if necessary
  fileName <- getRandoFileName(site, stratum)
  if (file.exists(fileName)) {
    t1 <- readRDS(fileName)
    startPack <- max(t1$PackNo) + 1
    t <- t %>% add_column(PackNo=startPack:(startPack+n-1))
    t <- bind_rows(t1, t)
  } else {
    t <- t %>% add_column(PackNo=1:n)
  }
  #Save
  saveRDS(t, fileName)
}

getRandoFileName <- function(site, stratum) {
  paste0("./sites/", site, "/rando_", stratum, ".Rds")
}