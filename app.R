library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(shinyFeedback)
library(DT)
library(shinyjs)
library(shinyhelper)
library(knitr)

source("functions.R", echo=FALSE)
source("siteUI.R", echo=FALSE)
source("dm_functions.R", echo=FALSE)
#Source all files in the modules folder
sourceFolder("./modules")

versionString <- "v0.2.03 (beta)"

freqs <- readRDS("./codeFiles/frequencies.Rds")
routes <- readRDS("./codeFiles/routes.Rds")
units <- readRDS("./codeFiles/units.Rds")

# Define UI
ui <- dashboardPage(
  title=paste0("ICU doxycycline ", versionString),
  dashboardHeader(
    title="ICU doxycycline",
    dropdownMenuOutput("messageMenu"),
    dropdownMenuOutput("notificationMenu"),
    dropdownMenuOutput("taskMenu")
  ),
  dashboardSidebar(
    sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    useTitledWellPanel(),
    wellPanel(
      htmlOutput("userDetails")
    ),
    tabItems(
      tabItem(
        tabName="dataEntry",
        fluidRow(
          fluidRow(
            box(
              width=12,
              column(
                width=5,
                htmlOutput("currentPatient"),
              ),
              column(
                width=3,
                actionButton(
                  "register",
                  "Register new patient"
                )
              ),
              column(
                width=4,
                selectInput(
                  "patID",
                  "Select an existing patient to amend",
                  choices=c()
                )
              ),
              box(
                status="info",
                title="Hospital ID",
                width=12,
                fluidRow(
                  column(
                     width=10,
                     textInput("hospitalID", "Please enter") %>% 
                       helper(
                         type="inline",
                         title="Hospital ID",
                         content="Please add whatever information you need to link the patient's trial ID to their hospital records.  This information is not available to the study administrators."
                       )
                  ),
                  column(
                    width=2,
                    actionButton("updateHospitalID", "Update")
                  )
                )
              )
            ),
            tags$style(type='text/css', "#register { width:100%; margin-top: 25px;}"), #Align the Register button
            tags$style(type='text/css', "#updateHospitalID { width:100%; margin-top: 25px;}") #Align the Update button
          ),
          tabBox(
            title="Patient data",
            width=12,
            tabPanel(
              title="Screening",
              fluidRow(
                box(
                  width=4,
                  solidHeader=TRUE,
                  status="primary",
                  collapsible=TRUE,
                  title="Inclusion criteria",
                  validatedDateInputUI("icDate",  label="Date of informed consent"),
                  validatedSelectInputUI(
                    "sex", 
                    label="Patient sex", 
                    choiceList=c("- Enter Male or Female -"="", "Male"="male", "Female"="female")
                  ),
                  numericInput(
                    "age",
                    "Age in years",
                    min=1,
                    max=120,
                    step=1,
                    value=40
                  ),
                  numericInput(
                    "temperature",
                    "Body temperature [Celcius]",
                    min=30,
                    max=45,
                    step=0.1,
                    value=37
                  ),
                  # Categories from Mathur et al (2013) Availability and use of UK based ethnicity data for health research
                  # Table 4, based on UK census categories as harmonised by Simpson and Akinwale (2006)
                  validatedSelectInputUI(
                    "ethnicity",
                     label="Ethnicity:", 
                     choiceList=c(
                       "- Select one -"="", 
                       "White"="W", 
                       "Black Caribbean"="BC", 
                       "Black African"="BA", 
                       "Indian"="I", 
                       "Pakistani"="P", 
                       "Bangladeshi"="B", 
                       "Chinese"="C", 
                       "Other"="O"
                    )
                  ),
                  validatedDateInputUI("onsetDate", label="Date symptoms started"),
                  validatedDateInputUI("admissionDate", label="Date of admission to hospital"),
                  validatedSelectInputUI("pcrTest", label="SARS-CoV-2 infection confirmed by PCR?")
                ),
                box(
                  width=4,
                  solidHeader=TRUE,
                  status="primary",
                  collapsible=TRUE,
                  title= "Clinical Presentation",
                  validatedSelectInputUI("diarrhoea", label="Is the patient suffering from diarrhoea?"),
                  validatedSelectInputUI("anosmia", label="Has the patient recently lost their sense of smell?"),
                  validatedSelectInputUI(
                    "pneumonia", 
                    label="Is there evidence of pneumonia on the patient's x-ray?", 
                    choiceList=c("- Select one -"="", "Yes"=TRUE, "No"=FALSE, "X-ray not done"="NA")
                  )
                ),
                box(
                  width=4,
                  solidHeader=TRUE,
                  status="primary",
                  collapsible=TRUE,
                  title= "Exclusion criteria",
                  validatedSelectInputUI("dyspnea", label="Does the patient have a new, continuous cough or dyspnea?"),
                  validatedSelectInputUI("hypersensitivity", label="Known hypersensitivity to doxycycline?"),
                  validatedSelectInputUI("myastheniaGravis", label="Myasthenia gravis?"),
                  validatedSelectInputUI("pregnancy", label="Is the patient pregnant?", choiceList=c("- Enter Yes, No, or NA -"="", "Yes"=TRUE, "No"=FALSE, "NA"="NA")),
                  validatedSelectInputUI("cirrhosis", label="Known liver cirrhosis?")
                ),
                saveButtonUI("saveScreening")
              )   # Fluid row
            ),  # Tab panel [Screening]
            tabPanel(
              title="Randomisation",
              fluidRow(
                infoBox(
                  color="red",
                  fill=TRUE,
                  width=12,
                  title="The study has not yet received ethical approval.",
                  value="The features on this page should be used only for testing purposes.  They MUST NOT be used to determine the treatment of actual patients."
                ),
                box(
                  width=12,
                  status="primary",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  title="Stratification & randomisation",
                  column(
                    width=4,
                    titledWellPanel(
                      title="Respiratory conditions",
                      validatedSelectInputUI("stratumILD", label="ILD?"),
                      validatedSelectInputUI("stratumCOPD", label="COPD?"),
                      validatedSelectInputUI("stratumBronchiectasis", label="Bronchiectasis?"),
                      validatedSelectInputUI("stratumAsthma", label="Asthma?"),
                      validatedSelectInputUI("stratumOtherLung", label="Other respiratory disease?")
                    )
                  ),
                  column(
                    width=4,
                    titledWellPanel(
                      title="Other co-morbidities",
                      validatedSelectInputUI("stratumDiabetes", label="Diabetes?"),
                      validatedSelectInputUI("stratumHeartDisease", label="Heart disease?"),
                      validatedSelectInputUI("stratumHypertension", label="Hypertension?"),
                      validatedSelectInputUI("stratumCancer", label="Cancer?"),
                      validatedSelectInputUI("stratumOther", label="Other non-respiratory disease?")
                    )
                  ),
                  column(
                    width=3,
                    uiOutput("allocation")
                  ),
                  box(
                    status="primary",
                    solidHeader=TRUE,
                    collapsible=TRUE,
                    title="Stratum",
                    width=6,
                    textOutput("derivedStratumText")
                  ),
                  box(
                    status="success",
                    solidHeader=FALSE,
                    title="Action",
                    width=6,
                    actionButton("randomise", "Randomise")
                  )
                )
              )
            ),
            tabPanel(
              "Treatment",
              fluidRow(
                fluidRow(
                  box(
                    "Please indicate which classes of drug were administered during the trial",
                    column(
                      width=6,
                      validatedSelectInputUI("trtDoxycycline", label="Doxycycline?"),
                      validatedSelectInputUI("trtAntibiotics", label="Antibiotics (other than doxycycline)?"),
                      validatedSelectInputUI("trtAnalgesics", label="Analgesics?"),
                      validatedSelectInputUI("trtDiabetes", label="Diabetes treatment?"),
                      validatedSelectInputUI("trtHeartFailure", label="Treatment for heart failure?")
                    ),
                    column(width=6,
                      validatedSelectInputUI("trtAntiHypertensives", label="Antihypertensives?"),
                      validatedSelectInputUI("trtAntiDepressants", label="Antidepressants?"),
                      validatedSelectInputUI("trtCancer", label="Anti-cancer treatment?"),
                      validatedSelectInputUI("trtImmunosuppressant", label="Immunosuppresants?"),
                      validatedSelectInputUI("trtOther", label="Other relevant medication?")
                    )
                  )
                ),
                saveButtonUI("saveTreatment")
              )
            ),  # tabPanel [Treatment]
            tabPanel(
              "Outcome",
              fluidRow(
                box(
                  width=9,
                  status="primary",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  title="ICU admission",
                  fluidRow(
                    column(
                      width=3,
                      validatedSelectInputUI("icuAdmissionNeeded", label="Did the patient need to be admitted to ICU?")
                    )
                  ),
                  fluidRow(
                    conditionalPanel(
                      condition="input['icuAdmissionNeeded-select'] == 'TRUE'",
                      column(
                        width=4,
                        validatedDateInputUI("icuAdmissionNeededDate", label="When did the need first arise?:")
                      ),
                      column(
                        width=4,
                        validatedSelectInputUI("icuAdmissionSuccess", label="Was the patient actually admitted to ICU?")
                      ),
                      column(
                        width=4,
                        conditionalPanel(
                          condition="input['icuAdmissionSuccess-select'] == 'FALSE'",
                          validatedSelectInputUI(
                            "icuNonAdmissionReason",
                            label="Why didn't you admit the patient to ICU?",
                            choiceList=c("- Select one -"="", "Lack of beds"="BEDS",  "Patient unsuitable"="UNSUITABLE", "Patient declined"="DECLINED", "Other"="OTHER"))
                        ),
                        conditionalPanel(
                          condition="input['icuAdmissionSuccess-select'] == 'TRUE'",
                          validatedDateInputUI("icuDischargeDate", label="When did the patient leave ICU?")
                        )
                      )
                    )
                  )
                ), 
                box(
                  width=3,
                  status="primary",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  title="Hospital discharge",
                  validatedDateInputUI("hospitalDischargeDate", label="Discharge date")
                ),
                box(
                  width=4,
                  status="primary",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  title="Survival",
                  validatedSelectInputUI("alive", label="Is the patient still alive?"),
                  conditionalPanel(
                    condition="input['alive-select'] == 'FALSE'",
                    validatedDateInputUI("dateOfDeath", label="Date of death:"),
                    validatedSelectInputUI("relatedDeath", label="Related to COVID-19?")
                  )
                ),
                box(
                  width=4,
                  status="primary",
                  solidHeader=TRUE,
                  collapsible=TRUE,
                  title="Final status",
                  validatedDateInputUI("followUpDate", label="Follow up date (date of last contact):"),
                  validatedSelectInputUI(
                    "finalStatus",
                    label="Final status",
                    choiceList=c(
                      "- Select one -"="", 
                      "Completed study"="COMPLETE",
                      "Discontinued due to treatment-related AE"="DTRAE",
                      "Discontinued due to AE not related to treatment"="DAE",
                      "Discontinued for other reasons"="OTHER",
                      "Lost to follow-up"="LTFU",
                      "Consent withdrawn"="CONSENT",
                      "Other"="OTHER"
                    )
                  )
                ), # box
                saveButtonUI("saveResponse", boxWidth=4)
              )
            ) #tabPanel [Outcome]
          ) #tabBox [Patient data]
        ) #fluidRow
      ), #tabItem [Data entry]
      tabItem(
        tabName="dataQuality",
        box(
          title="Queries",
          status="primary",
          width=12,
          queryEditorUI("siteQueries")
        )
      ), #tabItem [dataQuality]
      # siteUI("site"),
      tabItem(
        tabName="siteDetails",
        box(
          status="primary",
          solidHeader=TRUE,
          collapsible=TRUE,
          title="Site details",
          siteEditorUI("siteDetails")
        ),
        box(
          status="info",
          solidHeader=TRUE,
          collapsible=TRUE,
          title="Randomisation",
          wellPanel(
            fluidRow(
              tableOutput("randoSummary")
            ),
            fluidRow(
              actionButton("extendRando", "Extend randomisation")
            )
          )
        ),
        box(
          status="info",
          solidHeader=TRUE,
          collapsible=TRUE,
          title="Data download",
          wellPanel(
            selectInput("siteDownloadType", label="File type", choices=c("R data file"="Rds", "CSV"="csv")),
            fluidRow(
              downloadButton("siteDownloadPatients", "Download patient data"),
              downloadButton("downloadHospitalData", "Download hospital data")
            )
          )
        )
      ),
      tabItem(
        tabName="trialAdmin",
        fluidRow(
          box(
            status="primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            title="Site summary",
            tableOutput("siteSummary")
          ),
          box(
            status="success",
            solidHeader=TRUE,
            collapsible=TRUE,
            collapsed=TRUE,
            title="Site registration",
            siteEditorUI("siteRegistration", adminMode=TRUE)
          ),
          box(
            status="primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            collapsed=TRUE,
            width=12,
            title="Data quality",
            queryEditorUI("trialQueries")
          ),
          box(
            status="info",
            solidHeader=TRUE,
            collapsible=TRUE,
            collapsed=TRUE,
            width=12,
            title="Analysis"
          )
        )
      ),
      tabItem(
        tabName="settings",
        box(
          width=4,
          status="info",
          solidHeader=TRUE,
          collapsible=TRUE,
          collapsed=FALSE,
          title="Test mode",
          selectInput("settingsTestMode", "Test mode:", c("Yes"=TRUE, "No"=FALSE), selected=FALSE)
        ),
        box(
          width=4,
          status="info",
          solidHeader=TRUE,
          collapsible=TRUE,
          collapsed=FALSE,
          title="Drug name",
          textInput("settingsDrugName", "Drug name:")
        )
      ),
      tabItem(
        tabName="changeLog",
        box(
          width=12,
          fluidRow(
            includeMarkdown("changeLog.Rmd")
          )
        )
      ) #tabItem [changeLog]
    ) #tabItems
  ) #dashboardBody
) #dashboardpage

# Define server logic
server <- function(input, output, session) {
  
  #Initialise
  shinyhelper::observe_helpers()
  session$userData$patientID <- NA
  session$userData$notifications <- list()
  session$userData$currentPatient <- NA

  observe({
    #Handle user authentication
        #Development environment.  Synthesise user data
        if (!isTruthy(session$userData$synthised)) {
          print("Development user...")
          session$userData$user <- list(SiteID="001", Admin=TRUE, EmailAddress="john@puzzledface.net", Title="Mr", GivenName="John", FamilyName="Kirkpatrick", UserID=1)
          #session$userData$user <- list(SiteID="004", Admin=FALSE, EmailAddress="doxyicu.user@gmail.com", Title="Mx", GivenName="DoxyICU", FamilyName="User", UserID=4)
          session$userData$settings <- readSettings(session$userData$user$SiteID)
          repopulatePatientSelectInput()
          session$userData$synthised <- TRUE
        }
      session$userData$settings <- readSettings(session$userData$user$SiteID)

    # Settings
    updateSelectInput(session, "settingsTestMode", selected=session$userData$settings$testMode)
    updateTextInput(session, "settingsDrugName", value=session$userData$settings$drugName)
    session$userData$currentAdminSite <- session$userData$user
  })
  
  
  callModule(saveButtonController, "saveScreening", saveFunc=updatePatient)
  callModule(saveButtonController, "saveResponse", saveFunc=updatePatient)
  callModule(saveButtonController, "saveTreatment", saveFunc=updatePatient)
  
  # Demographics
  ethnicity <- callModule(validatedSelectInputController, id="ethnicity")
  # Presentation
  anosmia <- callModule(validatedSelectInputController, id="anosmia")
  dyspnea <- callModule(validatedSelectInputController, id="dyspnea")
  diarrhoea <- callModule(validatedSelectInputController, id="diarrhoea")
  pneumonia <- callModule(validatedSelectInputController, id="pneumonia")
  # Exclusion criteria
  lbl <- paste0("Known hypersensitivity to ", session$userData$settings$drugName, " is an exclusion criterion")
  hypersensitivity <- callModule(validatedSelectInputController, id="hypersensitivity", warnings=list(lbl=TRUE))
  cirrhosis <- callModule(validatedSelectInputController, id="cirrhosis", warnings=list("Known cirrhosis of the liver is an exclusion criterion"=TRUE))
  pregnancy<- callModule(validatedSelectInputController, id="pregnancy", warnings=list("Pregnancy is an exclusion criterion"=TRUE))
  myastheniaGravis <- callModule(validatedSelectInputController, id="myastheniaGravis", warnings=list("Known myasthenia gravis is an exclusion criterion"=TRUE))
  # Strata
  stratumILD <- callModule(validatedSelectInputController, id="stratumILD")
  stratumCOPD <- callModule(validatedSelectInputController, id="stratumCOPD")
  stratumBronchiectasis <- callModule(validatedSelectInputController, id="stratumBronchiectasis")
  stratumAsthma <- callModule(validatedSelectInputController, id="stratumAsthma")
  stratumOtherLung <- callModule(validatedSelectInputController, id="stratumOtherLung")
  stratumDiabetes <- callModule(validatedSelectInputController, id="stratumDiabetes")
  stratumHeartDisease <- callModule(validatedSelectInputController, id="stratumHeartDisease")
  stratumHypertension <- callModule(validatedSelectInputController, id="stratumHypertension")
  stratumCancer <- callModule(validatedSelectInputController, id="stratumCancer")
  stratumOther <- callModule(validatedSelectInputController, id="stratumOther")
  # Study treatments
  trtDoxycycline <- callModule(validatedSelectInputController, id="trtDoxycycline")
  trtAntibiotics <- callModule(validatedSelectInputController, id="trtAntibiotics")
  trtAnalgesics <- callModule(validatedSelectInputController, id="trtAnalgesics")
  trtDiabetes <- callModule(validatedSelectInputController, id="trtDiabetes")
  trtHeartFailure <- callModule(validatedSelectInputController, id="trtHeartFailure")
  trtAntihypertensives <- callModule(validatedSelectInputController, id="trtAntiHypertensives")
  trtAntiDepressants <- callModule(validatedSelectInputController, id="trtAntiDepressants")
  trtCancer <- callModule(validatedSelectInputController, id="trtCancer")
  trtImmunosuppressant <- callModule(validatedSelectInputController, id="trtImmunosuppressant")
  trtOther <- callModule(validatedSelectInputController, id="trtOther")
  # Data
  alive <- callModule(validatedSelectInputController, id="alive")
  sex <- callModule(validatedSelectInputController, id="sex")
  pcrTest <- callModule(validatedSelectInputController, id="pcrTest", warnings=list("A negative PCR test is an exclusion criterion."=FALSE))
  icDate <- callModule(validatedDateInputController, id="icDate")
  onsetDate <- callModule(validatedDateInputController, id="onsetDate")
  admissionDate <- callModule(validatedDateInputController, id="admissionDate")
  icuAdmissionNeeded <- callModule(validatedSelectInputController, id="icuAdmissionNeeded")
  icuAdmissionSuccess <- callModule(validatedSelectInputController, id="icuAdmissionSuccess")
  icuAdmissionNeededDate <- callModule(validatedDateInputController, id="icuAdmissionNeededDate")
  icuNonAdmissionReason <- callModule(validatedSelectInputController, id="icuNonAdmissionReason")
  icuDischargeDate <- callModule(validatedDateInputController, id="icuDischargeDate")
  dateOfDeath <- callModule(validatedDateInputController, id="dateOfDeath")
  relatedDeath <- callModule(validatedSelectInputController, id="relatedDeath")
  finalStatus <- callModule(validatedSelectInputController, id="finalStatus")
  followUpDate <-callModule(validatedDateInputController, id="followUpDate")
  hospitalDischargeDate <-  callModule(validatedDateInputController, id="hospitalDischargeDate")
  
  callModule(queryEditorController, "siteQueries")
  callModule(siteEditorController, id="siteDetails")
  
  # Trial admin
  callModule(queryEditorController, "trialQueries", editableColumns=c("AdminComment", "Status"))
  callModule(siteEditorController, "siteRegistration")
  
  # Sidebar menu
  output$sidebar <- renderMenu({
    req (session$userData$user)
    if (session$userData$user$Admin) {
      sidebarMenu(
        menuItem("Data entry", tabName="dataEntry", icon=NULL, selected=TRUE),
        menuItem("Data quality", tabName="dataQuality", icon=NULL),
        menuItem("Site details", tabName="siteDetails", icon=NULL),
        menuItem("Trial admin", tabName="trialAdmin", icon=NULL),
        menuItem("Settings", tabName="settings", icon=NULL),
        menuItem("Change log", tabName="changeLog", icon=NULL)
      ) 
      
    } else {
      sidebarMenu(
        menuItem("Data entry", tabName="dataEntry", icon=NULL, selected=TRUE),
        menuItem("Data quality", tabName="dataQuality", icon=NULL),
        menuItem("Site details", tabName="siteDetails", icon=NULL),
        menuItem("Change log", tabName="changeLog", icon=NULL)
      ) 
    }
  })
  
  # Settings management
  observeEvent(input$settingsTestMode, {
    session$userData$settings$testMode <- input$settingsTestMode
    saveSettings(session$userData$user$SiteID, session$userData$settings)
  })
  
  observeEvent(input$settingsDrugName, {
    if (nchar(input$settingsDrugName) > 0) {
      session$userData$settings$drugName <- input$settings$drugName
    }
    saveSettings(session$userData$user$SiteID, session$userData$settings)
  })
  
  observe({
    credentials <- readCredentials(session$userData$settings$testMode)
  })
  
  # Reactive UI
  allPatients <- reactive({
    bind_rows(lapply(list.dirs("./sites", full.names=FALSE, recursive=FALSE), function(x) readPatients(x, session$userData$settings$testMode)))
  })
  
  allQueries <- reactive({
    queries <- bind_rows(lapply(list.dirs("./sites", full.names=FALSE, recursive=FALSE), function(x) readQueries(x, session$userData$settings$testMode)))
  })
  
  output$siteSummary <- renderTable({
     queries <- allQueries() %>% select(SiteID, PatNo) %>% unique() %>% add_column(Clean=FALSE)
     pats <- allPatients() %>% 
               mutate(
                 Randomised=!is.na(Treatment),
                 Completed=FinalStatus != "",
                 ScreenFailure=FinalStatus == "SCREENFAIL",
                 Ongoing=FinalStatus == "",
                 Evaluable=!is.na(ICUAdmissionNeeded)
               ) %>% 
               select(SiteID, PatNo, Randomised, ScreenFailure, Ongoing, Completed, Evaluable)
     t <- pats %>% 
            left_join(queries %>% head(1), by=c("SiteID", "PatNo")) %>% 
            replace_na(list(Clean=TRUE)) %>% 
            group_by(SiteID, Clean) %>% 
            select(-PatNo) %>% 
            summarise(Randomised=sum(Randomised),
                      Completed=sum(Completed),
                      Ongoing=sum(Ongoing),
                      Evaluable=sum(Evaluable)
            ) %>% 
            ungroup()
     t
  })
  
  output$siteDownloadPatients <- downloadHandler(
    filename=function() {paste0("doxPreventICU_PatientData.", input$siteDownloadType)},
    content=function(file) {
              if (input$siteDownloadType == "csv") write.csv(readPatients(session$userData$currentPatient$SiteID, session$userData$settings$testMode), file, row.names=FALSE)
              else saveRDS(readPatients(session$userData$currentPatient$SiteID, session$userData$settings$testMode), file)
            }
  )
  output$downloadHospitalDatas <- downloadHandler(
    filename=function() {paste0("doxPreventICU_HospitalData.", input$siteDownloadType)},
    content=function(file) {
      if (input$siteDownloadType == "csv") write.csv(readHospitalData(session$userData$currentPatient$SiteID, session$userData$settings$testMode), file, row.names=FALSE)
      else saveRDS(readHospitalData(session$userData$currentPatient$SiteID, session$userData$settings$testMode), file)
    }
  )

  observeEvent(input$updateHospitalID, {
    req(session$userData$currentPatient)
    t <- readHospitalData(session$userData$currentPatient$SiteID, session$userData$settings$testMode)
    if (nrow(t %>% filter(PatNo == session$userData$currentPatient$PatNo)) == 0) {
      # Create new record
      t <- t %>% add_row(PatNo=session$userData$currentPatient$PatNo, SiteID=session$userData$user$SiteID, HospitalID=input$hospitalID)
    } else {
      # Update existing record
      t <- t %>% mutate(HospitalID=ifelse(PatNo == session$userData$currentPatient$PatNo, input$hospitalID, HospitalID))
    }
    saveHospitalData(t, session$userData$currentPatient$SiteID, session$userData$settings$testMode)
  })
  
  output$randoSummary <- renderTable({
    input$extendRando
    r <- bind_rows(lapply(c("NONE", "LUNG", "OTHER"), function(x) readRDS(paste0("./sites/", session$userData$user$SiteID, "/rando_", x, ".Rds"))))
    r <- r %>% 
           group_by(Stratum, Allocated) %>% 
           summarise(N=n()) %>% 
           ungroup() %>% 
           mutate(Allocated=ifelse(Allocated, "Assigned", "Available")) %>% 
           spread(key=Allocated, value=N) %>% 
           replace_na(list(Assigned=0, Available=0)) %>% 
           mutate(Assigned=floor(Assigned), Available=floor(Available))
     return(r)
  })
  
  observeEvent(input$extendRando, {
    randomise(session$userData$user$SiteID, "NONE")
    randomise(session$userData$user$SiteID, "LUNG")
    randomise(session$userData$user$SiteID, "OTHER")
  })

  observeEvent(input$age, {
    feedbackWarning(
      inputId="age",
      show=input$age < 40 |  input$age > 90,
      text="Age under 40 or over 90 is an exclusion criterion"
    )
    if (is.na(input$age)) {
      feedbackDanger(
        inputId="age",
        show=TRUE,
        text="Please provide the patient's age"
      )
    }
    if (is.list(session$userData$currentPatient) & isTruthy(input$age)) session$userData$currentPatient$Age <- input$age
  })
  
  observeEvent(input$temperature, {
    feedbackDanger(
      inputId="temperature",
      show=is.na(input$temperature ),
      text="Please provide the patient's temperature. [TODO: Implement temporal temp]"
    )
    if (!is.na(input$temperature)) {
      feedbackWarning(
        inputId="temperature",
        show=input$temperature <= 37.5,
        text="Patient temperature (axillary or frontal) of less than than 37.5 C is an exclusion crtierion"
      )
    }
    if (is.list(session$userData$currentPatient) & isTruthy(input$temperature)) session$userData$currentPatient$Temperature <- input$temperature
  })
  
  observeEvent(input$randomise, {
    req(session$userData$currentPatient)
    if (!is.na(session$userData$currentPatient$Treatment)) {
      displayMessage("Unable to randomise", "This patient has already been randomised!")
      return()
    }
    eligible <- checkEligibility(session$userData$currentPatient)
    if (eligible != TRUE) {
      reasons <- paste0(eligible, sep="</li>", collapse="<li>")
      displayMessage(
        "Unable to randomise!",
        HTML(paste0("This patient does not meet the study's entry criteria for the following reasons:<ul><li>", reasons, "</ul>Please try again."))
      )
      return()
    }
    if (derivedStratum() == "UNDEFINED") {
      displayMessage("Unable to randomise", HTML("You cannot randomise this patien tuntil their stratum has been defined.<br>Please try again."))
    } else {
      # At last.  Randomise!
      rFile <- getRandoFileName(session$userData$user$SiteID, derivedStratum())
      rData <- readRDS(rFile)
      nextRecord <- rData %>%  filter(!Allocated) %>% head(1)
      rData <- rData %>% mutate(Allocated=ifelse(PackNo == nextRecord$PackNo[1], TRUE, Allocated))
      saveRDS(rData, rFile)
      #Trigger only one update
      isolate({
        session$userData$currentPatient$Treatment <- nextRecord$Treatment[1]
      })
      session$userData$currentPatient$Stratum <- input$stratum
      pData <- readPatients(session$userData$user$SiteID, session$userData$settings$testMode)
      
      pData <- pData %>%
        mutate(
          Treatment=ifelse(PatNo == session$userData$currentPatient$PatNo, rData$Treatment, Treatment),
          Stratum=ifelse(PatNo == session$userData$currentPatient$PatNo, derivedStratum(), Stratum)
        )
      saveRDS(pData, pFile)
      displayMessage(
        "Success",
        paste0(
          "Successfully randomised patient ",
          session$userData$currentPatient$PatNo,
          " to ",
          session$userData$currentPatient$Treatment
        )
      )
    }
  })
  
  validateICUAdmission <- function() {
  }
  
  observeEvent(input$icuAdmission, {
    feedbackDanger(
      "icuAdmission",
      show=!isTruthy(input$icuAdmission),
      text="Please indicate if the patient was admitted to ICU."
    )
    if (isTruthy(input$icuAdmission)) {
      validateICUAdmission()
    }
    if (is.list(session$userData$currentPatient) & isTruthy(input$icuAdmission)) session$userData$currentPatient$ICUAdmission <- input$icuAdmission
  })
  
  validateICUAdmissionAttempted <- function() {
    if (input$icuAdmissionAttempted == TRUE) {
      feedbackDanger(
        "icuAdmissionAttemptedDate",
        show=!isTruthy(input$icuAdmissionAttemptedDate),
        text="Please provide the date of the first failed ICU admission attempt"
      )
      if (isTruthy(input$icuAdmissionAttemptedDate)) {
        feedbackWarning(
          "icuAdmissionAttemptedDate",
          show=input$icuAdmissionAttemptedDate > lubridate::today(),
          text="Date of attempted admission cannot be in the future"
        )
      }
    } else if (input$icuAdmissionAttempted == FALSE) {
      feedbackDanger(
        "icuAdmissionAttemptedDate",
        show=isTruthy(input$icuAdmissionAttemptedDate),
        text="The date of failed ICU admission must be blank if Attempted Admission is No"
      )
    }
  }
  
  observeEvent(input$icuAdmissionAttempted, {
    feedbackDanger(
      "icuAdmissionAttempted",
      show=!isTruthy(input$icuAdmissionAttempted),
      text="Please indicate if a failed attempt to admit the patient to ICU was made."
    )
    if (isTruthy(input$icuAdmissionAttempted)) {
      validateICUAdmissionAttempted()
    }
    if (is.list(session$userData$currentPatient) & isTruthy(input$icuAdmissionAttempted)) session$userData$currentPatient$ICUAdmissionAttempted <- input$icuAdmissionAttempted
  })
  
  validateDateOfDeath <- function() {
  }
  
  observeEvent(input$dateOfDeath, {
    validateDateOfDeath()
    if (is.list(session$userData$currentPatient) & isTruthy(input$DateOfDeath)) session$userData$currentPatient$DateOfDeath <- input$DateOfDeath
  })
  
  observeEvent(input$finalStatus, {
    feedbackDanger(
      "finalStatus",
      show=input$finalStatus == "",
      text="Please supply a final status."
    )
    if (is.list(session$userData$currentPatient) & isTruthy(input$finalStatus)) session$userData$currentPatient$FinalStatus <- input$finalStatus
  })
  
  # Reactive UI
  site <- callModule(siteController, "site")
  
  output$allocation <- renderUI({
    req(session$userData$currentPatient)
    # Update on randomisation
    input$randomise
    # Begin
    text <- ifelse(is.na(session$userData$currentPatient$Treatment), "", session$userData$currentPatient$Treatment)
    infoBox(
      "Allocation",
      value=text,
      subtitle=NULL,
      width=12,
      fill=TRUE
    )
  })
  
  repopulatePatientSelectInput <- function() {
    req(session$userData$user)
    #Populate patID combobox dropdown list
    t <- readPatients(session$userData$user$SiteID, session$userData$settings$testMode)
    if (nrow(t) == 0) return()
    if (nrow(t) > 0) {
      id <- ifelse(is.null(input$patId), (t %>% head(1))$PatNo, input$patID)
      updateSelectInput(
        session,
        "patID",
        choices=t %>% mutate(temp=paste0(SiteID, "-", PatNo)) %>% select(temp, PatNo) %>% deframe(),
        selected=id
      )
    }
  }
  
  observeEvent(session$userData$user, {
    session$userData$currentPatient <- readPatients(session$userData$user$SiteID, session$userData$settings$testMode) %>% head(1)
    repopulatePatientSelectInput()
    if (nrow(session$userData$currentPatient) == 1) {
      updateSelectInput(session, "patID", selected=session$userData$currentPatient$PatNo)
    }
  })
  
  observeEvent(session$userData$currentPatient, {
    updateGUI()
  })
  
  observeEvent(input$register, {
    session$userData$currentPatient <- createPatient(session$userData$user$SiteID, testMode=session$userData$settings$testMode)
    repopulatePatientSelectInput()
    updateSelectInput(session, "patID", selected=session$userData$currentPatient$PatNo)
    displayMessage("Registration", "New patient successfully registered")
  })
  
  updateGUI <- function() {
    req(session$userData$currentPatient)
    t <- session$userData$currentPatient
    if (nrow(t) == 0) return()
    
    fields <- names(t)
    fields <- fields[!(fields %in% c("PatNo", "SiteID", "Stratum", "Treatment"))]
    widgetNames <- names(session$input)
    # How to get the class of the underlying widget?  There must be a more general way...
    widgetClass <- c(
      "icDate"="dateInput" ,
      "anosmia"="selectInput" , 
      "dateOfDeath"="dateInput",
      "pneumonia"="selectInput",
      "age"="numericInput",
      "temperature"="numericInput",
      "finalStatus"="selectInput",
      "myastheniaGravis"="selectInput",
      "dyspnea"="selectInput",
      "cirrhosis"="selectInput",
      "icuAdmissionAttempted"="selectInput",
      "alive"="selectInput",
      "sex"="selectInput",
      "ethnicity"="selectInput",
      "followUpDate"="dateInput",
      "admissionDate"="dateInput",
      "hypersensitivity"="selectInput",
      "stratum"="selectInput",
      "stratumILD"="selectInput",
      "stratumCOPD"="selectInput",
      "stratumBronchiectasis"="selectInput",
      "stratumAsthma"="selectInput",
      "stratumOtherLung"="selectInput",
      "stratumDiabetes"="selectInput",
      "stratumHeartDisease"="selectInput",
      "stratumHypertension"="selectInput",
      "stratumCancer"="selectInput",
      "stratumOther"="selectInput",
      "trtDoxycycline"="selectInput",
      "trtAntibiotics"="selectInput",
      "trtAnalgesics"="selectInput",
      "trtDiabetes"="selectInput",
      "trtHeartFailure"="selectInput",
      "trtAntiHypertensives"="selectInput",
      "trtAntiDepressants"="selectInput",
      "trtCancer"="selectInput",
      "trtImmunosuppressant"="selectInput",
      "trtOther"="selectInput",
      "icuAdmissionNeeded"="selectInput",
      "icuAdmissionNeededDate"="dateInput",
      "icuDischargeDate"="dateInput",
      "icuNonAdmissionReason"="selectInput",
      "icuAdmissionSuccess"="selectInput",
      "pcrTest"="selectInput",
      "diarrhoea"="selectInput",
      "onsetDate"="dateInput",
      "hospitalDischargeDate"="dateInput",
      "pregnancy"="selectInput",
      "relatedDeath"="selectInput")
    for (f in fields) {
      w <- fieldToWidget(f)
      if (w %in% widgetNames & !is.na(t[[f]])) {
        switch(
          widgetClass[w],
          "dateInput"=updateDateInput(session, w, value=as.Date(t[[f]], origin="1970-01-01")),
          "numericInput"=updateNumericInput(session, w, value=t[[f]]),
          "selectInput"=updateSelectInput(session, w, selected=t[[f]]),
          stop(paste0("Unable to process ", widgetClass[w]," widgets!"))
        )
      } else {
        switch(
          widgetClass[w],
          "dateInput"=updateDateInput(session, paste0(w, "-date"), value=as.Date(t[[f]], origin="1970-01-01")),
          "selectInput"=updateSelectInput(session, paste0(w, "-select"), selected=t[[f]]),
          print(paste0("No widget for ", f))
        )
      }
      # Hospital ID
      h <- readHospitalData(session$userData$currentPatient$SiteID, session$userData$settings$testMode) %>% 
             filter(PatNo == session$userData$currentPatient$PatNo)
      if (nrow(h) == 0) {
        updateTextInput(session, "hospitalID", value="")
      } else {
        updateTextInput(session, "hospitalID", value=h$HospitalID)
      }
    }
    # Enable or disable stratum widgets depending on randomisation status
    stratumWidgets <- names(session$input)
    stratumWidgets <- stratumWidgets[startsWith(stratumWidgets, "stratum")]
    sapply(stratumWidgets, function(x) if (is.na(session$userData$currentPatient$Treatment)) enable(x) else disable(x))
  }
  
  observeEvent(session$userData$currentPatient, {
    updateGUI()
  })
  
  observeEvent(input$patID, {
    req(session$userData$user, input$patID)
    p <- readPatients(session$userData$user$SiteID, session$userData$settings$testMode) %>% filter(PatNo == input$patID)
    if (nrow(p) != 1) {
      displayMessage("Error!", paste0("Unable to load data for patient ", session$userData$user$SiteID, "-", input$patID))
    } else {
      session$userData$currentPatient <- p
      updateGUI()
    }
  })
  
  # UI
  output$adminSiteID <- renderText({
    if (is.list(session$userData$currentAdminSite)) return(paste0("Site ID: ", session$userData$currentAdminSite[["SiteID"]]))
    else if (is.na(session$userData$user)) return(paste0("Site ID: ", session$userData$user[["SiteID"]]))
    else return("SiteID: - Unknown -")
  })
  
  output$messageMenu <- renderMenu({
    dropdownMenu(
      type="messages", 
      .list=list()
    )
  })
  
  output$taskMenu <- renderMenu({
    dropdownMenu(
      type="tasks", 
      .list=list()
    )
  })
  
  output$notificationMenu <- renderMenu({
    dropdownMenu(
      type="notifications", 
      .list=list()
    )
  })
  
  output$userDetails <- renderText({
    req(session$userData$user, session$userData$settings$testMode)
    s <- paste0("Logged in as '", session$userData$user$EmailAddress, "' [", ifelse(session$userData$user$Admin, "Admin", "User"), "] at site ", session$userData$user$SiteID, ".")
    if (session$userData$user$Admin) {
      if (session$userData$settings$testMode) {
        s <- paste0(s, HTML(" <span style='color:green;'>Running in TEST mode.</span>"))
      } else {
        s <- paste0(s, HTML(" <span style='color:blue;'>Running in LIVE mode.</span>"))
      }
    }
  })
  
  output$currentPatient <- renderText({
    input$patID
    req(session$userData$currentPatient)
    if (nrow(session$userData$currentPatient)) {
      paste0(
        "<h2>Patient: ", 
        session$userData$currentPatient %>% 
          mutate(temp=paste0(SiteID, "-", PatNo)) %>% 
          select(temp) %>% 
          deframe(),
        "</h2>"
      )
    } else {
      "<h2>No patients at site</h2>"
    }
  })
  
  derivedStratum <- reactive({
    lungRisks <- c(stratumILD(), stratumCOPD(), stratumBronchiectasis(), stratumAsthma(), stratumOtherLung())
    otherRisks <- c(stratumDiabetes(), stratumHeartDisease(), stratumHypertension(), stratumCancer(), stratumOther())
    
    if (any(c(lungRisks, otherRisks) == "")) rv <- "UNDEFINED"
    else {
      if (any(lungRisks)) rv <- "LUNG"
      else if (any(otherRisks)) rv <- "OTHER"
      else rv <- "NONE"
    }
    req(session$userData$currentPatient)
    session$userData$currentPatient$Stratum <- rv
    return(rv)
  })
  
  output$derivedStratumText <- renderText({
    switch(
      derivedStratum(),
      "LUNG"="Respiratory co-morbidities with or without cardiac involvement",
      "OTHER"="Cardiac co-morbidities without respiratory involvement",
      "NONE"="No relevant co-morbidities",
      "UNDEFINED"="You must confirm the presence or absence of all relevant co-morbidities before the stratum can be determined."
    )
  })
}

# Run the application 
shinyApp(ui, server)