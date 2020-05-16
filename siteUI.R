siteUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("adminGUI"))
}  

siteController <- function(input, output, session) {
  ns <- session$ns
  
  observe({
    c <- readCredentials(session$userData$settings$testMode)
  })
  
  updateGUI <- function() {
    if (session$userData$user$Admin) {
      updateSelectInput(session, inputId="adminSite", selected=session$userData$currentAdminSite[["SiteID"]])
      updateCheckboxInput(session, "isAdmin", value=session$userData$currentAdminSite[["Admin"]])
    }
    updateTextInput(session, inputId="Title", value=session$userData$currentAdminSite[["Title"]])
    updateTextInput(session, inputId="GivenName", value=session$userData$currentAdminSite[["GivenName"]])
    updateTextInput(session, inputId="FamilyName", value=session$userData$currentAdminSite[["FamilyName"]])
    updateTextInput(session, inputId="Email", value=session$userData$currentAdminSite[["EmailAddress"]])
  }
  
  observeEvent(input$Email, {
    valid <- isValidEmailAddress(input$Email)
    feedbackDanger(
      "Email",
      condition=!valid,
      text="Please enter a valid email address"
    )
    if (valid) session$userData$currentAdminSite$EmailAddress <- input$Email
  })
  
  observeEvent(input$Title, {
    valid <- length(input$Title) > 0
    feedbackDanger(
      "Title",
      condition=!valid,
      text="Investigator title cannot be blank"
    )
    if (valid) session$userData$currentAdminSite$Title <- input$Title
  })
  
  observeEvent(input$GivenName, {
    valid <- length(input$GivenName) > 0
    feedbackDanger(
      "GivenName",
      condition=!valid,
      text="Given name cannot be blank"
    )
    if (valid) session$userData$currentAdminSite$GivenName <- input$GivenName
  })
  
  observeEvent(input$FamilyName, {
    valid <- length(input$FamilyName) > 0
    feedbackDanger(
      "FamilyName",
      condition=!valid,
      text="Family name cannot be blank"
    )
    if (valid) session$userData$currentAdminSite$FamilyName <- input$FamilyName
  })
  
  observeEvent(input$adminSite, {
    site <- c %>% filter(SiteID == input$adminSite)
    feedbackDanger(
      "adminSiteID",
      condition=nrow(site) != 1,
      text="Unable to retrive site details"
    )
    if (nrow(site) != 1) return()
    session$userData$currentAdminSite <- as.vector(site)
    updateGUI()
  })
  
  observeEvent(input$adminSaveSite, {
    c <- c %>% 
           mutate(
             EmailAddress=ifelse(SiteID == session$userData$currentAdminSite$SiteID, input$Email, EmailAddress),
             Title=ifelse(SiteID == session$userData$currentAdminSite$SiteID, input$Title, Title),
             GivenName=ifelse(SiteID == session$userData$currentAdminSite$SiteID, input$GivenName, GivenName),
             FamilyName=ifelse(SiteID == session$userData$currentAdminSite$SiteID, input$FamilyName, FamilyName)
           )
    if (session$userData$user$Admin) {
      c <- c %>% 
           mutate(
             Admin=ifelse(SiteID == session$userData$currentAdminSite$SiteID, input$isAdmin, Admin)
           )
    }
    updateSelectInput(
      session, 
      "adminSite", 
      choices=c %>% mutate(temp=paste0(SiteID, " ", Title, " ", FamilyName)) %>% select(temp, SiteID) %>% deframe(),
      selected=session$userData$currentAdminSite$SiteID
    )
    saveCredentials(c, session$userData$settings$testMode)
    displayMessage("Information", "Changes saved")
  })
  
  observeEvent(input$adminNewSite, {
    newID <- sprintf("%03i", max(as.numeric(c$SiteID)) + 1)
    createNewSite(newID)
    c <- readCredentials(session$userData$settings$testMode)
    updateSelectInput(
      session, 
      "adminSite", 
      choices=c %>% mutate(temp=paste0(SiteID, " ", Title, " ", FamilyName)) %>% select(temp, SiteID) %>% deframe()
    )
    updateSelectInput(
      session, 
      "adminSite", 
      selected=newID
    )
    session$userData$currentAdminSite <- as.vector(c %>% filter(SiteID == newID))
  })
  
  observeEvent (session$userData$currentAdminSite, {
    req(session$userData$user)
    updateGUI()
  })
  
  output$downloadPatients <- downloadHandler(
    filename = function() {
      paste0("Patients", session$userData$currentAdminSite$SiteID, ".", input$downloadFormat)
    },
    content = function(file) {
      if (input$downloadFormat=="csv") write.csv(readPatients(session$userData$currentAdminSite$SiteID, session$userData$settings$testMode), file, row.names=FALSE)
      else saveRDS(readPatients(session$userData$currentAdminSite$SiteID, session$userData$settings$testMode), file)
    }
  )
  
  output$adminGUI <- renderUI({
    administratorBox <- 
      box(
        status="primary",
        width=10,
        solidHeader=TRUE,
        collapsible=FALSE,
        title="Administrator functions",
        fluidRow(
         selectInput(
            ns("adminSite"), 
            label="Current site", 
            choices=c %>% mutate(temp=paste0(SiteID, " ", Title, " ", FamilyName)) %>% select(temp, SiteID) %>% deframe()
          ),
          actionButton(
            ns("adminNewSite"), 
            label="Create new site"
          )
        )
      )

    siteBox <-
      box(
        status="primary",
        width=10,
        solidHeader=TRUE,
        collapsible=TRUE,
        title="Site details",
        textOutput(ns("adminSiteID")),
        fluidRow(
          column(
            width=3,
            textInput(ns("Title"), "Investigator title", placeholder="Eg `Dr`, `Prof med` etc")
          ),
          column(
            width=4,
            textInput(ns("GivenName"), "Given name")
          ),
          column(
            width=5,
            textInput(ns("FamilyName"), "Family name")
          )
        ),
        textInput(ns("Email"), "Investigator email address", placeholder="Enter the investigator's email address"),
        fluidRow(
          actionButton(
            ns("adminSaveSite"), 
            label="Save changes"
          )
        )
      )
    
    siteBoxAdmin <-
      box(
        status="primary",
        width=12,
        solidHeader=TRUE,
        collapsible=TRUE,
        title="Site details",
        textOutput(ns("adminSiteID")),
        fluidRow(
          column(
            width=3,
            textInput(ns("Title"), "Investigator title", placeholder="Eg `Dr`, `Prof med` etc")
          ),
          column(
            width=4,
            textInput(ns("GivenName"), "Given name")
          ),
          column(
            width=5,
            textInput(ns("FamilyName"), "Family name")
          )
        ),
        fluidRow(
          textInput(ns("Email"), "Investigator email address", placeholder="Enter the investigator's email address")
        ),
        fluidRow(
          checkboxInput(
            ns("isAdmin"), 
            "Study administrator"
          )
        ),
        fluidRow(
          actionButton(
            ns("adminSaveSite"), 
            label="Save changes"
          )
        )
      )

    downloadBox <-
      box(
        title="Download site data",
        solidHeader=TRUE,
        collapsible=TRUE,
        collapsed=TRUE,
        status="info",
        selectInput(ns("downloadFormat"), "Download format", choices=c("CSV"="csv", "R data file"="Rds")),
        downloadButton(ns("downloadPatients"), "Download patient data"),
        downloadButton(ns("downloadTreatment"), "Download treatment records")
      )
  
  if (session$userData$user$Admin) {
     ui <- tabItem(
      tabName="admin",
      fluidRow(
        # administratorBox,
        # siteBoxAdmin,
        # downloadBox
        box(
          "Testing..."
        )
      )
    )
  } else {
    ui <- tabItem(
      tabName="admin",
      fluidRow(
        siteBox,
        downloadBox
      )
    )
  }
    return(tagList(ui))
})

}