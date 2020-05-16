siteEditorUI <- function(id, adminMode=FALSE) {
  ns <- NS(id)
  
  if (adminMode) {
    selector <- tagList(
                  wellPanel(
                    fluidRow(
                      selectInput(ns("site"), "Select site:", choices=list.dirs(path="./sites", recursive=FALSE, full.names=FALSE), selected=NA),
                      selectInput(ns("user"), "Select investigator:", choices=c(), selected=NA)
                    )
                  )
                )
    buttons <- tagList(
      fluidRow(
        actionButton(ns("save"), "Save"), 
        actionButton(ns("new"), "Register new site")
      )
    )
  } else {
    buttons <- tagList(fluidRow(actionButton(ns("save"), "Save")))
  }
  widgets <- tagList(
               fluidRow(textInput(ns("title"), "Title:")),
               fluidRow(textInput(ns("givenName"), "Given name:")),
               fluidRow(textInput(ns("familyName"), "Family name:")),
               fluidRow(textInput(ns("emailAddress"), "Email address:"))
            )
  if (adminMode) {
    tagList(selector, wellPanel(widgets, fluidRow(selectInput(ns("admin"), "Administrator?:", choices=c("Yes"=TRUE, "No"=FALSE), selected=FALSE)),buttons))
  } else {
    tagList(wellPanel(widgets, buttons))
  }
}

siteEditorController <- function(input, output, session) {
  ns <- session$ns
  
  v <- reactiveValues(
         displayedUser=NA,
         actualUser=NA,
         inputNames=NA,
         initialised=FALSE
       )
  
  observe({
    # Prevent overwriting changes to the site selectInput
    if (v$initialised) return()
    parentSession <- .subset2(session, "parent")
    v$displayedUser <- parentSession$userData$user
    v$actualUser <- v$displayedUser
    v$inputNames <- names(input)
    v$initialised <- TRUE
  })
  
  observeEvent(v$displayedUser, {
    req(v$displayedUser)
    updateTextInput(session, "title", value=v$displayedUser$Title)
    updateTextInput(session, "givenName", value=v$displayedUser$GivenName)
    updateTextInput(session, "familyName", value=v$displayedUser$FamilyName)
    updateTextInput(session, "emailAddress", value=v$displayedUser$EmailAddress)
    # if ("admin" %in% v$inputNames) updateSelectInput(session, "admin", selected=v$displayedUser$Admin)
    if ("site" %in% v$inputNames) updateSelectInput(session, "site", selected=v$displayedUser$SiteID)
  })
  
  observeEvent(input$save, {
    c <- readCredentials(session$userData$settings$testMode)
    c <- c %>% mutate(
                 Title=ifelse(UserID == v$displayedUser$UserID, input$title, Title),
                 GivenName=ifelse(UserID == v$displayedUser$UserID, input$givenName, GivenName),
                 FamilyName=ifelse(UserID == v$displayedUser$UserID, input$familyName, FamilyName),
                 EmailAddress=ifelse(UserID == v$displayedUser$UserID, input$emailAddress, EmailAddress)
               )
    if ("admin" %in% names(input)) {
      c <- c %>% mutate(
                   Admin=ifelse(UserID == v$displayedUser$UserID, input$admin, Admin)
                 )
    }
    saveCredentials(c, session$userData$settings$testMode)
    #In case investigator name has changed
    populateInvestigatorList(input$site)
    displayMessage("Success", "Changes saved successfully.")
  })
  
  populateSiteList <- function() {
    c <- readCredentials(session$userData$settings$testMode) %>% 
           select(SiteID)
    updateSelectInput(session, "site", choices=c$SiteID)
  }
  populateInvestigatorList <- function(siteID) {
    req(session$userData$settings$testMode)
    c <- readCredentials(session$userData$settings$testMode) %>% 
           filter(SiteID == siteID) %>% 
           mutate(
             DisplayText=paste0(
                           ifelse(is.na(Title), "", paste0(Title, " ")),
                           ifelse(is.na(GivenName), "", paste0(GivenName, " ")),
                           ifelse(is.na(FamilyName), "- New Investigator -", FamilyName)
                         )
           ) %>% 
           select(DisplayText, UserID)
    updateSelectInput(session, "user", choices=c %>% deframe(), selected=c$UserID[1])
  }
  
  observeEvent(input$site, {
    req(input$site)
    populateInvestigatorList(input$site)
  })
  
  observeEvent(input$user, {
    req(input$site, input$user)
    #Update the editor panel
    user <- readCredentials(session$userData$settings$testMode) %>% 
      filter(
        SiteID == input$site,
        UserID == input$user
      )
    if (nrow(user) == 1) {
      v$displayedUser <- user
    } else {
      displayMessage("Error!", HTML(paste0("Unable to retrieve details for site ", input$site, "; user: ", input$user, ".<br>This shouldn't happen!")))
    }
  })
  
  observeEvent(input$new, {
    c <- readCredentials(session$userData$settings$testMode)
    if (nrow(c) > 0) {
      userID <- max(c$UserID) + 1
      siteID <- sprintf("%03.0f", max(as.numeric(c$SiteID)) + 1)
    }
    else {
      userID <- 1
      siteID <- "001"
    }
    c <- c %>% add_row(SiteID=siteID, UserID=userID, Admin=FALSE)
    saveCredentials(c, session$userData$settings$testMode)
    createNewSite(siteID, updateCredentials=FALSE, testMode=session$userData$settings$testMode)
    isolate({
      v$displayedUser$SiteID <- siteID
      v$displayedUser$UserID <- userID
      populateSiteList()
    })
    updateSelectInput(session, "site", selected=siteID)
  })
  
  observeEvent(input$title, {
    feedbackDanger(inputId=ns("title"), show=(nchar(input$title) == 0), text="Investigator title cannot be blank")
  })
  
  observeEvent(input$givenName, {
    feedbackDanger(ns("givenName"), show=(nchar(input$givenName) == 0), text="Investigator given name cannot be blank"
    )
  })
  
  observeEvent(input$familyName, {
    feedbackDanger(ns("familyName"), show=(nchar(input$familyName) == 0), text="Investigator family name cannot be blank"
    )
  })
  
  observeEvent(input$emailAddress, {
    feedbackDanger(ns("emailAddress"), show=!isValidEmailAddress(input$emailAddress), text="Please enter a valid email address")
  })
}