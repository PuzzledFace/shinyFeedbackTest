#' @param  id Module ID
#' @param choices the choices presented by the widget to users
#' @param ... Passed through to selectInput
validatedSelectInputUI <- function(
  id,
  choiceList=c("- Select one -"="", "Yes"=TRUE, "No"=FALSE),
  ...
) {
  ns <- NS(id)
  
  selectInput(
    inputId=ns("select"),
    choices=choiceList,
    ...
  )
}

#' @param input Mandatory.  The input object for the module
#' @param output Mandatory.  The output object for the module
#' @param session Mandatory.  The session object for the module
#' @param obj The element of the PARENT session$userData to which the return value is written
#' @param allowMissing  Are missing values permitted?  We can't use errors=list('Please supply a value'='') because this creates an illegal index at names(errors)[[input$select]], 
#' @param errors Values of input$select that will generage an error message, and the message to display
#' @param warnings Values of input$select that will generage a warning message, and the message to display
#' @param valFunc A function to do additional custom validation  Takes session and ... as parameters.  Return TRUE if it OK to update the Not tested.
#' @param ... Additional parameters passed to valFunc
validatedSelectInputController <- function(
  input,
  output,
  session,
  obj="currentPatient",
  allowMissing=FALSE,
  errors=list('Please supply a value'=''),
  warnings=list(),
  valFunc=NULL,
  ...
) {
  ns <- session$ns
  target <- widgetToField(str_replace(ns(""), "-", ""))
  
  # Handle changes to the input widget
  observeEvent(input$select, {
    parentSession <- .subset2(session, "parent")
    req(parentSession$userData[[obj]])
    # See https://github.com/rstudio/shiny/issues/1546
    text <- ifelse(input$select %in% warnings, names(warnings)[[match(input$select, warnings)]], "")
    feedbackWarning(
      inputId=ns("select"),
      show=input$select %in% warnings,
      text=text
    )
    text <- ifelse(input$select %in% errors, names(errors)[[match(input$select, errors)]], "")
    feedbackDanger(
      inputId=ns("select"),
      show=input$select %in% errors,
      text=text
    )
    ok <- TRUE
    if (!is.null(valFunc)) ok <- valFunc(session, ...)
    parentSession$userData[[obj]][[target]] <- input$select
  })
  
  return(reactive({input$select}))
}
