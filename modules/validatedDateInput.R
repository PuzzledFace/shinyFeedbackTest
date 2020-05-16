#' @param  id Module ID
#' @param ... Passed through to dateInput
validatedDateInputUI <- function(id, value=NA, ...) {
  ns <- NS(id)
  
  dateInput(inputId=ns("date"), value=value, ...)
}

#' @param input Mandatory.  The input object for the module
#' @param output Mandatory.  The output object for the module
#' @param session Mandatory.  The session object for the module
#' @param obj The element of the PARENT session$userData to which the return value is written
#' @param errors Values of input$select that will generage an error message, and the message to display
#' @param warnings Values of input$select that will generage a warning message, and the message to display
#' @param allowFuture Are dates which are in the future allowed?
#' @param valFunc A function to do additional custom validation  Takes session and ... as parameters.  Return TRUE if it OK to update the Not tested.
#' @param ... Additional parameters passed to valFunc
validatedDateInputController <- function(
  input,
  output,
  session,
  obj="currentPatient",
  allowFuture=FALSE,
  errors=list("Please supply a value"=NA),
  warnings=list(),
  valFunc=NULL,
  ...
) {
  ns <- session$ns
  target <- widgetToField(str_replace(ns(""), "-", ""))
  
  # Handle changes to the input widget
  observeEvent(input$date, {
    # See https://github.com/rstudio/shiny/issues/1546
    parentSession <- .subset2(session, "parent")
    feedbackWarning(
      inputId=ns("date"),
      show=input$date %in% warnings,
      text=names(warnings)[[input$date]]
    )
    req(input$date)
    if (!allowFuture) {
      feedbackDanger(
        inputId=ns("date"),
        show=input$date > Sys.Date(),
        text="A date in the future is not allowed"
      )
    }
    ok <- TRUE
    if (!is.null(valFunc)) ok <- valFunc(session, ...)
    parentSession$userData[[obj]][[target]] <- input$date
  })
  
  return(reactive({input$date}))
}
