saveButtonUI <- function(
                  id, 
                  buttonLabel="Save changes",
                  boxStatus="success",
                  boxTitle="Save",
                  ...
                ) {
  ns <- NS(id)
  
  box(
    status=boxStatus,
    title=boxTitle,
    actionButton(inputId=ns("save"), label=buttonLabel, ...)
  )
}

saveButtonController <- function(input, output, session, saveFunc) {
  ns <- session$ns
  
  observeEvent(input$save, {
    saveFunc(session)
  })
}