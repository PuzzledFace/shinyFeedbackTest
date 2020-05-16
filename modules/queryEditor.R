queryEditorUI <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    fluidRow(
      column(
        width=2,
        selectInput(ns("showTypes"), label="Filter by status:", choices=c("Only open queries"="OPEN", "Both open and closed queries"="BOTH", "Only closed queries"="CLOSED"))
      )
    ),
    fluidRow(
      DTOutput(ns("queries"))
    ),
    fluidRow(
      actionButton(ns("runQualityCheck"), "Run QC check")
    )
  )
}

getSiteQueries <- function(siteID) {
  fileName <- getQueryFileName(siteID)
  readRDS(fileName)
}

queryEditorController <- function(
                           input, 
                           output, 
                           session, 
                           displayedColumns=c("PatNo", "FirstRaised", "QueryText", "SiteComment", "AdminComment", "Status"),
                           editableColumns=c("SiteComment")
                         ) {
 ns <- session$ns
 
 v <- reactiveValues(data=NA)
 
 loadData <- function() {
   parentSession <- .subset2(session, "parent")
   req(parentSession$userData$user)
   if (parentSession$userData$user$Admin) {
     sites <- list.dirs(path="./sites/", full.names=FALSE, recursive=FALSE)
     v$data <- bind_rows(lapply(sites, function(x) getSiteQueries(x)))
   } else {
     v$data <- getSiteQueries(parentSession$userData$user$SiteID)
   }
 }
 
 observe({
   if (is_tibble(v$data)) return()
   loadData()
 })
 
 proxy <- dataTableProxy("queries")
 
 displayedQueries <- reactive({
   rv <- v$data %>% select(displayedColumns)
   switch(input$showTypes,
          "OPEN"=rv <- rv %>% filter(Status=="OPEN"),
          "CLOSED"= rv <- rv %>% filter(Status=="CLOSED"),
          "BOTH"=rv <- rv
   )
   rv
 })
 
 output$queries <- renderDT({
   datatable(
     displayedQueries(),
     rownames=FALSE,
     selection=list(mode="single", target="cell"),
     editable=list(target="cell", disable=list(columns=(0:(length(displayedColumns)-1))[!(names(displayedQueries()) %in% editableColumns)])),
     filter="top",
     caption="Queries corrected by data changes will be closed automatically when the QC report is next run.  For other queries, please comment as appropriate."
   )
 })
 
 observeEvent(input$queries_cell_edit, {
   row <- input$queries_cell_edit$row
   col <- input$queries_cell_edit$col
   val <- input$queries_cell_edit$val
   colName <- displayedColumns[col+1]
   colIndex <- which(names(v$data) == colName)
   # isolate(v$data[row, colIndex] <- val)
   # replaceData(proxy, v$data, resetPaging=FALSE)
   v$data[row, colIndex] <- val
   saveQueries(v$data %>% filter(SiteID == v$data$SiteID[row]), v$data$SiteID[row], session$userData$settings$testMode)
 })
 
 observeEvent(input$runQualityCheck, {
   print("Running quality check...")
   lapply(unique(v$data$SiteID), function(x) runDataQualityCheck(x))
   loadData()
 })
}