server_show_tables<-function(input, output){
  
# output$MixedModelDefaultTable <- renderDT({
#   
#   dbdata<-ModelMixedResultsDefault[, c('sending_country','receiving_country','year','model','pred_q50')]#,'pred_alt_q50')]
#   colnames(dbdata)<-c('Origin','Destination','Year','Model','Median')#,'Median (alternative)')
#   suppressWarnings(datatable(
#     dbdata,
#     rownames = FALSE,
#     options = list(
#       searching = TRUE,
#       columnDefs = list(list(className = 'dt-center', targets = 0:4)),
#       pageLength = 50,
#       info = FALSE
#     )
#   ))
# }, server = FALSE)

output$CoverageTable <- renderDT({
  datatable(
    DTCoverage,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      columnDefs = list(
        list(className = 'dt-left', targets = 0),
        list(className = 'dt-center', targets = 1),
        list(className = 'dt-center', targets = 2)
      ),
      pageLength = 100,
      dom = 't',
      info = FALSE
    )
  )
}, server = FALSE)

output$CoverageTransitionsTable <- renderDT({
  datatable(
    DTCoverageTr,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      columnDefs = list(
        list(className = 'dt-left', targets = 0),
        list(className = 'dt-center', targets = 1),
        list(className = 'dt-center', targets = 2)
      ),
      pageLength = 100,
      dom = 't',
      info = FALSE
    )
  )
}, server = FALSE)

output$UndercountingTransitionsTable <- renderDT({
  datatable(
    DTUndercountingTr,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      columnDefs = list(
        list(className = 'dt-left', targets = 0),
        list(className = 'dt-center', targets = 1),
        list(className = 'dt-center', targets = 2)
      ),
      pageLength = 100,
      dom = 't',
      info = FALSE
    )
  )
}, server = FALSE)

output$AccuracyTransitionsTable <- renderDT({
  datatable(
    DTAccuracyTr,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      columnDefs = list(
        list(className = 'dt-left', targets = 0),
        list(className = 'dt-center', targets = 1),
        list(className = 'dt-center', targets = 2)
      ),
      pageLength = 100,
      dom = 't',
      info = FALSE
    )
  )
}, server = FALSE)
}