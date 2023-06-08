server_show_tables<-function(input, output, session){
  
output$MixedModelTable <- renderDT({if (length(colnames(ModelMixedResults()))) {
  shiny::req(ModelMixedResults())
  #print(colnames(ModelMixedResults()))
  #print(colnames(ModelMixedResults()) %in% c('orig','dest','year','model','pred_q50','pred_alt_q50'))
  dbdata<-ModelMixedResults()[, c('orig','dest','year','model','pred_q50')]#,'pred_alt_q50')]
  colnames(dbdata)<-c('Origin','Destination','Year','Model','Median')#,'Median (alternative)')
  datatable(
    dbdata,
    rownames = FALSE,
    options = list(
      searching = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:4)),
      pageLength = 50,
      info = FALSE
    )
  )
}}, server = FALSE)

output$MixedModelDefaultTable <- renderDT({
  
  dbdata<-ModelMixedResultsDefault[, c('orig','dest','year','model','pred_q50')]#,'pred_alt_q50')]
  colnames(dbdata)<-c('Origin','Destination','Year','Model','Median')#,'Median (alternative)')
  datatable(
    dbdata,
    rownames = FALSE,
    options = list(
      searching = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:4)),
      pageLength = 50,
      info = FALSE
    )
  )
}, server = FALSE)

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