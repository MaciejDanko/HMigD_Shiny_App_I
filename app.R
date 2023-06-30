rm(list=ls())
set.seed(123)
defaultW <- getOption("warn") # 0
options(warn = 1)

library(Cairo)
library(RColorBrewer)
library(devtools)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(usethis)
library(shinyhelper)
library(shinycssloaders)
library(magicaxis)
library(data.table)
library(countrycode)
library(openxlsx)
library(bslib)
library(tidyverse)
library(circlize)
library(RSQLite)
library(DT)
library(dplyr)
library(httr)
library(googlesheets4)
library(googledrive)

library(rhandsontable)

options(bitmapType="cairo")

makebold<<-function(x) unname(sapply(x, function(k) HTML(unname(paste0('<b>',k,'</b>')))))

makeList<<-function(x){
  r<-as.list(seq_along(x))
  names(r)<-x#sapply(x,HTML)
  r
}

mix_models<-function(model_mat){
  res<- lapply(letters[1:6], function(j) {
    cn<-colnames(model_mat)
    rn<-rownames(model_mat)
    tmp<-apply(model_mat, 1, function(k)  cn[k==j])
    tmp<-lapply(seq_along(tmp), function(k) if (length(tmp[[k]])) data.frame(origin=rn[k], dest=tmp[[k]]) )
    Select<-data.table::rbindlist(tmp)
    Select<-Select[Select$origin!=Select$dest,]
    Select$id<-paste(Select$origin,Select$dest,sep='_')
    if (length(Select$id)) {
      data<-switch(j, 
                   'a' = Data_input_80a, 
                   'b' = Data_input_80b,
                   'c' = Data_input_80c,
                   'd' = Data_input_80d,
                   'e' = Data_input_80e,
                   'f' = Data_input_80f)  
      #data=Data_input_80a
      data<-as.data.frame(data)
      data$id<-paste(data$orig,data$dest, sep='_')
      
      data<-data[which(data$id %in% Select$id),c('orig','dest','year',sort(c('pred_q05',
                                                                             'pred_q25', 'pred_q50', 'pred_q75',
                                                                             'pred_q95'#, 
                                                                             #'pred_alt_q05', 'pred_alt_q25', 'pred_alt_q50',
                                                                             #'pred_alt_q75', 'pred_alt_q95'
                                                                             )))]
      data$model<-j
      data
    } else NULL
  })
  res<-as.data.frame(data.table::rbindlist(res))
  res[order(paste(res$year, res$orig, res$dest)),]
  
}

# makeList(LETTERS)
# colabout="#A9DFBF"
# colimmi="#AED6F1"
# colemi="#FAD7A0"
# coltxt='black'
# colsel='#873600'
source('./.secrets/getpass.R')
source('./code/load_texts.R')
source('./code/load_data.R')
source('./code/plot_pair.R')
source('./code/plot_input.R')
source('./code/code_input.R')
try(source('./code/google_drive.R'))
source('./code/run_example1.R')
source('./code/server_show_tables.R')
source('./code/server_download_tables.R')
source('./code/server_plot_figures.R')
source('./code/server_save_figures.R')
source('./code/server_button_pass_check.R')
source('./code/ui_panels.R')

OLD_send_ini<<-CountriesFull[Countries%in% c('CZ', 'EE', 'LT', 'LV', 'PL', 'SI', 'SK','HU')]
OLD_rec_ini<<-CountriesFull[Countries%in% c('UK')]

figures_path <- "./figures"
addResourcePath(prefix = "figurespath", directoryPath = figures_path)
#There is a clear problem with DK->DE flows. The flows reported by Eurostat are very similar, but the two datasets have very different definitions of the duration of a migrant's stay. For Germany it is 0-1 month and for Denmark a year. We expect that in this case the German data should be overestimated in relation to the DK data. Since the model takes into account the duration of stay, the German data underestimates the flows estimated by the model. However, we don't know where the problem lies. Or the Danes use German data for their calculations, the Germans do it with Danish data.

NCntr<-length(Countries)
initial_values <- matrix(rep("f", NCntr * NCntr), nrow = NCntr, ncol = NCntr)
colnames(initial_values)<-Countries 
rownames(initial_values)<-Countries
initial_values[c('BG','BE','CH','CZ','FR','GR','HR','IE','LU','LV','RO','MT'), 'PT'] <- "b"
initial_values[c('BG','BE','CZ','ES','GR','HU','IT','LV','NL','PT','RO','SI','SK','UK'), 'CY'] <- "b"
initial_values[c('BG','FR','IT','PT'), 'RO'] <- "b"
initial_values[c('CY','PT'), 'MT'] <- "b"
initial_values[c('CY','GR','IT','NL','SK'), 'PL'] <- "b"
initial_values[c('CY','IT','UK'), 'SK'] <- "b"
initial_values[c('UK','BE','FR','CH','HU','IE','ES','NL','PL'), 'CZ'] <- "b"
initial_values[c('UK','IE'), 'LT'] <- "b"
initial_values[c('BG','DK','GR','IE','MT','PT','UK'), 'LV'] <- "b"
initial_values[c('UK','SI','LT'), 'LU'] <- "b"
initial_values[c('IE','HU','EE','DK','CH','BE','AT'), 'LU'] <- "b"
initial_values[c('UK','RO','PL','LU','IE','BG','BE','AT'), 'FR'] <- "b"
initial_values[c('IE','LT','LV','NL'), 'ES'] <- "b"
initial_values[c('CH','FR','UK'), 'HR'] <- "b"
initial_values[c('CY','CZ','ES','FR','IE','MT','PL','SK','UK'), 'HU'] <- "b"

ModelMixedResultsDefault<<-mix_models(initial_values)

users <<- reactiveValues(count = 0)

shinyServer <-  function(input, output, session) {
  
  OLD_send <- reactiveVal(OLD_send_ini)
  OLD_rec <- reactiveVal(OLD_rec_ini)
  justchanged <- reactiveVal(TRUE)
  firstrunSen <- reactiveVal(TRUE)
  firstrunRec <- reactiveVal(TRUE)
  
  Ymaxenabled <-reactiveVal(FALSE)  
  
  ThresholdYear <- reactiveVal(2000)
  
  AggrSave <- reactiveVal(NULL)
  SingleSave <- reactiveVal(NULL)
  
  observeEvent(input$bprev, {
    print('dec')
    req(ThresholdYear())
    if (as.numeric(ThresholdYear())>2000) ThresholdYear(as.numeric(ThresholdYear()) - 1)
  })
  observeEvent(input$bnext ,{
    print('inc')
    req(ThresholdYear())
    if (as.numeric(ThresholdYear())<2020) ThresholdYear(as.numeric(ThresholdYear()) + 1)
  })
  output$ThrY <- renderUI({
    h4(ThresholdYear())
  })
  
  onSessionStart <- isolate({
    users$count <- users$count + 1
    shinyjs::disable('YMaxCompareModels')
    shinyjs::html("YMaxCompareModels-label",'<span style="color: gray;">Max Y-axis value</span>')
    Ymaxenabled(FALSE)
  })
  
  onSessionEnded(function() {
    isolate({
      users$count <- users$count - 1
    })
  })
  
  output$userstext = renderUI({
    if (users$count==1) {
      h4(paste0("There is only one user connected to this app"))
    } else {
      h4(paste0("There are ", users$count, " users connected to this app"))
    }
  })
  #YMaxReactive <- reactiveVal(get_ymax(input))
  
  # onSessionStart(function() {
  #   initialYMax <- get_ymax(input)  # Replace with your logic to get the initial value
  #   updateNumericInput(session, inputId = "YMaxCompareModels", value = initialYMax)
  # })
  
  observeEvent(input$FixedYMaxCompareModels, {
    req(input$SendingCountry)
    req(input$ReceivingCountry)
    req(input$MODEL1)
    req(input$MODEL2)
    if (input$FixedYMaxCompareModels) {
      shinyjs::enable('YMaxCompareModels')
      shinyjs::html("YMaxCompareModels-label",'<span style="color: black;">Max Y-axis value</span>')
      Ymaxenabled(TRUE)
      #if (length(input$YMaxCompareModels)==0) updateNumericInput(session, inputId="YMaxCompareModels", value = get_ymax(input))
      #YMaxReactive(input$YMaxCompareModels)
      #if (length(YMaxReactive())==0) YMaxReactive(get_ymax(input))
    } else {
      shinyjs::disable('YMaxCompareModels')
      shinyjs::html("YMaxCompareModels-label",'<span style="color: gray;">Max Y-axis value</span>')
      #YMaxReactive(get_ymax(input))
      
      Ymaxenabled(FALSE)
    }
    updateNumericInput(session, inputId="YMaxCompareModels", value = get_ymax(input))
  })
  
  output$aboutContent <- renderText({about_list})
  
  ModelMixingTable<-reactiveVal(initial_values)
  ModelMixedResults<-reactiveVal(mix_models(initial_values))
  
  server_show_tables(input, output)
  output$MixedModelTable <- renderDT({if (length(colnames(ModelMixedResults()))) {
    shiny::req(ModelMixedResults())
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
  
  
  observeEvent(input$ModelTableLoad, {
    req(input$ModelTableLoad$datapath)
    df <- try(read.xlsx(input$ModelTableLoad$datapath, sheet=1, colNames = TRUE, rowNames = FALSE, skipEmptyCols = FALSE, skipEmptyRows = FALSE),silent = TRUE)
    if (class(df)[1]=='try-error'){
      showModal(WrongModelTableFile(),session)  
      session$sendCustomMessage("upload_msg", "")
      session$sendCustomMessage("upload_txt", "")
      session$sendCustomMessage('hideProgressBar') 
      
    } else {
      rownames(df)<-df[,1]
      dim(df)
      df <- df[,-1]
      df <- df[1:length(Countries),1:length(Countries)]
      
      print('success')
      df<-df[,order(colnames(df))]
      df<-df[order(rownames(df)),]
      if ((any(!unlist(df)%in%c(letters[1:6],""))) ||
          (any(rownames(df)!=Countries)) ||
          (any(colnames(df)!=Countries))){
        showModal(WrongModelTableFile(),session)  
        session$sendCustomMessage("upload_msg", "")
        session$sendCustomMessage("upload_txt", "No file selected")
      } else {
        ModelMixingTable(df)
        ModelMixedResults(mix_models(df))
      }
    }
  })
  
  observeEvent(input$ModelTableRestore,{
    showModal(CleanModelTable_1(session),session)
  })
  
  observeEvent(input$ModelTableSolidSubmit,{
    req(input$ModelTableSolidInput)
    showModal(CleanModelTable_2(session, letters[as.integer(input$ModelTableSolidInput)]),session)
  })
  
  observeEvent(input$CleanModelTableYes2,{
    req(input$ModelTableSolidInput, ModelMixingTable())
    tmp<-initial_values
    cn<-colnames(tmp)
    rn<-rownames(tmp)
    dims<-dim(tmp)
    tmp<-letters[as.integer(input$ModelTableSolidInput)]
    tmp<-matrix(tmp, dims[1],dims[2])
    colnames(tmp)<-cn
    rownames(tmp)<-rn
    ModelMixingTable(tmp)
    ModelMixedResults(mix_models(ModelMixingTable()))
    removeModal(session = session)
    
  })
  
  observeEvent(input$CleanModelTableYes1,{
    
    ModelMixingTable(initial_values)
    ModelMixedResults(mix_models(initial_values))
    removeModal(session = session)
  })
  
  observeEvent(input$ModelTable,{
    tmp<-hot_to_r(input$ModelTable)
    colnames(tmp)<-Countries 
    rownames(tmp)<-Countries
    ModelMixingTable(tmp)  
    ModelMixedResults(mix_models(tmp))
  })
  
  output$ModelTable <- renderRHandsontable({
    
    
    tmp<-ModelMixingTable()
    for (i in 1:NCntr) tmp[i,i]<-''
    ModelMixingTable(tmp)
    rhandsontable(ModelMixingTable(), colWidths=32, rowHeaderWidth=32, colHeaderWidth=32, contextMenu = FALSE, height=747) %>% #overflow = 'visible' remove if more countries
      
      hot_cols(renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          var colors = [ '#FFE3A3', '#F7B6B6', '#FFB273', '#968EE4', '#C9C5BA', '#A6D1B6'];
          var index = value.charCodeAt(0) - 'a'.charCodeAt(0);
          var color = colors[index];
          td.style.backgroundColor = color;
          td.style.textAlign = 'center'; 
          if (row === col) {
           cellProperties.readOnly = true;
          }
        }
      ") %>%
      hot_col(col = 1:NCntr, type = "dropdown", source = c("a", "b", "c", "d", "e", "f"))
    
  })
  
  observeEvent(input$selectallsen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull)
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectallrec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull)
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectnonesen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull[0])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectnonerec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull[0])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectnordicsen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull[Countries%in%c('SE','NO','IS','DK','FI')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectnordicrec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull[Countries%in%c('SE','NO','IS','DK','FI')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectNsen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull[Countries%in%c('SE','NO','IS','DK','FI','IE','UK','LT','EE','LV')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectNrec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull[Countries%in%c('SE','NO','IS','DK','FI','IE','UK','LT','EE','LV')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectSsen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull[Countries%in%c('HR','CY','GR','MT','IT','PT','ES','SI')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectSrec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull[Countries%in%c('HR','CY','GR','MT','IT','PT','ES','SI')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectWsen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull[Countries%in%c('FR','DE','NL','BE','LU','CH','AT')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectWrec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull[Countries%in%c('FR','DE','NL','BE','LU','CH','AT')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectEsen,{
    updateAwesomeCheckbox(session,"SendCntrs",value=CountriesFull[Countries%in%c('CZ','PL','SK','HU','BG','RO')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  observeEvent(input$selectErec,{
    updateAwesomeCheckbox(session,"RecCntrs",value=CountriesFull[Countries%in%c('CZ','PL','SK','HU','BG','RO')])
    updateAwesomeCheckbox(session,"UseThreshold", value=FALSE)
  })
  
  
  
    # actionButton('selectallsen','All'),
  # actionButton('selectnonesen','None'),
  # actionButton('selectnonesen','Nordic'),
  # actionButton('selectNsen','+ N EU'),
  # actionButton('selectSsen','+ S EU'),
  # actionButton('selectWsen','+ W EU'),
  # actionButton('selectEsen','+ E EU'),
  # 
  server_download_tables(input, output)
  
  output$ModelTableDownload <- downloadHandler(
    filename = function() {
      paste("HMigD_model_mixing_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      #value<-googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")
      # Save the entire database connection object as an RDS file
      tmp <- data.frame(ModelMixingTable(), ' '='', check.names=FALSE, fix.empty.names =FALSE, stringsAsFactors = FALSE, check.rows = FALSE)
      write.xlsx(tmp, file, rowNames =TRUE, colNames =TRUE, tabColour ='#607080', colWidths=list(3.9))
    }
  )
  
  output$SaveModel2Data <- downloadHandler(
    filename = function() {
      paste("HMigD_aggregateflows_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {  
      write.xlsx(AggrSave(), file, rowNames =FALSE, colNames =TRUE, tabColour ='#607080')  
    }
  )
  
  output$SaveModel1Data <- downloadHandler(
    filename = function() {
      paste("HMigD_singleflows_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {  
      write.xlsx(SingleSave(), file, rowNames =FALSE, colNames =TRUE, tabColour ='#607080')  
    }
  )
  
  
  observeEvent( input$RecCntrs,{
    req(c(input$RecCntrs,input$SendCntrs, input$MODEL1b, input$MODEL2b))
    AggrSave(get_aggregated_(input))
  })
  
  observeEvent(input$SendCntrs,{
    req(c(input$RecCntrs,input$SendCntrs, input$MODEL1b, input$MODEL2b))
    AggrSave(get_aggregated_(input))
  })
  
  observeEvent( input$MODEL1b,{
    req(c(input$RecCntrs,input$SendCntrs, input$MODEL1b, input$MODEL2b))
    AggrSave(get_aggregated_(input))
  })
  
  observeEvent(input$MODEL2b,{
    req(c(input$RecCntrs,input$SendCntrs, input$MODEL1b, input$MODEL2b))
    AggrSave(get_aggregated_(input))
  })
  
  observeEvent(input$Examples1,{
    req(c(input$RecCntrs,input$SendCntrs, input$MODEL1b, input$MODEL2b))
    AggrSave(get_aggregated_(input))
  })
  
  
  output$SelectedModelTableDownload <- downloadHandler(
    filename = function() {
      paste("HMigD_raw_user_results_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      #value<-googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")
      # Save the entire database connection object as an RDS file
      tmp <- data.frame(ModelMixedResults(), ' '='', check.names=FALSE, fix.empty.names =FALSE, stringsAsFactors = FALSE, check.rows = FALSE)
      write.xlsx(tmp, file, rowNames =FALSE, colNames =TRUE, tabColour ='#607080', colWidths=list(7))
    }
  )
  
  observeEvent(c(input$ReceivingCountry,input$SendingCountry),{
    selectedl<-ModelMixedResultsDefault$model[(ModelMixedResultsDefault$dest==Countries[as.numeric(input$ReceivingCountry)]) & 
                                                (ModelMixedResultsDefault$orig==Countries[as.numeric(input$SendingCountry)])][1]
    selected<-which(letters==selectedl)
    output$cm_m12<-renderUI({
      tagList(
        br(),
        div(style = "display: inline-flex; align-items: center; width: 1200px; margin-left: 20px;",
            div(style = "width: 390px;",
                h3("Default recommended model #1")
            ),
            tags$head(tags$style(HTML("#MODEL1.form-control {padding-left: 6px;}"))),
            div(style = "width: 810px;",
                selectInput("MODEL1", 
                            label = NULL,           
                            choices = makeList(MODELS),
                            selectize = FALSE,
                            selected = selected,
                            width = '780px'
                )
            )
        ),
        div(style = "display: inline-flex; align-items: center; width: 1200px; margin-left: 20px;",
            div(style = "width: 390px;",
                h3("Optional model #2 for comparisons")
            ),
            tags$head(tags$style(HTML("#MODEL2.form-control {padding-left: 6px;}"))),
            div(style = "width: 810px;",
                selectInput(
                  "MODEL2", label = NULL,
                  choices = makeList(MODELS),
                  selectize = FALSE,
                  selected = selected,
                  width = '780px'
                )
            )
        ),
      )
    })
    if (!Ymaxenabled()) updateNumericInput(session, inputId="YMaxCompareModels", value = get_ymax(input))
    # observeEvent( input$ReceivingCountry,{
    #   req(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2))
    #   print('ReceivingCountry changed')
    #   print(paste(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2)))
    SingleSave(get_single_(input))
    #   print(names(SingleSave()))
    # })
    # 
    # observeEvent( input$SendingCountry,{
    #   req(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2))
    #   print('SendingCountry changed')
    #   print(paste(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2)))
    #   SingleSave(get_single_(input))
    #   print(names(SingleSave()))
    # })
    
  })
  
  # 
  # observeEvent(input$MODEL1,{
  #   req(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2))
  #   print('MODEL 1 changed')
  #   print(paste(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2)))
  #   SingleSave(get_single_(input))
  #   print(names(SingleSave()))
  # })
  # 
  # observeEvent(input$MODEL2,{
  #   req(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2))
  #   print('MODEL 2 changed')
  #   print(paste(c(input$ReceivingCountry,input$SendingCountry, input$MODEL1, input$MODEL2)))
  #   SingleSave(get_single_(input))
  #   print(names(SingleSave()))
  # })

  observeEvent(c(input$MODEL1,input$MODEL2), {
    if (!Ymaxenabled()) updateNumericInput(session, inputId="YMaxCompareModels", value = get_ymax(input))
    SingleSave(get_single_(input))
  })
  # observeEvent(c(input$MODEL_PANEL,input$MM1,input$MM2,input$MM3,input$MM4, input$reverse),{ # repair the shiny bug
  #   updateSelectInput(session, "MODEL3", label = NULL,
  #                     choices = makeList(MODELS),
  #                     selected = input$MODEL3)
  #   # updateSelectInput(session, "MODEL1b", label = NULL,
  #   #                   choices = makeList(MODELS),
  #   #                   selected = input$MODEL1b)
  #   # updateSelectInput(session, "MODEL2b", label = NULL,
  #   #                   choices = makeList(MODELS),
  #   #                   selected = input$MODEL2b)
  #   # # req(input$MODEL1, input$MODEL2)
  #   # updateSelectInput(session, "MODEL1", label = NULL,
  #   #                   choices = makeList(MODELS),
  #   #                   selected = input$MODEL1)
  #   # updateSelectInput(session, "MODEL2", label = NULL,
  #   #                   choices = makeList(MODELS),
  #   #                   selected = input$MODEL2)
  # })
  # 
  observeEvent(input$MODEL4, {
    shinyjs::disable("DownloadCode")
    if (input$MODEL4==1) {
      output$png_view1 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_ae.png")
      })
      output$png_view2 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_with_LFS.png")
      })
    } else if (input$MODEL4==2) {
      output$png_view1 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_bf.png")
      })
      output$png_view2 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_with_LFS.png")
      })
    } else if (input$MODEL4==3){
      output$png_view1 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_c.png")
      })
      output$png_view2 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_with_LFS.png")
      })
    } else if (input$MODEL4==4) {
      output$png_view1 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_d.png")
      })
      output$png_view2 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_with_LFS.png")
      })
    } else if (input$MODEL4==5) {
      output$png_view1 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_ae.png")
      })
      output$png_view2 <- renderUI({
        tags$img(style="height:auto; width:90%; display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_no_LFS.png")
      })
    } else if (input$MODEL4==6) {
      output$png_view1 <- renderUI({
        tags$img(style="height:auto; width:90%;display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_bf.png")
      })
      output$png_view2 <- renderUI({
        tags$img(style="height:auto; width:90%;display: block; margin-left: auto; margin-right: auto;", src="figurespath/Model_no_LFS.png")
      })
    }
  })
  
  insertResponse <- function() {
    write_survey_to_google(session, input)
    #rv$data <- dbGetQuery(con, "SELECT * FROM survey_responses")
    updateTextAreaInput(session, 'DBcomment', value='')
    DBsheets(googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments"))
  }
  
  eraser_password_value <- reactiveVal(FALSE)
  archive_password_value <- reactiveVal(FALSE)
  
  removeRows <- function() {
    if (!is.null(input$surveyTable_rows_selected) && length(input$surveyTable_rows_selected) > 0) {
      
      if (!eraser_password_value() ) {
        showModal(eraser_password_input(input$surveyTable_rows_selected, session=session), session=session)
      } else {
        withProgress(message = 'Calculation in progress',
                     style='notification',
                     detail = 'This may take a while...', value = 0,  {
                       
                       removeSelectedRows()
                       updateSelectInput(session, "surveyTable_rows_selected", selected = NULL)
                     })
      }
    }
  }
  
  removeSelectedRows <- function() {
    
    tryCatch({
      # Get row IDs selected in DT table
      isolate({
        tmp<-as.data.frame(DBsheets())
        tmp$id<-paste(tmp$hash,tmp$ip,tmp$time,tmp$name)
        selected_rows <- unlist(tmp[input$surveyTable_rows_selected, "id"])
        cat('Rows to remove:\n')
        print(selected_rows)
        values <- as.data.frame(googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments"))
        values$id<-paste(values$hash,values$ip,values$time,values$name)
        
        row_num<- sort(which(values$id %in% selected_rows) + 1, decreasing = TRUE)
        print(row_num)
        for (k in row_num){
          print(k)
          googlesheets4::range_delete(ss = sheet_id, sheet = "Comments", range=paste(k))
        }
      })
      
      DBsheets(googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments"))
      
    }, error = function(e) {
      showNotification("An error occurred while removing selected rows.", type = "error")
      print(e)
    })
    
    # reset the eraser password value
    eraser_password_value(FALSE)
  }
  
  observeEvent(input$submit2, {
    if (!length(input$eraser_password)) {
      eraser_password_value(FALSE) 
      validate(
        showNotification("Please enter password.", type = "warning")
      )
    } else if (input$eraser_password != eraser_password) {
      eraser_password_value(FALSE)
      validate(showNotification("Incorrect password.", type = "error"))
    } else {
      withProgress(message = 'Calculation in progress',
                   style='notification',
                   detail = 'This may take a while...', value = 0,  {
                     
                     eraser_password_value(TRUE)
                     removeSelectedRows()
                     
                     updateSelectInput(session, "surveyTable_rows_selected", selected = NULL)
                   })
      removeModal(session = session)
      
    }
  })
  
  # Inside the modal
  observeEvent(input$submit_showsurvey_password, {
    if (!length(input$showsurvey_password)) {
      archive_password_value(FALSE) 
      validate(
        showNotification("Please enter password.", type = "warning")
      )
    } else if (input$showsurvey_password != showsurvey_password) {
      archive_password_value(FALSE)
      validate(
        showNotification("Incorrect password.", type = "error")
      )
    } else {
      archive_password_value(TRUE)
      removeModal(session = session)
      updateActionButton(session = session, "showsurvey", label="Show all responses", icon=shiny::icon('unlock'))
    }
  })
  
  
  cDBname <- reactiveVal('black')
  cDBemail <- reactiveVal('black')
  cDBcomment <- reactiveVal('black')
  
  output$hDBname <- renderUI({
    h4('Please provide your name',style=paste0('color:',cDBname()))
  })
  
  output$hDBemail <- renderUI({
    h4('Please provide your emial or contact information',style=paste0('color:',cDBemail()))
  })
  
  output$hDBcomment <- renderUI({
    h4('Please provide your comment and press submit button below',style=paste0('color:',cDBcomment()))
  })
  
  
  # Submit survey response when button is clicked
  observeEvent(input$submit, {
    if (!length(input$DBname) || nchar(input$DBname)<=1) {
      cDBname('red')
      cDBemail('black')
      cDBcomment('black')
      validate(showNotification("Your name is too short. Please provide your name or alias.", type = "warning"))
      
    } else if (!length(input$DBemail) || nchar(input$DBemail)<=4){ 
      cDBname('black')
      cDBemail('red')
      cDBcomment('black')
      validate(showNotification("Your contact inforation is too short. Please provide your email or contact info.", type = "warning"))
    } else if (!length(input$DBcomment) || nchar(input$DBcomment)<=4){ 
      cDBname('black')
      cDBemail('black')
      cDBcomment('red')
      validate(showNotification("Your comment is too short. Please provide your comment.", type = "warning"))
      
    } else {
      cDBname('black')
      cDBemail('black')
      cDBcomment('black')
      #progress <- Progress$new(session, min = 0, max = 1, message = "Inserting response", style = "notification")
      
      withProgress(message = 'Calculation in progress',
                   style='notification',
                   detail = 'This may take a while...', value = 0,  {
                     insertResponse()
                   })
      showModal(modalDialog(h3("Thank you for your response!"),easyClose = TRUE)) 
    }
    output$hDBname <- renderUI({
      h4('Please provide your name',style=paste0('color:',cDBname()))
    })
    
    output$hDBemail <- renderUI({
      h4('Please provide your emial or contact information',style=paste0('color:',cDBemail()))
    })
    
    output$hDBcomment <- renderUI({
      h4('Please provide your comment and press submit button below',style=paste0('color:',cDBcomment()))
    })
    
  })
  
  observe({
    if (archive_password_value()) {
      updateTextInput(session, "showArchiveDiv", value = "true")  # show the div block
    } else {
      updateTextInput(session, "showArchiveDiv", value = "false")  # hide the div block
    }
  })
  
  observeEvent(input$showsurvey, {
    if (!archive_password_value() ) {
      showModal(showsurvey_password_input(session=session), session=session)
    }
  })
  
  
  # Remove selected rows when button pressed
  observeEvent(input$remove, {
    removeRows()
  })
  
  # Render data table of survey responses
  DBsheets <- reactiveVal(try(googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")))
  
  output$surveyTable <- renderDT({
    DBsheets(googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments"))
    dbdata<-DBsheets()[,c(3,4,5,6,7,11)]
    datatable(
      dbdata,
      options = list(
        searching = FALSE,
        pageLength = 1000,
        dom = 't',
        info = FALSE,
        #rownamesWidth = "20px",
        columnDefs = list(
          list(
            width = "1px",
            targets = 0:5
          ),
          list(
            width = "auto",
            targets = 6
          )
        )
      )
    )
  }, server = FALSE)
  
  # Download entire database as xlsx file
  output$downloadDataBase <- downloadHandler(
    filename = function() {
      paste("HMigD_comments_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      value<-googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")
      # Save the entire database connection object as an RDS file
      write.xlsx(value, file)
    }
  )
  
  
  # trick to avoid bug in the updateSelectInput code
  observeEvent(input$MODEL2b,{
    if(input$MODEL2b==2){
      updateSelectInput(session=session,
                        "MODEL2b", label = NULL,
                        choices = makeList(MODELS),
                        selected = 2)
    }  
  })
  # trick to avoid bug in the updateSelectInput code
  observeEvent(input$Examples1,{
    if(input$Examples1==4){
      
      updateSelectInput(session=session,
                        "Examples1", label = NULL,
                        choices = makeList(c('(0) None', FREEDOMCASES)),
                        selected = 4)
    }  
  })
  
  observe_helpers(withMathJax = TRUE, help_dir = 'helpfiles')
  
  observeEvent(input$EqualizeSending,{
    updateAwesomeCheckboxGroup(session = session, inputId="SendCntrs3" , selected = input$RecCntrs3)
  })
  
  observeEvent(input$EqualizeReceiving,{
    updateAwesomeCheckboxGroup(session = session, inputId="RecCntrs3" , selected = input$SendCntrs3)
  })
  
  
  observeEvent(input$SendCntrs,{
    if (!firstrunSen()) {
      if ((!justchanged())&&(!identical(OLD_send(),input$SendCntrs)))
        updateSelectInput(session = session, inputId = 'Examples1', selected = 1)
      justchanged(FALSE)
      
      OLD_send(input$SendCntrs)
      OLD_rec(input$RecCntrs)
    }
    firstrunSen(FALSE)  
  })
  
  observeEvent(input$RecCntrs,{
    if (!firstrunRec()) {
      if ((!justchanged())&&(!identical(OLD_rec(),input$RecCntrs)))
        updateSelectInput(session = session, inputId = 'Examples1', selected = 1)
      
      justchanged(FALSE)
      
      OLD_send(input$SendCntrs)
      OLD_rec(input$RecCntrs)
    }
    firstrunRec(FALSE)
  })
  
  observeEvent(input$Reverse,{
    RecC <- input$ReceivingCountry
    SenC <- input$SendingCountry
    updateSelectInput(session = session, inputId = "SendingCountry", selected = RecC)
    updateSelectInput(session = session, inputId = "ReceivingCountry", selected = SenC)
    
  })
  
  observeEvent(input$Examples1,{
    ww<-runexample1(session,input)
    print('***')
    print(ww)
    if (input$Examples1 >1) {
      ThresholdYear(ww)
      justchanged(TRUE)
      OLD_send(input$SendCntrs)
      OLD_rec(input$RecCntrs)
    } else justchanged(FALSE)
    print(ThresholdYear())
    print('***')
  })
  
  server_plot_figures(input, output)
  
  output$OutputFlowsPlot <- renderPlot({
    ModelMixedResultsArray<-xtabs(pred_q50 ~ orig + dest + year, data = ModelMixedResults())
    plot_output_flows_(input, ModelMixedResultsArray)
  }, height = 900, width = 1200, res=115)
  
  output$Model2Plot <- renderPlot({
    par(mar=c(5.1, 4.1, 3.0-2.4*!input$ShowTitleAgr, 16.0-15.4*!input$ShowLegendAgr))
    plot_aggregated_(input, TrYearV=ThresholdYear())
  }, height = 700, width = 1200, res=115)
  
  server_save_figures(input, output)
  
  output$SaveModel2Plot<- downloadHandler(
    filename = function() {
      paste('ModelComparison.', input$SaveModel2Format, sep='') },
    content = function(file) {
      if (input$SendingCountry!=input$ReceivingCountry) {
        RES2<-800
        ffo <- input$SaveModel2Format
        if(ffo=='pdf') {
          pdf(file,12,7)
        } else if(ffo=='png'){
          png(file,width=12*RES2,height=7*RES2,res=RES2)
        } else if(ffo=='tiff'){
          tiff(file,width=12*RES2,height=7*RES2,res=RES2,compression = 'rle')
        }
        
        par(mar=c(5.1, 4.1, 3.0-2.4*!input$ShowTitleAgr, 16.0-15.4*!input$ShowLegendAgr))
        # print('test1')
        # THRE<-input$ThrY - 1000*(1-as.numeric(input$UseThreshold))
        plot_aggregated_(input, ThresholdYear())
        dev.off()
      }
    }
  )
}  


PanelNames<-(c('About','Input flows','Input transitions (LFS)','Gravity covariates','Model schemes','Model estimates & comparison','Models mixing & download'))
# Spistresci <- data.frame(
#   main = c("Accuracy", "Accuracy","Accuracy","Undercounting","Undercounting","Undercounting", "Duration of stay", "Migration flows","Migration flows","Migration transitions"),
#   sub1 =   c("Administrative data", "Administrative data", "LFS Survey data","Administrative data", "Administrative data", "LFS Survey data", "Administrative data", "Administrative data", "Administrative data", "LFS Survey data"),
#   sub2 =  c("Data", "Metadata", "Data","Data", "Metadata", "Data", "Data", "Data", "Metadata", NA),
#   stringsAsFactors = FALSE
# )

shinyUI <-  bootstrapPage(
  
  useShinyjs(),
  tags$meta(charset = "UTF-8"), #24.03.2023
  setModalsStyle(), #24.03.2023
  # tags$head(
  #   tags$style(
  #     HTML(".ui-notification {
  #             bottom: 0;
  #             left: 0;
  #             top: auto;
  #             right: auto;
  #             width: auto;
  #             margin: 0.5em;
  #           }")
  #   )
  # ),
  tags$head(tags$style(HTML(".ht_master, .ht_clone_top, .ht_clone_left, .ht_clone_right, .ht_clone_bottom  {overflow: hidden !important;}"))),
  tags$head(tags$style(HTML(".irs-grid-text {font-size: 14px;}"))),
  tags$head(tags$style(HTML(".irs-min {display: none;}"))),
  tags$head(tags$style(HTML(".irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {font-size: 12px;}"))),
  tags$head(tags$style(HTML(".irs-max {display: none;}"))),
  tags$style(".shiny-file-input-progress {display: none}"),
  tags$style("input-group.form-control {visibility: hidden}"),
  tags$head(tags$style("body {min-width:100%; max-width: 100%; background-color: #FCFAF0}", media="screen", type="text/css")),
  theme = bs_theme(version = 3),
  #tags$style(".checkbox-bs-primary input[type='checkbox']:checked + label::before, .checkbox-bs-primary input[type='radio']:checked + label::before {background-color: #FFBB00; border-color: #FFBB00;}
  #            .checkbox-primary input[type='checkbox']:checked + label::before, .checkbox-primary input[type='radio']:checked + label::before {background-color: #FFBB00; border-color: #FFBB00;}"),
  
  tags$head(
    tags$style(
      HTML("
        .dt-custom-center {
          text-align: center !important;
          display: flex;
          justify-content: center;
          align-items: center;
        }
      ")
    )
  ),
  
  titlePanel(HTML('<span style="color:#000070;font-family:Serif,Georgia,Serif"><b>Human Migration Database I</b></span>'),'HMigD I App'),
  #fluidRow(style='max-width:1800px; min-width:1300px',
  div(class='row',style='width:5000px',
      column(width = 12,
             tags$head(tags$style("h3 {margin-top:0px;}", media="screen", type="text/css")),
             tags$head(tags$style("h4 {margin-top:0px;}", media="screen", type="text/css")),
             tags$head(tags$style("img {border:0px; border-color: #D5D5D5; border-style: solid;}", media="screen", type="text/css")),
             #tags$head(tags$style("tabbable {max-width:1800px; min-width:1100px}", media="screen", type="text/css")),
             #tags$head(tags$style("nav {max-width:1800px; min-width:1100px}", media="screen", type="text/css")),
             
             #tags$head(tags$style(".well {border:2px; border-color: #D5D5D5; border-style: solid; 
             #                      padding: 3px; background-color: #F5F5F5; margin:5px}", media="screen", type="text/css")), #margin-left: 10px; margin-bottom: 10px
             
             tags$style(HTML(paste("
                          .tabbable > .nav > li > a {background-color: #A0B179; border-color: #80A060;  color:#FFFFFF; font-size: 17px}

                          .tabbable > .nav > li[class=active] > a {background-color: #E8E6D9; border-color: #99A285; color:#4f6b4f}",sep=''))),
             br(),
             tabsetPanel(type='tabs',
                         tabPanel(title = PanelNames[1],
                                  br(),
                                  tags$div(class='row',
                                           style='font-size:16px;width:1050px; margin-left:75px;',
                                           about_list
                                  ),
                                  
                         ),         
                         
                         tabPanel(title = PanelNames[2],style='width:1200px;margin-left:5px',
                                  
                                  mysubmenu(ID="FlowsPanels",
                                            choiceNames = makebold(c("Migration flows","Accuracy","Undercounting","Coverage","Duration of stay")),
                                            space=3, 
                                            width=243*5+3,
                                            #width_panel=width/length(choiceNames)+space,
                                            width_panel = 243,
                                            passive.bg="#DCA564",
                                            passive.bo="#906010",
                                            passive.co="#FFFFFF",
                                            active.bg="#FFDFAB",
                                            active.bo="#9985A2",
                                            active.co="#6f5b3f",
                                            individual=TRUE,
                                            fontsize=16,
                                            height=43,
                                            top.padding=20
                                  ),
                                  
                                  conditionalPanel(condition = "input.FlowsPanels == 1",#style='width:1200px;margin-bottom:-1px',
                                                   
                                                   mysubmenu(ID="DATA_FLOW_PANELS",
                                                             choiceNames = makebold(c("Immigration data sources","Emigration data sources","Immigration data","Emigration data")),
                                                             space=3, 
                                                             width=304*4+2,
                                                             width_panel=304,
                                                             passive.bg="#B17970",
                                                             passive.bo="#A06060",
                                                             passive.co="#FFFFFF",
                                                             active.bg="#E6C9C8;",
                                                             active.bo="#A28589",
                                                             active.co="#6f4040",
                                                             individual=TRUE,
                                                             fontsize=16,
                                                             height=43,
                                                             top.padding=3
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(condition = "input.DATA_FLOW_PANELS == 1",
                                                                    ui_flow_sources_imm(),     
                                                                    
                                                   ),
                                                   conditionalPanel(condition = "input.DATA_FLOW_PANELS == 2",
                                                                    ui_flow_sources_emi(),
                                                   ),
                                                   conditionalPanel(condition = "input.DATA_FLOW_PANELS == 3",
                                                                    ui_flow_imm(),     
                                                                    
                                                   ),
                                                   conditionalPanel(condition = "input.DATA_FLOW_PANELS == 4",
                                                                    ui_flow_emi(),
                                                   ),
                                                   
                                  ),
                                  
                                  conditionalPanel(condition = "input.FlowsPanels == 2",
                                                   
                                                   mysubmenu(ID="ACCURACY_PANELS",
                                                             choiceNames = makebold(c("Immigration data",'Emigration data')), #"Immigration metadata"),#,"Emigration data", "Emigration metadata")),
                                                             space=3, 
                                                             width=608*2+2,
                                                             width_panel=608,
                                                             passive.bg="#B17970",
                                                             passive.bo="#A06060",
                                                             passive.co="#FFFFFF",
                                                             active.bg="#E6C9C8;",
                                                             active.bo="#A28589",
                                                             active.co="#6f4040",
                                                             individual=TRUE,
                                                             fontsize=16,
                                                             height=43,
                                                             top.padding=3
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.ACCURACY_PANELS == 1",
                                                                    ui_flow_accuracy_imm(),
                                                                    
                                                   ),
                                                   conditionalPanel(condition = "input.ACCURACY_PANELS == 2",
                                                                    ui_flow_accuracy_emi(),
                                                                    
                                                   ),
                                                   
                                                   
                                  ),
                                  
                                  conditionalPanel(condition = "input.FlowsPanels == 3",
                                                   
                                                   mysubmenu(ID="UNDERCOUNT_PANELS",
                                                             choiceNames = makebold(c("Immigration data","Emigration data")),
                                                             space=3, 
                                                             width=405*3+3,
                                                             width_panel=405,
                                                             passive.bg="#B17970",
                                                             passive.bo="#A06060",
                                                             passive.co="#FFFFFF",
                                                             active.bg="#E6C9C8;",
                                                             active.bo="#A28589",
                                                             active.co="#6f4040",
                                                             individual=TRUE,
                                                             fontsize=16,
                                                             height=43,
                                                             top.padding=3
                                                             
                                                   ),
                                                   
                                                   conditionalPanel(condition = "input.UNDERCOUNT_PANELS == 1",
                                                                    ui_flow_undercounting_imm(),
                                                                    
                                                   ),
                                                   conditionalPanel(condition = "input.UNDERCOUNT_PANELS == 2",
                                                                    
                                                                    ui_flow_undercounting_emi(),
                                                                    
                                                   ),
                                                   
                                                   
                                                   
                                                   
                                  ),
                                  
                                  conditionalPanel(condition = "input.FlowsPanels == 4",
                                                   ui_flow_coverage(),
                                  ),
                                  
                                  conditionalPanel(condition = "input.FlowsPanels == 5",
                                                   
                                                   mysubmenu(ID="DURATION_PANELS",
                                                             choiceNames = makebold(c("Immigration data","Emigration data")),
                                                             space=3, 
                                                             width=608*2+2,
                                                             width_panel=608,
                                                             passive.bg="#B17970",
                                                             passive.bo="#A06060",
                                                             passive.co="#FFFFFF",
                                                             active.bg="#E6C9C8;",
                                                             active.bo="#A28589",
                                                             active.co="#6f4040",
                                                             individual=TRUE,
                                                             fontsize=16,
                                                             height=43,
                                                             top.padding=3
                                                   ),
                                                   conditionalPanel(condition = "input.DURATION_PANELS == 1",
                                                                    ui_flow_duration_imm(),
                                                   ),
                                                   conditionalPanel(condition = "input.DURATION_PANELS == 2",
                                                                    ui_flow_duration_emi(),
                                                                    
                                                   ),
                                                   conditionalPanel(condition = "input.DURATION_PANELS == 3",
                                                                    SaveBlock1('DURATION_META',5,'#F5DFD5'),
                                                                    
                                                   ),
                                                   
                                  ),
                         ),
                         
                         
                         tabPanel(title = PanelNames[3], style='max-width:1200px;margin-left:5px',
                                  mysubmenu(ID="TransitionsPanels",
                                            choiceNames = makebold(c("Migration transitions","Accuracy","Undercounting","Coverage")),
                                            space=3, 
                                            width=304*4+2,
                                            #width_panel=width/length(choiceNames)+space,
                                            width_panel = 304,
                                            passive.bg="#DCA564",
                                            passive.bo="#906010",
                                            passive.co="#FFFFFF",
                                            active.bg="#FFDFAB",
                                            active.bo="#9985A2",
                                            active.co="#6f5b3f",
                                            individual=TRUE,
                                            fontsize=16,
                                            height=43,
                                            top.padding=20
                                  ),
                                  
                                  conditionalPanel(condition = "input.TransitionsPanels == 1",
                                                   ui_transitions_count(),
                                  ),
                                  conditionalPanel(condition = "input.TransitionsPanels == 2",
                                                   ui_transition_accuracy()
                                  ),
                                  conditionalPanel(condition = "input.TransitionsPanels == 3",
                                                   ui_transition_undercounting(),
                                  ),
                                  conditionalPanel(condition = "input.TransitionsPanels == 4",
                                                   ui_transition_coverage(),
                                  ),
                                  #br(),
                                  
                                  
                         ),
                         tabPanel(title = PanelNames[4],style='width:1200px;margin-left:5px',
                                  
                                  mysubmenu(ID="GRAVITY_PANELS",
                                            choiceNames = makebold(c("Freedom of movement","Population size", "Migration stocks","Trade","GNI ratio","Language","Geographic distance")),
                                            space=3, 
                                            width=174*6-3,
                                            width_panel=174,
                                            passive.bg="#DCA564",
                                            passive.bo="#906010",
                                            passive.co="#FFFFFF",
                                            active.bg="#FFDFAB",
                                            active.bo="#9985A2",
                                            active.co="#6f5b3f",
                                            individual=TRUE,
                                            fontsize=14,
                                            height=43,
                                            top.padding=20
                                  ),
                                  
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 1",
                                                   
                                                   ui_covariates_freedom(),                                                                  
                                  ),
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 2",
                                                   
                                                   ui_covariates_population(),
                                  ),
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 3",
                                                   
                                                   ui_covariates_stocks()
                                  ),
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 4",
                                                   ui_covariates_trade()
                                                   
                                  ),
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 5",
                                                   ui_covariates_gni_ratio(),
                                                   
                                  ),
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 6",
                                                   ui_covariates_language(),
                                                   
                                  ),
                                  conditionalPanel(condition = "input.GRAVITY_PANELS == 7",
                                                   
                                                   ui_covariates_distance()
                                  ),
                         ),
                         tabPanel(title = PanelNames[5], style='max-width:1200px;margin-left:5px',
                                  ui_model_schemes(),
                         ),
                         tabPanel(title = PanelNames[6], style='max-width:1200px;margin-left:5px',
                                  mysubmenu(ID="MODEL_PANEL",
                                            choiceNames = makebold(c("Compare single flows","Compare aggregated flows","Circular plots of estimated flows")),
                                            space=3, 
                                            width=405*3+3,
                                            #width_panel=width/length(choiceNames)+space,
                                            width_panel = 405,
                                            passive.bg="#DCA564",
                                            passive.bo="#906010",
                                            passive.co="#FFFFFF",
                                            active.bg="#FFDFAB",
                                            active.bo="#9985A2",
                                            active.co="#6f5b3f",
                                            individual=TRUE,
                                            fontsize=16,
                                            height=43,
                                            top.padding=20
                                  ),
                                  conditionalPanel(condition = "input.MODEL_PANEL == 1", style='width:1200px',
                                                   ui_compare_models_single()                                                     ,
                                                   
                                  ),
                                  conditionalPanel(condition = "input.MODEL_PANEL == 2", style='width:1200px',  
                                                   ui_compare_models_aggregated(),
                                  ),
                                  
                                  conditionalPanel(condition = "input.MODEL_PANEL == 3", style='width:1200px',  
                                                   ui_compare_models_circular()
                                  ),
                                  
                         ),         
                         tabPanel(title = PanelNames[7],style='max-width:1200px;margin-left:5px',
                                  mysubmenu(ID="DOWNLOAD_PANEL",
                                            choiceNames = makebold(c("Fast download","Alternative model mixing","Visualization")),
                                            space=3, 
                                            width=405*3+3,
                                            #width_panel=width/length(choiceNames)+space,
                                            width_panel = 405,
                                            passive.bg="#DCA564",
                                            passive.bo="#906010",
                                            passive.co="#FFFFFF",
                                            active.bg="#FFDFAB",
                                            active.bo="#9985A2",
                                            active.co="#6f5b3f",
                                            individual=TRUE,
                                            fontsize=16,
                                            height=43,
                                            top.padding=20
                                  ),
                                  conditionalPanel(condition = "input.DOWNLOAD_PANEL == 1", style='width:1200px',
                                                   ModelsOutputFrame(DTid="MixedModelDefaultTable",BTNid='SelectedModelDefaultTableDownload',
                                                                     'Estimated migratioon flows for default selection of models'),
                                                   
                                  ),
                                  conditionalPanel(condition = "input.DOWNLOAD_PANEL == 2", style='width:1200px',  
                                                   ui_output(),
                                  ),
                                  
                                  conditionalPanel(condition = "input.DOWNLOAD_PANEL == 3", style='width:1200px',  
                                                   ui_visualize_output(),
                                  ),
                                  
                                  
                                  
                         ),
                         
             ),
             div(class="row", style='margin-left:5px; font-size:16px; width:1215px; margin-top:5px; background-color:#000088; border-style: solid; border-color:#000000; border-width:2px; color:#EEEEFF',
                 
                 div( style='margin-left:20px; margin-top:10px; display: flex; align-items: center; align-vertical: center', uiOutput("userstext")),
                 
                 
             ),
             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      
  ),
  
)

shinyApp(ui=shinyUI, server = shinyServer)
#rsconnect::deployApp()
