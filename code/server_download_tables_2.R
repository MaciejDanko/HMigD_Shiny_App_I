write.hmigd.xlsx<-function(filename, hmigddata){
  wb <- openxlsx::createWorkbook(filename)
  headerStyle <- openxlsx::createStyle(
    fontSize = 10, fontColour = "#000000", halign = "center", valign='center',
    fgFill = "#ffe1aa", border = "bottom", textDecoration ='bold'
  )
  bodyStyle1 <- openxlsx::createStyle(fontName='Arial', fontSize = 10, halign = "justify", border = "bottom",valign='center', fgFill = "#cfefff")
  bodyStyle2 <- openxlsx::createStyle(fontName='Arial', fontSize = 10, halign = "justify", border = "bottom", valign='center', fgFill = '#efffcf')
  bodyStyle3 <- openxlsx::createStyle(fontName='Arial', fontSize = 10, halign = "center", border = "bottom", valign='center', fgFill = "#efefff")
  bodyStyle4 <- openxlsx::createStyle(fontName='Arial', fontSize = 10, halign = "center", border = "bottom", valign='center', fgFill = '#efdfdf')
  bodyStyle5 <- openxlsx::createStyle(fontName='Arial', fontSize = 10, halign = "left", border = "bottom", valign='center', fgFill = "#ffffef")
  bodyStyle6 <- openxlsx::createStyle(fontName='Arial', fontSize = 10, halign = "left", border = "bottom", valign='center', fgFill = '#dfefdf')
  
  openxlsx::addWorksheet(wb, "HMigD full data")
  openxlsx::addWorksheet(wb, "HMigD variables info")
  openxlsx::addWorksheet(wb, "AccuracyAdm DataQualityCodes")
  openxlsx::addWorksheet(wb, "AccuracyAdm ActionCodes")
  
  openxlsx::writeData(wb, sheet = 1, hmigddata, rowNames = FALSE)
  openxlsx::addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:ncol(hmigddata), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 1, bodyStyle3, rows = seq(2,nrow(hmigddata)+1,2), cols = 1:ncol(hmigddata), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 1, bodyStyle4, rows = seq(3,nrow(hmigddata)+1,2), cols = 1:ncol(hmigddata), gridExpand = TRUE)
  max_lengths <- sapply(hmigddata, function(x) max(nchar(paste(x)),na.rm = TRUE))
  max_lengths <- mapply(max, max_lengths, nchar(colnames(hmigddata)))
  openxlsx::setColWidths(wb, sheet=1, cols = 1:ncol(hmigddata), widths = max_lengths)
  openxlsx::freezePane(wb, sheet=1, firstActiveRow = 2, firstActiveCol=4)
  
  openxlsx::writeData(wb, sheet = 2, HMigD_COLUMNS, rowNames = FALSE)
  openxlsx::addStyle(wb, sheet = 2, headerStyle, rows = 1, cols = 1:ncol(HMigD_COLUMNS), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 2, bodyStyle5, rows = seq(2,nrow(HMigD_COLUMNS)+1,2), cols = 1:ncol(HMigD_COLUMNS), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 2, bodyStyle6, rows = seq(3,nrow(HMigD_COLUMNS)+1,2), cols = 1:ncol(HMigD_COLUMNS), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet = 2, 1:2, c(50,150))#HMigD_COLUMNS_widths)
  openxlsx::freezePane(wb, sheet=2, firstRow = TRUE)
  
  openxlsx::writeData(wb, sheet = 3, ACCURACY_ADM_Comments$data, rowNames = FALSE)
  openxlsx::addStyle(wb, sheet = 3, headerStyle, rows = 1, cols = 1:ncol(ACCURACY_ADM_Comments$data), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 3, bodyStyle1, rows = seq(2,nrow(ACCURACY_ADM_Comments$data)+1,2), cols = 1:ncol(ACCURACY_ADM_Comments$data), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 3, bodyStyle2, rows = seq(3,nrow(ACCURACY_ADM_Comments$data)+1,2), cols = 1:ncol(ACCURACY_ADM_Comments$data), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 3,  headerStyle , rows=1:(nrow(ACCURACY_ADM_Comments$data)+1),cols=1)
  openxlsx::setRowHeights(wb, sheet = 3, 1:(nrow(ACCURACY_ADM_Comments$data)+1), ACCURACY_ADM_Comments$heights)
  openxlsx::setColWidths(wb, sheet = 3, 1:(ncol(ACCURACY_ADM_Comments$data)),ACCURACY_ADM_Comments$widths)
  openxlsx::freezePane(wb, sheet=3, firstRow = TRUE)
  
  openxlsx::writeData(wb, sheet = 4, ACCURACY_ADM_Action$data, rowNames = FALSE)
  openxlsx::addStyle(wb, sheet = 4, headerStyle, rows = 1, cols = 1:ncol(ACCURACY_ADM_Action$data), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 4, bodyStyle1, rows = seq(2,nrow(ACCURACY_ADM_Action$data)+1,2), cols = 1:ncol(ACCURACY_ADM_Action$data), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 4, bodyStyle2, rows = seq(3,nrow(ACCURACY_ADM_Action$data)+1,2), cols = 1:ncol(ACCURACY_ADM_Action$data), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 4, headerStyle, rows=1:(nrow(ACCURACY_ADM_Action$data)+1),cols=1)
  openxlsx::setRowHeights(wb, sheet = 4, 1:(nrow(ACCURACY_ADM_Action$data)+1), ACCURACY_ADM_Action$heights)
  openxlsx::setColWidths(wb, sheet = 4, 1:(ncol(ACCURACY_ADM_Action$data)),ACCURACY_ADM_Action$widths)
  openxlsx::freezePane(wb, sheet=4, firstRow = TRUE)
  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

Save3DArray<-function(file.name, array_data){
  print(file.name)
  print(dim(array_data))
  workbook <- createWorkbook()
  dnam<-dimnames(array_data)
  for (z in 1:dim(array_data)[3]) {
    z_slice <- array_data[, , z]
    worksheet_name <- dnam[[3]][z]
    addWorksheet(workbook, sheetName = worksheet_name)
    writeData(workbook, sheet = worksheet_name, x = z_slice, rowNames = TRUE, colNames = TRUE)
  }
  saveWorkbook(workbook, file = file.name, overwrite = TRUE)
}

Save3DTwoArrays<-function(file.name, array_data1, array_data2, pref1, pref2){
  print(file.name)
  print(dim(array_data1))
  print(dim(array_data2))
  workbook <- createWorkbook()
  dnam1<-dimnames(array_data1)
  dnam2<-dimnames(array_data2)
  for (z in 1:dim(array_data1)[3]) {
    z_slice1 <- array_data1[, , z]
    worksheet_name1 <- paste0(pref1,dnam1[[3]][z])
    addWorksheet(workbook, sheetName = worksheet_name1)
    writeData(workbook, sheet = worksheet_name1, x = z_slice1, rowNames = TRUE, colNames = TRUE)
  }
  for (z in 1:dim(array_data2)[3]) {
    z_slice2 <- array_data2[, , z]
    worksheet_name2 <- paste0(pref2,dnam2[[3]][z])
    addWorksheet(workbook, sheetName = worksheet_name2)
    writeData(workbook, sheet = worksheet_name2, x = z_slice2, rowNames = TRUE, colNames = TRUE)
  }
  saveWorkbook(workbook, file = file.name, overwrite = TRUE)
}

SaveNamedList<-function(file.name, mylist){
  workbook <- createWorkbook()
  nm<-names(mylist)
  for (z in seq_along(nm)) {
    addWorksheet(workbook, sheetName = nm[z])
    writeData(workbook, sheet = nm[z], x = mylist[[z]], rowNames = TRUE, colNames = TRUE)
  }
  saveWorkbook(workbook, file = file.name, overwrite = TRUE)
}


server_download_tables<-function(input, output){
    
  output$SaveSRC_EMIDataSrc <- downloadHandler(
    filename = function() {
      paste("Source_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DArray(file, rsm.data$x.emi.src)
    }
  )
  
  output$SaveSRC_IMMDataSrc <- downloadHandler(
    filename = function() {
      paste("Source_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DArray(file, rsm.data$x.imm.src)
    }
  )
  
  output$SaveACCURACY_EMIData <- downloadHandler(
    filename = function() {
      paste("Accuracy_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list(adjusted=FormatAccu(rsm.data_updated$A_E.r,'E'), unadjusted = FormatAccu(rsm.data$A_E.r,'E')))
    }
  )
  
  output$SaveACCURACY_IMMData <- downloadHandler(
    filename = function() {
      paste("Accuracy_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list(adjusted=FormatAccu(rsm.data_updated$A_I.r,'I'), unadjusted = FormatAccu(rsm.data$A_I.r,'I')))
    }
  )
  
  output$SaveUNDERCOUNT_EMIData <- downloadHandler(
    filename = function() {
      paste("Undercounting_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list(undercounting=FormatUnder(rsm.data$U4_E.r,'E')))
    }
  )
  
  output$SaveUNDERCOUNT_IMMData <- downloadHandler(
    filename = function() {
      paste("Undercounting_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list(undercounting=FormatUnder(rsm.data$U4_I.r,'I')))
    }
  )
  
  output$SavePOPULATIONData <- downloadHandler(
    filename = function() {
      paste("Population_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      #SaveNamedList(file, list('log-centered'=rsm.data$log_pop, raw=rsm.data$grav_pop))
      SaveNamedList(file, list('population'=rsm.data$p_midy))
    }
  )
  
  # output$SaveDISTANCEData <- downloadHandler(
  #   filename = function() {
  #     paste("Distance_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     SaveNamedList(file, list('log-centered'=rsm.data$log_distwces, raw=rsm.data$grav_dist))
  #   }
  # )
  # 
  # output$SaveSTOCKSData <- downloadHandler(
  #   filename = function() {
  #     paste("Stocks_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     SaveNamedList(file, list('log-centered'=rsm.data$log_msto_simple, raw=rsm.data$grav_stocks_simple))
  #   }
  # )
  # 
  # output$SaveLANGUAGEData <- downloadHandler(
  #   filename = function() {
  #     paste("Language_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     SaveNamedList(file, list('log-centered'=rsm.data$log_cl, raw=rsm.data$grav_lang))
  #   }
  # )
  # 
  # output$SaveTRADEData <- downloadHandler(
  #   filename = function() {
  #     paste("Trade_table_",input$trade_year,'_', format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     Save3DTwoArrays(file, rsm.data$log_trade, rsm.data$grav_trade, 'log-centered_','raw_')
  #   }
  # )
  # 
  # output$SaveGNIData <- downloadHandler(
  #   filename = function() {
  #     paste("GNI_ratio_table_",input$gni_year,'_', format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     Save3DTwoArrays(file, rsm.data$grav_log_gni_ratio, rsm.data$grav_gni_ratio, 'log-centered_','raw_')
  #   }
  # )
  
  
  output$SaveDURATION_IMMData <- downloadHandler(
    filename = function() {
      paste("Duration_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      tmp<-dI[,paste(Years)]
      tmp[tmp=='-1']<-''
      SaveNamedList(file, list(durationreceiving=tmp))
    }
  )
  
  output$SaveDURATION_EMIData <- downloadHandler(
    filename = function() {
      paste("Duration_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      tmp<-dE[,paste(Years)]
      tmp[tmp=='-1']<-''
      SaveNamedList(file, list(durationsending=tmp))
    }
  )
  
  output$CoverageTableDownload <- downloadHandler(
    filename = function() {
      paste("Covarage_flows_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(DTCoverage, file, rowNames =FALSE, colNames =TRUE)
    }
  )
  
  output$CoverageTransitionsTableDownload <- downloadHandler(
    filename = function() {
      paste("Covarage_transitions_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(DTCoverageTr, file, rowNames =FALSE, colNames =TRUE)
    }
  )
  
  output$UndercountingTransitionsTableDownload <- downloadHandler(
    filename = function() {
      paste("Undercounting_transitions_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(DTUndercountingTr, file, rowNames =FALSE, colNames =TRUE)
    }
  )
  
  output$AccuracyTransitionsTableDownload <- downloadHandler(
    filename = function() {
      paste("Accuracy_transitions_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(DTAccuracyTr, file, rowNames =FALSE, colNames =TRUE)
    }
  )
  
  # observe({
  #   shinyjs::enable("SelectedModelDefaultTableDownload")
  # })
  # 
  # observeEvent(input$dSelectedModelDefaultTableDownload, {
  #   cat('SelectedModelDefaultTableDownload triger\n')
  #   shinyjs::disable("SelectedModelDefaultTableDownload")
  #   session$sendCustomMessage("SelectedModelDefaultTableDownload", list(fileName = paste("HMigD_raw_results_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")))
  #   cat('SelectedModelDefaultTableDownload triger E\n')
  # })
  
  output$SelectedModelDefaultTableDownload <- downloadHandler(
    filename = function() {
      paste("HMigD_raw_results_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      #value<-googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")
      # Save the entire database connection object as an RDS file
      #tmp <- data.frame(ModelMixedResultsDefault, ' '='', check.names=FALSE, fix.empty.names =FALSE, stringsAsFactors = FALSE, check.rows = FALSE)
      
      shinyjs::disable("SelectedModelDefaultTableDownload")
      showModal(modalDialog(HTML('<b>Downloading...</b>'),footer=NULL, size='m'))
      write.hmigd.xlsx(file, ModelMixedResultsDefault)
      Sys.sleep(0.5)
      removeModal()
      shinyjs::enable("SelectedModelDefaultTableDownload")
      
      #write.hmigd.xlsx(tmp, file,  ames =FALSE, colNames =TRUE, tabColour ='#607080', colWidths=list(7), keepNA=TRUE)
    }
  )
  
  # server_password_btn(input, output, session, 'SaveFLOWSIMMCOUNTDataRaw', pas=data_download_password,
  #                     filenameFunc = function() {
  #                       paste("Flows_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #                     },
  #                     contentFunc = function(file) {
  #                       Save3DArray(file, rsm.data$x.imm)
  #                     }
  # )
  # 
  # server_password_btn(input, output, session, 'SaveFLOWSEMICOUNTDataRaw', pas=data_download_password,
  #                     filenameFunc = function() {
  #                       paste("Flows_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #                     },
  #                     contentFunc = function(file) {
  #                       Save3DArray(file, rsm.data$x.emi)
  #                     }
  # )
  # 
  # server_password_btn(input, output, session, 'SaveTRANSITIONSCOUNTDataRaw', pas=data_download_password,
  #                     filenameFunc = function() {
  #                       paste("TransitionsLFS_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
  #                     },
  #                     contentFunc = function(file) {
  #                       Save3DArray(file, rsm.data$k.imm)
  #                     }
  # )
  output$SaveFLOWSIMMCOUNTDataRaw <- downloadHandler( #server_password_btn(input, output, session, 'SaveFLOWSIMMCOUNTDataRaw', pas=data_download_password,
    filename = function() {
      paste("Flows_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DArray(file, rsm.data$x.imm)
    }
  )
  
  output$SaveFLOWSEMICOUNTDataRaw <- downloadHandler( #server_password_btn(input, output, session, 'SaveFLOWSEMICOUNTDataRaw', pas=data_download_password,
    filename = function() {
      paste("Flows_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DArray(file, rsm.data$x.emi)
    }
  )
  
  output$SaveTRANSITIONSCOUNTDataRaw <- downloadHandler( #server_password_btn(input, output, session, 'SaveTRANSITIONSCOUNTDataRaw', pas=data_download_password,
    filename = function() {
      paste("TransitionsLFS_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DArray(file, rsm.data$k.imm)
    }
  )
  
  output$SaveFREEDOMData <- downloadHandler(
    filename = function() {
      paste("Freedom_of_movement_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DArray(file, rsm.data$acceu)
    }
  )
  
  output
  

}