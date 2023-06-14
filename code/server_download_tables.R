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
      SaveNamedList(file, list(undercounting=FormatAccu(rsm.data$U4_E.r,'E')))
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
      SaveNamedList(file, list('log-centered'=rsm.data$log_pop, raw=rsm.data$grav_pop))
    }
  )
  
  output$SaveDISTANCEData <- downloadHandler(
    filename = function() {
      paste("Distance_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list('log-centered'=rsm.data$log_distwces, raw=rsm.data$grav_dist))
    }
  )
  
  output$SaveSTOCKSData <- downloadHandler(
    filename = function() {
      paste("Stocks_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list('log-centered'=rsm.data$log_msto_simple, raw=rsm.data$grav_stocks_simple))
    }
  )
  
  output$SaveLANGUAGEData <- downloadHandler(
    filename = function() {
      paste("Language_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      SaveNamedList(file, list('log-centered'=rsm.data$log_cl, raw=rsm.data$grav_lang))
    }
  )
  
  output$SaveTRADEData <- downloadHandler(
    filename = function() {
      paste("Trade_table_",input$trade_year,'_', format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DTwoArrays(file, rsm.data$log_trade, rsm.data$grav_trade, 'log-centered_','raw_')
    }
  )
  
  output$SaveGNIData <- downloadHandler(
    filename = function() {
      paste("GNI_ratio_table_",input$gni_year,'_', format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      Save3DTwoArrays(file, rsm.data$grav_log_gni_ratio, rsm.data$grav_gni_ratio, 'log-centered_','raw_')
    }
  )
  
  
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
  
  output$SelectedModelDefaultTableDownload <- downloadHandler(
    filename = function() {
      paste("HMigD_raw_results_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      #value<-googlesheets4::read_sheet(ss = sheet_id,  sheet = "Comments")
      # Save the entire database connection object as an RDS file
      tmp <- data.frame(ModelMixedResultsDefault, ' '='', check.names=FALSE, fix.empty.names =FALSE, stringsAsFactors = FALSE, check.rows = FALSE)
      write.xlsx(tmp, file, rowNames =FALSE, colNames =TRUE, tabColour ='#607080', colWidths=list(7))
    }
  )
  
  server_password_btn(input, output, session, 'SaveFLOWSIMMCOUNTDataRaw', pas=data_download_password,
                      filenameFunc = function() {
                        paste("Flows_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
                      },
                      contentFunc = function(file) {
                        Save3DArray(file, rsm.data$x.imm)
                      }
  )
  
  server_password_btn(input, output, session, 'SaveFLOWSEMICOUNTDataRaw', pas=data_download_password,
                      filenameFunc = function() {
                        paste("Flows_emi_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
                      },
                      contentFunc = function(file) {
                        Save3DArray(file, rsm.data$x.emi)
                      }
  )
  
  server_password_btn(input, output, session, 'SaveTRANSITIONSCOUNTDataRaw', pas=data_download_password,
                      filenameFunc = function() {
                        paste("TransitionsLFS_imm_table_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx", sep = "")
                      },
                      contentFunc = function(file) {
                        Save3DArray(file, rsm.data$k.imm)
                      }
  )
  
  output
  
}