server_save_figures <- function(input, output, server){
  output$SaveFREEDOMPlot<- downloadHandler(
    filename = function() {
      paste('FreedomOfMovementOfWorkers.', input$SaveFREEDOMFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveFREEDOMFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_freedom_(input)
      dev.off()
      #}
    }
  )
  
  output$SavePOPULATIONPlot<- downloadHandler(
    filename = function() {
      paste0('PopulationSize',c('.','_log-centered.')[input$log_pop+1], input$SavePOPULATIONFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SavePOPULATIONFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,12))
      plot_pop_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveTRADEPlot<- downloadHandler(
    filename = function() {
      paste0('Trade_',input$trade_year ,c('.','_log-centered.')[input$log_trade+1], input$SaveTRADEFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      ffo <- input$SaveTRADEFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,12))
      plot_trade_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveGNIPlot<- downloadHandler(
    filename = function() {
      paste0('GNI_Ratio_',input$gni_year ,c('.','_log-centered.')[input$log_gni+1], input$SaveGNIFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      ffo <- input$SaveGNIFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,12))
      plot_gni_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveDISTANCEPlot<- downloadHandler(
    filename = function() {
      paste0('Distance',c('.','_log-centered.')[input$log_dist+1], input$SaveDISTANCEFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveDISTANCEFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,12))
      plot_dist_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveLANGUAGEPlot<- downloadHandler(
    filename = function() {
      paste0('Language',c('.','_log-centered.')[input$log_lang+1], input$SaveLANGUAGEFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveLANGUAGEFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,12))
      plot_lang_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveSTOCKSPlot<- downloadHandler(
    filename = function() {
      paste0('Stocks',c('.','_log-centered.')[input$log_stocks+1], input$SaveSTOCKSFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveSTOCKSFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,12))
      plot_stocks_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveACCURACY_EMIPlot<- downloadHandler(
    filename = function() {
      paste('Accuracy_Emi.', input$SaveACCURACY_EMIFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveACCURACY_EMIFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_acu_emi_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveUNDERCOUNT_EMIPlot<- downloadHandler(
    filename = function() {
      paste('Undercounting_Emi.', input$SaveUNDERCOUNT_EMIFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveUNDERCOUNT_EMIFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_under_emi_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveDURATION_EMIPlot<- downloadHandler(
    filename = function() {
      paste('Undercounting_Emi.', input$SaveDURATION_EMIFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveDURATION_EMIFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_dur_emi_(input)
      dev.off()
      #}
    }
  )
  
  
  output$SaveACCURACY_IMMPlot<- downloadHandler(
    filename = function() {
      paste('Accuracy_Imm.', input$SaveACCURACY_IMMFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveACCURACY_IMMFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_acu_imm_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveUNDERCOUNT_IMMPlot<- downloadHandler(
    filename = function() {
      paste('Undercounting_Imm.', input$SaveUNDERCOUNT_IMMFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveUNDERCOUNT_IMMFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_under_imm_(input)
      dev.off()
      #}
    }
  )
  
  output$SaveDURATION_IMMPlot<- downloadHandler(
    filename = function() {
      paste('Undercounting_Imm.', input$SaveDURATION_IMMFormat, sep='') },
    content = function(file) {
      #if (input$SendingCountry!=input$ReceivingCountry) {
      
      ffo <- input$SaveDURATION_IMMFormat
      if(ffo=='pdf') {
        pdf(file,12,9)
      } else if(ffo=='png'){
        png(file,width=12*600,height=9*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=12*600,height=9*600,res=600,compression = 'rle')
      }
      par(mar=c(4,4,1.5,10))
      plot_dur_imm_(input)
      dev.off()
      #}
    }
  )
  
  
  output$SaveModel1Plot<- downloadHandler(
    
    filename = function() {
      paste("HMigD_ModelComparison_from_",Countries[as.numeric(input$ReceivingCountry)],'_to_',Countries[as.numeric(input$SendingCountry)],'_',format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),'.', input$SaveModel1Format, sep = "")
    },
    content = function(file) {
      cat('#',input$SaveModel1Format,'\n')
      cat('#',input$SendingCountry,'\n')
      cat('#',input$ReceivingCountry,'\n')
      if (input$SendingCountry!=input$ReceivingCountry) {
        RES <- 800
        ffo <- input$SaveModel1Format 
        if (input$ShowLegendSin) width<-10.8 else width<-7.7
        if(ffo=='pdf') {
          pdf(file,width,7)
        } else if(ffo=='png'){
          png(file,width=width*RES,height=7*RES,res=RES)
        } else if(ffo=='tiff'){
          tiff(file,width=width*RES,height=7*RES,res=RES,compression = 'rle')
        }
        #par(mar=c(5.1, 4.1, 3.0, 16.0))
        plot_single_flow_(input, saving=TRUE)
        dev.off()
      }
    }
  )
  
  output$SaveModel3Plot<- downloadHandler(
    filename = function() {
      paste('ModelCircleFlowsPlot_',input$YearSel,'.', input$SaveModel2Format, sep='') },
    content = function(file) {
      if (input$SendingCountry!=input$ReceivingCountry) {
        
        ffo <- input$SaveModel2Format
        if(ffo=='pdf') {
          pdf(file,8,8)
        } else if(ffo=='png'){
          png(file,width=8*600,height=8*600,res=600)
        } else if(ffo=='tiff'){
          tiff(file,width=8*600,height=8*600,res=600,compression = 'rle')
        }
        
        par(mar=rep(0,4), oma=rep(0,4))
        plot_circular_flows_(input)
        dev.off()
      }
    }
  )
  
  output
}