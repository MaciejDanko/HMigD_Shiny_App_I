server_plot_figures<-function(input, output, server, ModelMixedResults_){
 
  output$TRANSITIONSUNDERCOUNTPlot <- renderPlot({
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    plot_lfs_undercounting(toSHINY_LFS_UNDERCOUNTING$combined_02_18[Countries,],toSHINY_LFS_UNDERCOUNTING$du_02_18[Countries,], toSHINY_LFS_UNDERCOUNTING$dm_02_18[Countries,])
  }, height = 680, width = 1200, res=115)
 
  output$TRANSITIONSNRSPlot <- renderPlot({
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    tmp<-toSHINY_LFS_UNDERCOUNTING$nrs_02_18[Countries,]
    dmtmp<-toSHINY_LFS_UNDERCOUNTING$dm_02_18[Countries,]
    dutmp<-toSHINY_LFS_UNDERCOUNTING$du_02_18[Countries,]
    tmp[is.na(dmtmp) | paste(dmtmp)=='0']<-NA
    plot_lfs_undercounting(tmp, dutmp, dmtmp)
  }, height = 680, width = 1200, res=115)
  
  output$TRANSITIONSMISSPlot <- renderPlot({
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    plot_lfs_undercounting(toSHINY_LFS_UNDERCOUNTING$frac_miss_02_18[Countries,],toSHINY_LFS_UNDERCOUNTING$du_02_18[Countries,], toSHINY_LFS_UNDERCOUNTING$dm_02_18[Countries,])
  }, height = 680, width = 1200, res=115)
  
  output$TRANSITIONSACCURACYlot <- renderPlot({ #nonadjusted
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    plot_lfs_accuracy(toSHINY_LFS_ACCU$cv.2[Countries,], toSHINY_LFS_ACCU$dm[Countries,])
  }, height = 680, width = 1200, res=115)
  
  output$OutputFlowsPlot <- renderPlot({
    #ModelMixedResultsArray<-xtabs(pred_alt_q50 ~ orig + dest + year, data = ModelMixedResults_)
    ModelMixedResultsArray<-xtabs(pred_q50 ~ orig + dest + year, data = ModelMixedResults_)
    plot_output_flows_(input, ModelMixedResultsArray)
  }, height = 900, width = 1200, res=115)
  
  output$TRANSITIONSCOUNTPlot <- renderPlot({
    plot_transitions_count_(input)
  }, height = 900, width = 1200, res=115)
  
  output$FLOWSIMMCOUNTPlot <- renderPlot({
    plot_flows_imm_(input)
  }, height = 900, width = 1200, res=115)
  
  output$FLOWSEMICOUNTPlot <- renderPlot({
    plot_flows_emi_(input)
  }, height = 900, width = 1200, res=115)
  
  output$POPULATIONPlot <- renderPlot({
    plot_pop_(input)
  }, height = 900, width = 1200, res=115)
  
  output$GNIPlot <- renderPlot({
    plot_gni_(input)
  }, height = 900, width = 1200, res=115)
  
  output$TRADEPlot <- renderPlot({
    plot_trade_(input)
  }, height = 900, width = 1200, res=115)
  
  output$STOCKSPlot <- renderPlot({
    plot_stocks_(input)
  }, height = 900, width = 1200, res=115)
  
  output$DISTANCEPlot <- renderPlot({
    plot_dist_(input)
  }, height = 900, width = 1200, res=115)
  
  output$LANGUAGEPlot <- renderPlot({
    plot_lang_(input)
  }, height = 900, width = 1200, res=115)
  
  output$FREEDOMPlot <- renderPlot({
    par(mar=c(4,4,1.5,10))
    plot_freedom_(input)
  }, height = 900, width = 1200, res=115)
  
  output$DurImmPlot <- renderPlot({
    plot_dur_imm_(input)
  }, height = 900, width = 1200, res=115)
  
  output$DurEmiPlot <- renderPlot({
    plot_dur_emi_(input)
  }, height = 900, width = 1200, res=115)
  
  output$SrcImmPlot <- renderPlot({
    plot_src_imm_(input)
  }, height = 900, width = 1200, res=115)
  
  output$SrcEmiPlot <- renderPlot({
    plot_src_emi_(input)
  }, height = 900, width = 1200, res=115)
  
  output$AccuImmPlot <- renderPlot({
    plot_acu_imm_(input)
  }, height = 900, width = 1200, res=115)
  
  output$AccuEmiPlot <- renderPlot({
    plot_acu_emi_(input)
  }, height = 900, width = 1200, res=115)
  
  output$UndercountPlotImm <- renderPlot({
    plot_under_imm_(input)
  }, height = 900, width = 1200, res=115)
  
  output$UndercountPlotEmi <- renderPlot({
    plot_under_emi_(input)
  }, height = 900, width = 1200, res=115)
  
  output$Model1Plot <- renderPlot({
    par(mar=c(5.1, 4.1, 3.0, 16.0))
    #par(family = 'serif')
    #print('test1')
    #print(ls(), envir = .GlobalEnv)
    req(input$SendingCountry, input$ReceivingCountry, input$MODEL1, input$MODEL2)
    if (input$SendingCountry!=input$ReceivingCountry)
      plot_single_flow_(input)
  }, height = 700, width = 1200, res=115)
  
  output$Model2Plot <- renderPlot({
    par(mar=c(5.1, 4.1, 3.0, 16.0))
    #par(family = 'serif')
    print('test1')
    THRE<-input$ThrY - 1000*(1-as.numeric(input$UseThreshold))
    print(input$UseThreshold)
    print(THRE)
    plot_aggregated_(input)
    
  }, height = 700, width = 1200, res=115)
  
  
  output$Model3Plot <- renderPlot({
    par(mar=rep(0,4), oma=rep(0,4))
    # #par(family = 'serif')
    # print('test1')
    # THRE<-input$ThrY - 1000*(1-as.numeric(input$UseThreshold))
    # print(input$UseThreshold)
    # print(THRE)
    plot_circular_flows_(input)
    
  }, height = 800, width = 800, res=115)
  
  output
}