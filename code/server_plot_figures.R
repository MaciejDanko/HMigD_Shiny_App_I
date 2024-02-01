server_plot_figures<-function(input, output){
  output$TRANSITIONSUNDERCOUNTPlot <- renderPlot({
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    plot_lfs_undercounting(toSHINY_LFS_UNDERCOUNTING$combined_02_20[Countries,],toSHINY_LFS_UNDERCOUNTING$du_02_20[Countries,], toSHINY_LFS_UNDERCOUNTING$dm_02_20[Countries,])
  }, height = 680, width = 1200, res=115)
 
  output$TRANSITIONSNRSPlot <- renderPlot({
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    tmp<-toSHINY_LFS_UNDERCOUNTING$nrs_02_20[Countries,]
    dmtmp<-toSHINY_LFS_UNDERCOUNTING$dm_02_20[Countries,]
    dutmp<-toSHINY_LFS_UNDERCOUNTING$du_02_20[Countries,]
    tmp[is.na(dmtmp) | paste(dmtmp)=='0']<-NA
    plot_lfs_undercounting(tmp, dutmp, dmtmp)
  }, height = 680, width = 1200, res=115)
  
  output$TRANSITIONSMISSPlot <- renderPlot({
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    plot_lfs_undercounting(toSHINY_LFS_UNDERCOUNTING$frac_miss_02_20[Countries,],toSHINY_LFS_UNDERCOUNTING$du_02_20[Countries,], toSHINY_LFS_UNDERCOUNTING$dm_02_20[Countries,])
  }, height = 680, width = 1200, res=115)
  
  output$TRANSITIONSACCURACYlot <- renderPlot({ #nonadjusted
    par(mar=c(4,4,0.5,0.5),oma=rep(0,4))
    plot_lfs_accuracy(toSHINY_LFS_ACCU$cv.2[Countries,], toSHINY_LFS_ACCU$dm[Countries,])
  }, height = 680, width = 1200, res=115)
  
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
  
  output$piesrcI <- renderPlot({
    plot_src_summary(src_summary_I)
  })
  
  output$piesrcE <- renderPlot({
    plot_src_summary(src_summary_E)
  })
  
  output$pielfs <- renderPlot({
    plot_lfs_stats()
  })
  
  output
}