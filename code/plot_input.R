plot_aggregated_<<-function(input){
  print('%%%%%%%%%%%%')
  print(as.numeric(TrYear()))
  print(as.numeric(input$UseThreshold))
  print('%%%%%%%%%%%%')
  plot_aggregated(cntr_sen_list=Countries[CountriesFull%in%input$SendCntrs],
                  cntr_rec_list=Countries[CountriesFull%in%input$RecCntrs],
                  Threshold=as.numeric(TrYear()) - 1000*(1-as.numeric(input$UseThreshold)),
                  m1=input$MODEL1b,
                  m2=input$MODEL2b,
                  linetype=as.numeric(input$STYLE2),
                  flow = 'pred',#c('pred',"pred_alt")[as.numeric(input$UseAltPred2)+1],
                  spaceX = 15,
                  col1 = 'orange2',
                  col2 = 'purple4',
                  alpha.75 = 0.3,
                  alpha.95 = 0.1,
                  MODELS2 = MODELS2,
                  show.data = input$sdat,
                  na.rm=input$narm,
                  plotCI=input$aCI)
  
}

plot_single_flow_<<-function(input){
  plot_models(cntr_sen=Countries[as.numeric(input$SendingCountry)],
              cntr_rec=Countries[as.numeric(input$ReceivingCountry)],
              m1=input$MODEL1,
              m2=input$MODEL2,
              linetype=as.numeric(input$STYLE1),
              max.cex.lfs=2, 
              max.cex.rec=3.5, 
              max.cex.sen=3.5,
              max.alfa.lfs=0.5,
              flow = 'pred',#c('pred',"pred_alt")[as.numeric(input$UseAltPred1)+1],
              spaceX=16,
              alpha.75 = 0.2,
              alpha.95 = 0.1,
              MODELS2 = MODELS2,
              col1 = 'orange2',
              col2 = 'purple4',
              pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22)) 
}

plot_circular_flows_<-function(input){
  data<-switch(paste(input$MODEL3), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  plot_circular(data=data, 
                year=as.numeric(as.character(input$YearSel)), 
                orig = Countries[CountriesFull%in%input$SendCntrs3], 
                dest = Countries[CountriesFull%in%input$RecCntrs3],
                alt=FALSE,#input$UseAltPred3, 
                link.lwd=1, border.alpha=0.5, arrow.alpha=0.5, link.arr.length=0.1, 
                showscale=input$ShowScale, 
                show.big.perc=input$Percentiles)
}

plot_freedom_<<-function(input){
  my2dplot(FREEDOM, LEVELS=FREEDOM.LEVELS, txtLEVELS = FREEDOM.txtLEVELS, namat=NULL, cexx=1, cexy=1, lox=1, loy=1,
           nodata='Missing data', naalpha=0.2, colors= (c(
             "#E8E6D9","dodgerblue", "dodgerblue3", "dodgerblue4", 
             "green3","green4","darkgreen","yellow",'gold','orange',"#F03333","#CC2222","#901111",'#555555','black'
           )))
  mtext(expression(bold('Sending country')),2, 2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1, 3,cex=1.2)
  mtext(expression('Freedom of movement of workers'),3, 0,cex=1.2, xpd=TRUE)
}

myPAL<<-c('#8BAB82','#3D4C9F','#DD8C6E',
         '#98ACB5',
         #'#174333',
         #'#0C2521',
         #'#C2882B',
         '#7B3400','#E8553C','#A84233','#2E748A','#013C4C',#'#E3DED7',#'#6A958C',
         #'#8EAD7C',
         #"#5D1D2E",#'#951233','#C15937',
         '#997929')
#plot(seq_along(myPAL),seq_along(myPAL),pch=19, col=(myPAL),cex=4)
plot_dur_emi_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_duration(dE[,paste(Years)], AllLevels = AllLevels, colors=myPAL[1:10])
  mtext(expression('Duration of stay of emigration flows'),3,0.1,cex=1.2)
  mtext(expression(bold('Sending country')),2,2.5, cex=1.2)
}

plot_dur_imm_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_duration(dI[,paste(Years)], AllLevels = AllLevels, colors=myPAL[1:10])
  mtext(expression('Duration of stay of immigration flows'),3,0.1,cex=1.2)
  mtext(expression(bold('Receiving country')),2,2.5,cex=1.2)
}

plot_src_emi_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_Sources(direction='E', input$src_e_year)
  mtext(expression('Data sources of emigration flows'),3,0.1,cex=1.2)
}

plot_src_imm_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_Sources(direction='I', input$src_i_year)
  mtext(expression('Data sources of immigration flows'),3,0.1,cex=1.2)
}

plot_acu_emi_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_Accuracy(direction='E', adjusted=input$acu_e_adjusted)
  mtext(expression('Accuracy of emigration flows'),3,0.1,cex=1.2)
}

plot_acu_imm_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_Accuracy(direction='I', adjusted=input$acu_i_adjusted)
  mtext(expression('Accuracy of immigration flows'),3,0.1,cex=1.2)
}

plot_under_imm_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  #my2dplot(rsm.data$U4_I.r,LEVELS=1:5, c('Very low','Low','Medium','High','Very high'))
  plot_Undercounting('I')
  mtext(expression('Undercounting of immigration flows'),3,0.1,cex=1.2)
  mtext(expression(bold('Receiving country')),2,2.5,cex=1.2)
}

plot_under_emi_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  plot_Undercounting('E')
  
  #my2dplot(rsm.data$U4_E.r,LEVELS=1:5, c('Very low','Low','Medium','High','Very high'))
  mtext(expression('Undercounting of emigration flows'),3,0.1,cex=1.2)
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
}

plot_dist_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  if (input$log_dist) {
    my2dplotClassify(rsm.data$log_distwces,digits=2, include_zero = FALSE, use.na=FALSE)
    mtext(expression('Log-centered population-weighted distance between countries in km'),3,0,cex=1.2)
  } else {
     my2dplotClassify(rsm.data$grav_dist/1000,digits=2, use.na=FALSE)
     mtext(expression('Population-weighted distance between countries in thousands of km'),3,0,cex=1.2)
  }
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
}

plot_lang_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  if (input$log_lang) {
    my2dplotClassify(rsm.data$log_cl, digits=2, include_zero = FALSE, use.na=FALSE)
    mtext(expression('Log-centered distance between languages in CEPII units'),3,0,cex=1.2)
  } else {
    my2dplotClassify(rsm.data$grav_lang, digits=2, use.na=FALSE)
    mtext(expression('Distance between languages in CEPII units'),3,0.1,cex=1.2)
  }
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
}

plot_stocks_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  if (input$log_stocks) {
    my2dplotClassify(rsm.data$log_msto_simple, digits=1, include_zero = FALSE, use.na=FALSE)
    mtext(expression('Log-centered year-averaged migration stocks'),3,0.1,cex=1.2)
  } else {
    my2dplotClassify(rsm.data$grav_stocks_simple/1e6, digits=1, use.na=FALSE)
    mtext(expression('Year-averaged migration stocks in milions of people'),3,0.1,cex=1.2)
  }
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
}

plot_trade_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  ncat<<-20
  if (input$log_trade) {
    rr<<-seq(min(unlist(rsm.data$log_trade),na.rm = TRUE), max(unlist(rsm.data$log_trade), na.rm=T), length.out=ncat+1)
    my2dplotClassify(rsm.data$log_trade[,,paste(input$trade_year)], digits=2, rr=rr, ncat=ncat, include_zero = FALSE, use.na=FALSE)
    mtext(paste('Log-centered bilateral trade in millions of dolars in',input$trade_year),3,0.1,cex=1.2)
  } else {
    rr<<-seq(min(unlist(rsm.data$grav_trade),na.rm = TRUE), max(unlist(rsm.data$grav_trade), na.rm=T), length.out=ncat+1)/1000
    my2dplotClassify(rsm.data$grav_trade[,,paste(input$trade_year)]/1000, digits=2, rr=rr, ncat=ncat, use.na=FALSE)
    print(table(rsm.data$grav_trade[,,paste(input$trade_year)]/1000 <rr[2]))
    mtext(paste('Bilateral trade in billions of dolars in',input$trade_year),3,0.1,cex=1.2)
  }
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
  # stop('check lebeling in other plots, log_centered* - help')
  # stop('Transfor data as it is used in the model, the same with accuracy')
}

plot_transitions_count_ <<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  ncat<<-20
    rr<<- c(0, 10, 100, 1000, 10000, 100000, 1e6)
    la <- c('(0, 10]','(10,100]','(100, 1K]','(1K, 10K]','(10K, 100K]','(100K, 1M]')
    CC <- rsm.data$k.imm[,,paste(input$transitions_count_year)]
    diag(CC)<-NA
    my2dplotClassify(CC, digits=2, rr=rr, labels = la, ncat=ncat, diagwhite=TRUE, nodata='Missing data or\nno migration\nreported', legoffs=0.5, include.lowest = FALSE, include_zero = FALSE)
    mtext(paste('Migration transitions in',input$transitions_count_year),3,0.1,cex=1.2)
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
  # stop('check lebeling in other plots, log_centered* - help')
  # stop('Transfor data as it is used in the model, the same with accuracy')
}

plot_output_flows_ <<-function(input, data){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  rr<<- c(0, 10, 100, 1000, 10000, 100000, 1e6)
  la <<- c('[0, 10]','(10,100]','(100, 1K]','(1K, 10K]','(10K, 100K]','(100K, 1M]')
  CC <- data[,,paste(input$visualize_output_year)]
  diag(CC)<-NA
  my2dplotClassify(CC, digits=2, rr=rr, labels = la, diagwhite=TRUE, nodata='', legoffs=0, use.na = FALSE)
  mtext(paste('Estimated migration flows in',input$visualize_output_year),3,0.1,cex=1.2)
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
  # stop('check lebeling in other plots, log_centered* - help')
  # stop('Transfor data as it is used in the model, the same with accuracy')
}

plot_flows_imm_ <<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  ncat<<-20
  rr<<- c(0, 10, 100, 1000, 10000, 100000, 1e6)
  la <- c('[0, 10]','(10,100]','(100, 1K]','(1K, 10K]','(10K, 100K]','(100K, 1M]')
  CC <- rsm.data$x.imm[,,paste(input$flows_imm_year)]
  diag(CC)<-NA
  my2dplotClassify(CC, digits=2, rr=rr, labels = la, ncat=ncat, diagwhite=TRUE, use.na= TRUE, nodata = 'Missing data')
  mtext(paste('Migration flows in',input$flows_imm_year),3,0.1,cex=1.2)
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
  # stop('check lebeling in other plots, log_centered* - help')
  # stop('Transfor data as it is used in the model, the same with accuracy')
}

plot_flows_emi_ <<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  ncat<<-20
  rr<<- c(0, 10, 100, 1000, 10000, 100000, 1e6)
  la <- c('[0, 10]','(10,100]','(100, 1K]','(1K, 10K]','(10K, 100K]','(100K, 1M]')
  CC <- rsm.data$x.emi[,,paste(input$flows_emi_year)]
  diag(CC)<-NA
  my2dplotClassify(CC, digits=2, rr=rr, labels = la, ncat=ncat, diagwhite=TRUE, use.na= TRUE, nodata = 'Missing data')
  mtext(paste('Migration flows in',input$flows_emi_year),3,0.1,cex=1.2)
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
  # stop('check lebeling in other plots, log_centered* - help')
  # stop('Transfor data as it is used in the model, the same with accuracy')
}

plot_gni_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  ncat<<-20
  if (input$log_gni) {
    rr<<-seq(min(unlist(rsm.data$grav_log_gni_ratio),na.rm = TRUE), max(unlist(rsm.data$grav_log_gni_ratio), na.rm=T), length.out=ncat+1)
    my2dplotClassify(rsm.data$grav_log_gni_ratio[,,paste(input$gni_year)], digits=2, rr=rr, ncat=ncat, include_zero = FALSE, use.na=FALSE)
    mtext(paste('Log-centered receiving-sending GNI ratio in',input$trade_year),3,0.1,cex=1.2)
  } else {
    rr<<-seq(min(unlist(rsm.data$grav_gni_ratio),na.rm = TRUE), max(unlist(rsm.data$grav_gni_ratio), na.rm=T), length.out=ncat+1)
    my2dplotClassify(rsm.data$grav_gni_ratio[,,paste(input$gni_year)], digits=2, rr=rr, ncat=ncat, use.na=FALSE)
    mtext(paste('Receiving-sending GNI ratio in',input$gni_year),3,0.1,cex=1.2)
  }
  mtext(expression(bold('Sending country')),2,2.5,cex=1.2)
  mtext(expression(bold('Receiving country')),1,2.5,cex=1.2)
  # stop('check lebeling in other plots, log_centered* - help')
  # stop('Transfor data as it is used in the model, the same with accuracy')
}

plot_pop_<<-function(input){
  par(mar=c(4,4,1.5,10),oma=c(0,0,0,0))
  if (input$log_pop) {
    my2dplotClassify(rsm.data$log_pop, digits=1, include_zero = FALSE, use.na=FALSE)
    mtext(expression('Log-centered population size'),3,0.1,cex=1.2)
  } else {
    my2dplotClassify(rsm.data$grav_pop, digits=1, use.na=FALSE)
    mtext(expression('Population in millions of people'),3,0.1,cex=1.2)
  }
  #mtext(expression(bold('Sending country')),1,2.5,cex=1.2)
  mtext(expression(bold('Reporting country')),2,2.5,cex=1.2)
}

# if (FALSE){
# #   test<-rsm.data$grav_dist/1000
# #   test[1,]<-1
# #   my2dplotClassify(test,digits=2)
# #   
# #   my2dplotClassify(rsm.data$grav_dist/1000,digits=2)
# # my2dplotClassify(rsm.data$log_distwces,digits=2)
# # 
# # my2dplotClassify(rsm.data$grav_lang, digits=2)
# # my2dplotClassify(rsm.data$log_cl, digits=2)
# 
# # my2dplotClassify(rsm.data$grav_trade[,,1], digits=2)
# # my2dplotClassify(rsm.data$log_trade[,,1], digits=2)
# 
# # my2dplotClassify(rsm.data$grav_gni_ratio[,,1], digits=2)
# # my2dplotClassify(rsm.data$grav_log_gni_ratio[,,1], digits=2)
# 
# # my2dplotClassify(rsm.data$grav_pop, digits=1)
# # my2dplotClassify(rsm.data$log_pop, digits=1)
# 
# # my2dplotClassify(rsm.data$grav_stocks_simple/1e6, digits=1)
# # my2dplotClassify(rsm.data$log_msto_simple, digits=1)
# }