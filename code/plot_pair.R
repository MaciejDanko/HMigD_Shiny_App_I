palette <- c(red = "#FF0000", orange = "#FFA500", yellow = "#FFFF00", 
             green = "#008000", cyan = "#00FFFF", blue = "#0000FF", 
             maroon = "#800000", pink = "#FFC0CB", salmon = "#FF8C69", 
             purple = "#800080", magenta = "#FF00FF", navy = "#000080", 
             olive = "#808000", teal = "#008080", coral = "#FF7F50", 
             brown = "#A52A2A", sienna = "#A0522D", tan = "#D2B48C", 
             gold = "#FFD700", khaki = "#F0E68C", orchid = "#DA70D6", 
             silver = "#C0C0C0", indigo = "#4B0082", aquamarine = "#7FFFD4", 
             chartreuse = "#7FFF00", lavender = "#E6E6FA", 
             steelblue = "#4682B4", tomato = "#FF6347", slategray = "#708090", 
             violet = "#EE82EE", fuchsia = "#FF00FF", darkgreen = "#006400", 
             darkblue = "#00008B", peru = "#CD853F", rosybrown = "#BC8F8F", 
             hotpink = "#FF69B4", deepskyblue = "#00BFFF", sandybrown = "#F4A460", 
             forestgreen = "#228B22", skyblue = "#87CEEB", sienna = "#A0522D", 
             oliveDrab = "#6B8E23", indianred = "#CD5C5C", chocolate = "#D2691E", black='#000000')

palette<-palette[order(colSums(col2rgb(palette)))]

make_str_list<-function(x){
  if (any(x =='...')) paste0(x, collapse = ', ') else
    if (length(x) == 2) paste0(x, collapse = ' and ') else
      if (length(x) > 2) paste0(c(paste0(x[-length(x)], collapse = ', '),x[length(x)]), collapse = ', and ') else
        paste(x)
}

top_text<-function(label, size=0.075, lwd=1.75, font=1, col=1, border=1, fill=0, boxtype=0){
  usr<-par('usr')
  rect(usr[1],usr[4],usr[2],usr[4]+size*diff(usr[3:4]),xpd=TRUE,lwd=lwd, col=fill, border=border)
  text(x=usr[1]+diff(usr[1:2])/2, y=usr[4]+size*diff(usr[3:4])/2,adj=c(0.5,0.5), 
       labels=label, xpd=TRUE, font = font, col=col)
  if (boxtype==1) box() else
    if (boxtype==2) rect(usr[1],usr[3],usr[2],usr[4]+size*diff(usr[3:4])*0.9999,lwd=lwd, border=1, xpd=TRUE) else
      if (boxtype!=0) stop()
}

tn<-function(k) as.numeric(as.character(k))

mypie<-function(xl,yb,xr,yt, cex=0.95, fractions=c(0.5,0.2,0.3), col=seq_along(fractions)+1, border=NULL,lty=1, lwd=0.5){
  fractions<-fractions/sum(fractions)
  cumfrac<-c(0,cumsum(fractions))*2*pi
  for (j in seq_along(fractions)){
    obw <- seq(cumfrac[j], cumfrac[j+1], length.out=100)
    xm <- (xr+xl)/2
    ym <- (yb+yt)/2
    ry <- cex*(yt-yb)/2
    rx <- cex*(xr-xl)/2
    x <- sin(obw)*rx+xm
    y <- cos(obw)*ry+ym
    if (fractions[j]>0){
      polygon(c(xm,x),c(ym,y),col=col[j], border=col[j])
    }
    if(length(border)) lines(x,y,col=border,lty=lty, lwd=lwd)
  }
}

prepare_data <- function(Data_input, orig, dest, flow = "pred", 
                         max.cex.lfs=3, max.cex.rec=5, max.cex.sen=5,
                         max.alfa.lfs=1,
                         pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22)) {
  Data_input <- as.data.frame(Data_input)
  Data_input<- Data_input[(Data_input$orig==orig) & (Data_input$dest == dest),]
  
  data.frame(sen=Data_input$orig, rec=Data_input$dest, year=Data_input$year,
             r_sen=Data_input$r_sen, 
             r_rec=Data_input$r_rec, 
             r_lfs=Data_input$s_rec, 
             
             pch_sen = pch.values[c('8-12','0-2','3','6','P')[tn(Data_input$I_S)+1]],
             pch_rec = pch.values[c('8-12','0-2','3','6','P')[tn(Data_input$I_R)+1]], # immigration by rec
             pch_lfs = rep(unname(pch.values['8-12']),nrow(Data_input)),
             alpha_sen=(4-Data_input$A_E.r)/3,   #er  
             alpha_rec=(4-Data_input$A_I.r)/3, #now: 3=high, 2=med, 1=low # ir
             alpha_lfs=max.alfa.lfs*(4-c(1,2)[Data_input$a_imm.s])/3, #now 3=high, 2=low
             cex_rec = (5-Data_input$U_I.r)*(max.cex.rec-1)/4+1 ,  # in-mig reported by receiving
             cex_sen = (5-Data_input$U_E.r)*(max.cex.sen-1)/4+1,
             cex_lfs = (1-Data_input$LUI.s)*(max.cex.lfs-1)+1 ,
             # cex_sen = (Data_input$U_I.r-1)*(max.cex.sen-1)/4+1 ,
             # cex_rec = (Data_input$U_E.r-1)*(max.cex.rec-1)/4+1,
             # cex_lfs = (Data_input$LUI.s)*(max.cex.lfs-1)+1 ,
             Flow05=unname(Data_input[,paste0(flow,"_q05")]), 
             Flow25=unname(Data_input[,paste0(flow,"_q25")]),
             Flow50=unname(Data_input[,paste0(flow,"_q50")]),
             Flow75=unname(Data_input[,paste0(flow,"_q75")]),
             Flow95=unname(Data_input[,paste0(flow,"_q95")]),
             stringsAsFactors = FALSE)
}

prepare_data_save <- function(Data_input, orig, dest, flow = "pred") {
  Data_input <- as.data.frame(Data_input)
  Data_input<- Data_input[(Data_input$orig==orig) & (Data_input$dest == dest),]
  
  data.frame(sending=Data_input$orig, receiving=Data_input$dest, year=Data_input$year,
             # r_sen=Data_input$r_sen, 
             # r_rec=Data_input$r_rec, 
             # r_lfs=Data_input$s_rec, 
             Flow05=unname(Data_input[,paste0(flow,"_q05")]), 
             Flow25=unname(Data_input[,paste0(flow,"_q25")]),
             Flow50=unname(Data_input[,paste0(flow,"_q50")]),
             Flow75=unname(Data_input[,paste0(flow,"_q75")]),
             Flow95=unname(Data_input[,paste0(flow,"_q95")]),
             
             'duration sending' = c('8-12','0-2','3','6','P')[tn(Data_input$I_S)+1],
             'duration receiving' = c('8-12','0-2','3','6','P')[tn(Data_input$I_R)+1],
             'accuracy sending' = c('high','medium','low')[Data_input$A_E.r],
             'accuracy receiving' = c('high','medium','low')[Data_input$A_I.r],
             'undercounting sending' = c('very low','low','medium','high','very high')[Data_input$U_E.r],
             'undercounting receiving' = c('very low','low','medium','high','very high')[Data_input$U_I.r],
             'accuracy LFS' = c('high','low')[Data_input$a_imm.s],
             'undercounting LFS'= c('low','high')[Data_input$LUI.s+1],
             # alpha_sen=(4-Data_input$A_E.r)/3,   #er  
             # alpha_rec=(4-Data_input$A_I.r)/3, #now: 3=high, 2=med, 1=low # ir
             # alpha_lfs=max.alfa.lfs*(4-c(1,2)[Data_input$a_imm.s])/3, #now 3=high, 2=low
             # cex_rec = (5-Data_input$U_I.r)*(max.cex.rec-1)/4+1 ,  # in-mig reported by receiving
             # cex_sen = (5-Data_input$U_E.r)*(max.cex.sen-1)/4+1,
             # cex_lfs = (1-Data_input$LUI.s)*(max.cex.lfs-1)+1 ,
             # cex_sen = (Data_input$U_I.r-1)*(max.cex.sen-1)/4+1 ,
             # cex_rec = (Data_input$U_E.r-1)*(max.cex.rec-1)/4+1,
             # cex_lfs = (Data_input$LUI.s)*(max.cex.lfs-1)+1 ,
             stringsAsFactors = FALSE)
}


# Data_input<-Data_input_80a
# orig<-c('DE','UK')
# dest<-c('PL','CZ')

prepare_aggregated_data_old <- function(Data_input, orig, dest, flow = "pred", na.rm=FALSE) {
  Data_input <- as.data.frame(Data_input)
  Data_input<- Data_input[(Data_input$orig%in%orig) & (Data_input$dest %in% dest),]
  Y<-sort(unique(Data_input$year))
  Flow05=unname(Data_input[,paste0(flow,"_q05")]) 
  Flow25=unname(Data_input[,paste0(flow,"_q25")])
  Flow50=unname(Data_input[,paste0(flow,"_q50")])
  Flow75=unname(Data_input[,paste0(flow,"_q75")])
  Flow95=unname(Data_input[,paste0(flow,"_q95")])
  data.frame( year=Y,
              r_sen=sapply(Y, function(k) sum(Data_input$r_sen[which(Data_input$year==k)],na.rm=na.rm)), 
              r_rec=sapply(Y, function(k) sum(Data_input$r_rec[which(Data_input$year==k)],na.rm=na.rm)), 
              r_lfs=sapply(Y, function(k) sum(Data_input$s_rec[which(Data_input$year==k)],na.rm=na.rm)),
              Flow05=sapply(Y, function(k) sum(Flow05[which(Data_input$year==k)],na.rm=na.rm)),
              Flow25=sapply(Y, function(k) sum(Flow25[which(Data_input$year==k)],na.rm=na.rm)),
              Flow50=sapply(Y, function(k) sum(Flow50[which(Data_input$year==k)],na.rm=na.rm)),
              Flow75=sapply(Y, function(k) sum(Flow75[which(Data_input$year==k)],na.rm=na.rm)),
              Flow95=sapply(Y, function(k) sum(Flow95[which(Data_input$year==k)],na.rm=na.rm)),
              stringsAsFactors = FALSE)
}

minus1fun<-function(x) {x<-x-1; x[x<0]<-0; x}
prepare_aggregated_data <- function(Data_input, Data_input2, orig, dest, flow = "pred", na.rm=FALSE) {
  
  Data_input<-Data_input[,Countries%in%orig,Countries%in%dest,,drop=FALSE]
  Data_input<-apply(Data_input,c(1,4),sum, na.rm=TRUE)
  Data_input2 <- as.data.frame(Data_input2)
  Data_input2<- Data_input2[(Data_input2$orig%in%orig) & (Data_input2$dest %in% dest),]
  Y<-sort(unique(Data_input2$year))
  data.frame( year=Y,
              r_sen=sapply(Y, function(k) sum(Data_input2$r_sen[which(Data_input2$year==k)],na.rm=na.rm)), 
              r_rec=sapply(Y, function(k) sum(Data_input2$r_rec[which(Data_input2$year==k)],na.rm=na.rm)), 
              r_lfs=sapply(Y, function(k) sum(Data_input2$s_rec[which(Data_input2$year==k)],na.rm=na.rm)),
              Flow05=sapply(1:ncol(Data_input),function(k) minus1fun(quantile(Data_input[,k], 0.05))),
              Flow25=sapply(1:ncol(Data_input),function(k) minus1fun(quantile(Data_input[,k], 0.25))),
              Flow50=sapply(1:ncol(Data_input),function(k) quantile(Data_input[,k], 0.50)),
              Flow75=sapply(1:ncol(Data_input),function(k) quantile(Data_input[,k], 0.75)+1),
              Flow95=sapply(1:ncol(Data_input),function(k) quantile(Data_input[,k], 0.95)+1),
              stringsAsFactors = FALSE)
}


get_aggregated <- function( cntr_sen_list=c('RO','BG'),
                            cntr_rec_list=c('UK'),
                            
                            m1=1,
                            m2=2,
                            
                            flow = "pred",
                            #spaceX = 15,
                            #col1 = 'orange2',
                            #col2 = 'deepskyblue2',
                            #alpha.75 = 0.3,
                            #alpha.95 = 0.1,
                            #MODELS2 = NULL,
                            #show.data = TRUE,
                            na.rm=FALSE 
                            #plotCI=FALSE,
                            #plotLegend=TRUE,
                            #plotTitle=TRUE
){
  m1=as.numeric(m1)
  m2=as.numeric(m2)
  if (m1==m2) col1<-col2<-'black'
  #print('test2')
  #print(ls())
  MO1D<-switch(paste(m1),
               '1' = Data_input_80a,
               '2' = Data_input_80b,
               '3' = Data_input_80c,
               '4' = Data_input_80d,
               '5' = Data_input_80e,
               '6' = Data_input_80f)
  MO2D<-switch(paste(m2),
               '1' = Data_input_80a,
               '2' = Data_input_80b,
               '3' = Data_input_80c,
               '4' = Data_input_80d,
               '5' = Data_input_80e,
               '6' = Data_input_80f)
  
  MO1F<-switch(paste(m1), 
               '1' = raw_int_flows_a, 
               '2' = raw_int_flows_b,
               '3' = raw_int_flows_c,
               '4' = raw_int_flows_d,
               '5' = raw_int_flows_e,
               '6' = raw_int_flows_f)
  MO2F<-switch(paste(m2), 
               '1' = raw_int_flows_a, 
               '2' = raw_int_flows_b,
               '3' = raw_int_flows_c,
               '4' = raw_int_flows_d,
               '5' = raw_int_flows_e,
               '6' = raw_int_flows_f)
  
  M1<-prepare_aggregated_data(MO1F, MO1D, cntr_sen_list,cntr_rec_list, flow=flow, na.rm=na.rm)[,-c(2,3,4)]
  M2<-prepare_aggregated_data(MO2F, MO2D,cntr_sen_list,cntr_rec_list, flow=flow, na.rm=na.rm)[,-c(2,3,4)]
  M1$model<-letters[as.integer(m1)]
  M2$model<-letters[as.integer(m2)]
  list(M1=M1,M2=M2)
}

plot_aggregated <- function(cntr_sen_list=c('RO','BG'),
                            cntr_rec_list=c('UK'),
                            Threshold=2014,
                            m1=1,
                            m2=2,
                            linetype=3,
                            flow = "pred",
                            spaceX = 15,
                            col1 = 'orange2',
                            col2 = 'deepskyblue2',
                            alpha.75 = 0.3,
                            alpha.95 = 0.1,
                            MODELS2 = NULL,
                            show.data = TRUE,
                            na.rm=FALSE, 
                            plotCI=FALSE,
                            plotLegend=TRUE,
                            plotTitle=TRUE
){
  m1=as.numeric(m1)
  m2=as.numeric(m2)
  if (m1==m2) col1<-col2<-'black'
  #print('test2')
  #print(ls())
  MO1D<-switch(paste(m1),
               '1' = Data_input_80a,
               '2' = Data_input_80b,
               '3' = Data_input_80c,
               '4' = Data_input_80d,
               '5' = Data_input_80e,
               '6' = Data_input_80f)
  MO2D<-switch(paste(m2),
               '1' = Data_input_80a,
               '2' = Data_input_80b,
               '3' = Data_input_80c,
               '4' = Data_input_80d,
               '5' = Data_input_80e,
               '6' = Data_input_80f)
  
  MO1F<-switch(paste(m1), 
               '1' = raw_int_flows_a, 
               '2' = raw_int_flows_b,
               '3' = raw_int_flows_c,
               '4' = raw_int_flows_d,
               '5' = raw_int_flows_e,
               '6' = raw_int_flows_f)
  MO2F<-switch(paste(m2), 
               '1' = raw_int_flows_a, 
               '2' = raw_int_flows_b,
               '3' = raw_int_flows_c,
               '4' = raw_int_flows_d,
               '5' = raw_int_flows_e,
               '6' = raw_int_flows_f)
  
  M1<-prepare_aggregated_data(MO1F, MO1D, cntr_sen_list,cntr_rec_list, flow=flow, na.rm=na.rm)
  M2<-prepare_aggregated_data(MO2F, MO2D,cntr_sen_list,cntr_rec_list, flow=flow, na.rm=na.rm)
  if (na.rm){
    M1$r_sen[M1$r_sen==0]<-NA
    M2$r_sen[M2$r_sen==0]<-NA
    M1$r_rec[M1$r_rec==0]<-NA
    M2$r_rec[M2$r_rec==0]<-NA
  }
  YLIM<-range(M1$r_sen,M2$r_sen,M1$r_rec,M2$r_rec,M1$r_lfs,M2$r_lfs, M1$Flow05,M2$Flow05,M1$Flow95,M2$Flow95, na.rm=TRUE)
  XLIM<-range(M1$year,M2$year)
  if (linetype==2) XLIM[2]<-XLIM[2]+1
  Z<-par('mar')
  if (plotLegend) Z[4]<-spaceX else {
    mid<-(spaceX + Z[2])/2 
    Z[4]<- mid-2
    Z[2]<- mid+2
  }
  if (plotTitle) Z[3]<- 3 else Z[3]<-1.1
  
  par(mar=Z)
  plot(M1$year, M1$r_sen/1000, ylim=c(0,YLIM[2])/1000, xlim=XLIM, type='n', axes='F',
       xlab='',ylab='')
  mtext(expression(bold(Year)),1,3.5, cex=1.2)
  mtext(expression(bold('Flows (in 1,000)')),2,2.2,cex=1.2)
  axis(1,at=XLIM[1]:XLIM[2], las=3)
  abline(v=1950:2050, h=axis(2), lty=2, col='gray')
  box()
  
  if (linetype==3){
    M1L<-spline(M1$year, M1$Flow50/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L<-spline(M2$year, M2$Flow50/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L25<-spline(M1$year, M1$Flow25/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L25<-spline(M2$year, M2$Flow25/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L75<-spline(M1$year, M1$Flow75/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L75<-spline(M2$year, M2$Flow75/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L05<-spline(M1$year, M1$Flow05/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L05<-spline(M2$year, M2$Flow05/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L95<-spline(M1$year, M1$Flow95/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L95<-spline(M2$year, M2$Flow95/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    
    if (plotCI) {
      polygon(c(M1L25$x,rev(M1L75$x)),c(M1L25$y,rev(M1L75$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.75))
      polygon(c(M1L05$x,rev(M1L95$x)),c(M1L05$y,rev(M1L95$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.95))
      if (m1!=m2) {
        polygon(c(M2L25$x,rev(M2L75$x)),c(M2L25$y,rev(M2L75$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.75))
        polygon(c(M2L05$x,rev(M2L95$x)),c(M2L05$y,rev(M2L95$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.95))
      }
    }
    
    lines(M1L, lwd=3, col=adjustcolor(col1,alpha.f = 0.9))
    if (m1!=m2) lines(M2L, lwd=3, col=adjustcolor(col2,alpha.f = 0.9))
  } else if (linetype==2){
    M1L<-approx(M1$year, M1$Flow50/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L<-approx(M2$year, M2$Flow50/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L25<-approx(M1$year, M1$Flow25/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L25<-approx(M2$year, M2$Flow25/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L75<-approx(M1$year, M1$Flow75/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L75<-approx(M2$year, M2$Flow75/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L05<-approx(M1$year, M1$Flow05/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L05<-approx(M2$year, M2$Flow05/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L95<-approx(M1$year, M1$Flow95/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L95<-approx(M2$year, M2$Flow95/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    
    if (plotCI) {
      polygon(c(M1L25$x,rev(M1L75$x)),c(M1L25$y,rev(M1L75$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.75))
      polygon(c(M1L05$x,rev(M1L95$x)),c(M1L05$y,rev(M1L95$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.95))
      if (m1!=m2) {
        polygon(c(M2L25$x,rev(M2L75$x)),c(M2L25$y,rev(M2L75$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.75))
        polygon(c(M2L05$x,rev(M2L95$x)),c(M2L05$y,rev(M2L95$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.95))
      }
    }
    
    lines(M1L, lwd=3, col=adjustcolor(col1,alpha.f = 0.9))
    if (m1!=m2) lines(M2L, lwd=3, col=adjustcolor(col2,alpha.f = 0.9))
    #lines(M1$year, M1$Flow50/1000, lwd=2, col=adjustcolor(col1,alpha.f = 0.8),type='s')
    #lines(M2$year, M2$Flow50/1000, lwd=2, col=adjustcolor(col2,alpha.f = 0.8),type='s')
  } else if (linetype==1){
    M1L<-approx(M1$year, M1$Flow50/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L<-approx(M2$year, M2$Flow50/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L25<-approx(M1$year, M1$Flow25/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L25<-approx(M2$year, M2$Flow25/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L75<-approx(M1$year, M1$Flow75/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L75<-approx(M2$year, M2$Flow75/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L05<-approx(M1$year, M1$Flow05/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L05<-approx(M2$year, M2$Flow05/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L95<-approx(M1$year, M1$Flow95/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L95<-approx(M2$year, M2$Flow95/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    
    if (plotCI) {
      polygon(c(M1L25$x,rev(M1L75$x)),c(M1L25$y,rev(M1L75$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.75))
      polygon(c(M1L05$x,rev(M1L95$x)),c(M1L05$y,rev(M1L95$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.95))
      
      if (m1!=m2) {
        polygon(c(M2L25$x,rev(M2L75$x)),c(M2L25$y,rev(M2L75$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.75))
        polygon(c(M2L05$x,rev(M2L95$x)),c(M2L05$y,rev(M2L95$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.95))
      }
    }
    #lines(M1$year, M1$Flow50/1000, lwd=2, col=adjustcolor(col1,alpha.f = 0.8),type='l')
    #if (m1!=m2) lines(M2$year, M2$Flow50/1000, lwd=2, col=adjustcolor(col2,alpha.f = 0.8),type='l')
    lines(M1L, lwd=3, col=adjustcolor(col1,alpha.f = 0.9))
    if (m1!=m2) lines(M2L, lwd=3, col=adjustcolor(col2,alpha.f = 0.9))
    
  }
  abline(v=Threshold, lwd=2, lty=2)
  if (show.data){
    lines(M1$year+0.5, M1$r_sen/1000, ylim=c(0,YLIM[2])/1000, 
          col='white',
          bg='white',
          pch=21, 
          cex=2, type='p')
    lines(M1$year+0.5, M1$r_rec/1000, ylim=c(0,YLIM[2])/1000, 
          col='white', 
          bg='white', 
          pch=21, 
          cex=2, type='p')
    lines(M1$year+0.5, M1$r_lfs/1000, ylim=c(0,YLIM[2])/1000, 
          col='white', 
          bg = 'white', 
          pch=21, 
          cex=2, type='p')
    
    lines(M1$year+0.5, M1$r_sen/1000, ylim=c(0,YLIM[2])/1000, 
          col=adjustcolor(2,alpha.f = 0.7, red.f = 0.8, green.f = 0.8, blue.f = 0.8),
          bg=adjustcolor(2,alpha.f = 0.7),
          pch=21, 
          cex=2, 
          type='p')
    lines(M1$year+0.5, M1$r_rec/1000, ylim=c(0,YLIM[2])/1000, 
          col=adjustcolor(4,alpha.f = 0.7, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg=adjustcolor(4,alpha.f = 0.7), 
          pch=21, 
          cex=2, type='p')
    lines(M1$year+0.5, M1$r_lfs/1000, ylim=c(0,YLIM[2])/1000, 
          col= adjustcolor(3,alpha.f = 0.7, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg = adjustcolor(3,alpha.f = 0.7), 
          pch=21, 
          cex=2, type='p')
  }
  #mark accession !!!
  
  cntr_sen_listP<-cntr_sen_list; cntr_rec_listP<-cntr_rec_list
  if(length(cntr_sen_listP)>6) cntr_sen_listP<-c(cntr_sen_listP[1:6],'...')
  if(length(cntr_rec_listP)>6) cntr_rec_listP<-c(cntr_rec_listP[1:6],'...')
  if (plotTitle) top_text(paste0('Migration flows from ',make_str_list(cntr_sen_listP),' to ',make_str_list(cntr_rec_listP)), fill='lightgray')
  
  usr<-par('usr')
  #size=0.1
  #usr[4]<-usr[4]+(0.1-0.075)*diff(usr[3:4])
  usr[2]<-usr[2]+0.25
  x0<-max(sapply(c(letters,LETTERS),strwidth))
  y0<-max(sapply(c(letters,LETTERS),strheight))
  
  if (linetype==2) facx<-(length(unique(M1$year))+1)/length(unique(M1$year)) else facx<-1
  #rect(usr[2],usr[3],usr[2]+max(sapply(c(letters,LETTERS),strwidth))*10,usr[4],xpd=TRUE,lwd=1.5, col=0, border=1)
  #text(usr[2]+x0*0.5, usr[4] - y0, 'Accuracy and data source', xpd=TRUE, adj=0)
  #text(usr[2]+0:2+x0*1.5 ,rep(usr[4] - y0*2.6,3), c('low','med','high'), xpd=TRUE, cex=0.7, adj=c(0.5,0))
  
  if (plotLegend) {
    if (show.data){
      text(usr[2]+facx*(x0*0.5), usr[4] - y0, 'Aggregated data', xpd=TRUE, adj=0)
      lines(usr[2]+facx*(x0*1.5) ,usr[4] - y0*3,
            col = adjustcolor(2,alpha.f = 0.7, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
            bg = adjustcolor(2,alpha.f = 0.7), 
            pch=21, xpd=TRUE, type='p', cex=2)
      lines(usr[2]+facx*(x0*1.5) ,usr[4] - y0*5,
            col= adjustcolor(4,alpha.f = 0.7, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
            bg = adjustcolor(4,alpha.f = 0.7), 
            pch=21, xpd=TRUE, type='p', cex=2)
      lines(usr[2]+facx*(x0*1.5) ,usr[4] - y0*7,
            col = adjustcolor(3,alpha.f = 0.7, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
            bg = adjustcolor(3,alpha.f = 0.7), 
            pch=21, xpd=TRUE, type='p', cex=2)
      text(usr[2]+facx*(0.5+x0*1.5) ,usr[4] - y0*3, paste('Sending countries'), cex=0.7, adj=0, xpd=TRUE)
      text(usr[2]+facx*(0.5+x0*1.5) ,usr[4] - y0*5, paste('Receiving countries'), cex=0.7, adj=0, xpd=TRUE)
      text(usr[2]+facx*(0.5+x0*1.5) ,usr[4] - y0*7, paste('Receiving countries (LFS)'), cex=0.7, adj=0, xpd=TRUE)
    }
    sy=-5
    lines(c(usr[2]+facx*(x0*0.5),usr[2]+facx*(x0*0.5+1)), rep(usr[4] - y0*(32+sy),2), lwd=2, col=col1, xpd=TRUE)
    if (m1!=m2) lines(c(usr[2]+facx*(x0*0.5),usr[2]+facx*(x0*0.5+1)), rep(usr[4] - y0*(36.5+sy+(m1>4)*1.5),2), lwd=2, col=col2, xpd=TRUE)
    
    if (m1==m2) text(usr[2]+facx*(x0*0.5), usr[4] - y0*(30.5+sy), 'Model', xpd=TRUE, adj=0) else text(usr[2]+facx*(x0*0.5), usr[4] - y0*(30.5+sy), 'Models', xpd=TRUE, adj=0)
    text(usr[2]+facx*(1.5+x0*0.5) ,usr[4] - y0*(32+sy-0.5), paste('#1',MODELS2[m1]), cex=0.8, adj=c(0,1), xpd=TRUE)
    if (m1!=m2) text(usr[2]+facx*(1.5+x0*0.5) ,usr[4] - y0*(36.5+sy+(m1>4)*1.5-0.5), paste('#2',MODELS2[m2]), cex=0.8, adj=c(0,1), xpd=TRUE)
  }
}


get_ymax_raw <- function(cntr_sen='PL',
                         cntr_rec='DE',
                         m1=1,
                         m2=2,
                         flow = "pred"
){
  #print(cntr_sen)
  #print(cntr_rec)
  #print(m1)
  #print(m2)
  #print(flow)
  MO1<-switch(paste(m1), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  MO2<-switch(paste(m2), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  
  linetype=3;  max.cex.lfs=3 ;  max.cex.rec=5 ;  max.cex.sen=5;  max.alfa.lfs=1
  M1<-prepare_data(MO1, cntr_sen, cntr_rec, flow=flow,
                   max.cex.lfs=max.cex.lfs, max.cex.rec=max.cex.rec, max.cex.sen=max.cex.sen,
                   max.alfa.lfs=0.5,
                   pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22))
  M2<-prepare_data(MO2, cntr_sen, cntr_rec, flow=flow,
                   max.cex.lfs=max.cex.lfs, max.cex.rec=max.cex.rec, max.cex.sen=max.cex.sen,
                   max.alfa.lfs=0.5,
                   pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22))
  YLIM<-range(M1$r_sen,M2$r_sen,M1$r_rec,M2$r_rec,M1$r_lfs,M2$r_lfs, M1$Flow05,M2$Flow05,M1$Flow95,M2$Flow95, na.rm=TRUE)
  as.numeric(YLIM[2])/1000
}

get_single_flows <- function(
                        cntr_sen='PL',
                        cntr_rec='DE',
                        m1=1,
                        m2=2,
                        flow = "pred"
                        
                        
){
  m1=as.numeric(m1)
  m2=as.numeric(m2)
  MO1<-switch(paste(m1), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  MO2<-switch(paste(m2), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  
  M1<-prepare_data_save(MO1, cntr_sen, cntr_rec, flow=flow)
  M2<-prepare_data_save(MO2, cntr_sen, cntr_rec, flow=flow)
  M1$model<-letters[as.integer(m1)]
  M2$model<-letters[as.integer(m2)]
  #print(paste('>>>D>>>',m1,m2))
  list(M1=M1,M2=M2)
}

plot_models <- function(cntr_sen='PL',
                        cntr_rec='DE',
                        m1=1,
                        m2=2,
                        MO1=NULL,
                        MO2=NULL,
                        linetype=3,
                        max.cex.lfs=3, 
                        max.cex.rec=5, 
                        max.cex.sen=5,
                        max.alfa.lfs=1,
                        flow = "pred",
                        spaceX = 16,
                        col1 = 'orange2',
                        col2 = 'deepskyblue2',
                        alpha.75 = 0.3,
                        alpha.95 = 0.1,
                        MODELS2 = NULL,
                        pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22),
                        setYmax = FALSE,
                        Ymax = 1000,
                        plotLegend = TRUE
                        
){
  m1=as.numeric(m1)
  m2=as.numeric(m2)
  if (m1==m2) col1<-col2<-'black'
  #print(setYmax)
  #print(Ymax)
  ##print(ls())
  MO1<-switch(paste(m1), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  MO2<-switch(paste(m2), 
              '1' = Data_input_80a, 
              '2' = Data_input_80b,
              '3' = Data_input_80c,
              '4' = Data_input_80d,
              '5' = Data_input_80e,
              '6' = Data_input_80f)
  
  plot_LFS <- (as.integer(m1)<5) |  (as.integer(m2)<5)
  
  M1<-prepare_data(MO1, cntr_sen, cntr_rec, flow=flow,
                   max.cex.lfs=max.cex.lfs, max.cex.rec=max.cex.rec, max.cex.sen=max.cex.sen,
                   max.alfa.lfs=0.5,
                   pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22))
  M2<-prepare_data(MO2, cntr_sen, cntr_rec, flow=flow,
                   max.cex.lfs=max.cex.lfs, max.cex.rec=max.cex.rec, max.cex.sen=max.cex.sen,
                   max.alfa.lfs=0.5,
                   pch.values=c('0-2'=23, '3'=25, '6'=24, '8-12'=21, 'P'=22))

  YLIM<-range(M1$r_sen,M2$r_sen,M1$r_rec,M2$r_rec,M1$r_lfs,M2$r_lfs, M1$Flow05,M2$Flow05,M1$Flow95,M2$Flow95, na.rm=TRUE)
  if (setYmax) YLIM[2] <- Ymax*1000
  XLIM<-range(M1$year,M2$year)
  if (linetype==2) XLIM[2]<-XLIM[2]+1
  Z<-par('mar')
  #Z[4]<-spaceX
  Z[3]<-3
  if (plotLegend) Z[4]<-spaceX else {
    mid<-(spaceX + Z[2])/2 
    Z[4]<- mid-2
    Z[2]<- mid+2
  }
  #if (plotTitle) Z[3]<- 3 else Z[3]<-0.6
  
  par(mar=Z)
  plot(M1$year, M1$r_sen/1000, ylim=c(0,YLIM[2])/1000, xlim=XLIM, type='n', axes='F',
       xlab='',ylab='')
  mtext(expression(bold(Year)),1,3.5, cex=1.2)
  mtext(expression(bold('Flows (in 1,000)')),2,2.2,cex=1.2)
  axis(1,at=XLIM[1]:XLIM[2], las=3)
  abline(v=1950:2050, h=axis(2), lty=2, col='gray')
  box()
  
  if (linetype==3){
    M1L<-spline(M1$year, M1$Flow50/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L<-spline(M2$year, M2$Flow50/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L25<-spline(M1$year, M1$Flow25/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L25<-spline(M2$year, M2$Flow25/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L75<-spline(M1$year, M1$Flow75/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L75<-spline(M2$year, M2$Flow75/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L05<-spline(M1$year, M1$Flow05/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L05<-spline(M2$year, M2$Flow05/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    M1L95<-spline(M1$year, M1$Flow95/1000, xout=seq(min(M1$year),max(M1$year), length.out=100))
    M2L95<-spline(M2$year, M2$Flow95/1000, xout=seq(min(M2$year),max(M2$year), length.out=100))
    
    polygon(c(M1L25$x,rev(M1L75$x)),c(M1L25$y,rev(M1L75$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.75))
    polygon(c(M1L05$x,rev(M1L95$x)),c(M1L05$y,rev(M1L95$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.95))
    if (m1!=m2) {
      polygon(c(M2L25$x,rev(M2L75$x)),c(M2L25$y,rev(M2L75$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.75))
      polygon(c(M2L05$x,rev(M2L95$x)),c(M2L05$y,rev(M2L95$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.95))
    }
    
    lines(M1L, lwd=3, col=adjustcolor(col1,alpha.f = 0.9))
    if (m1!=m2) lines(M2L, lwd=3, col=adjustcolor(col2,alpha.f = 0.9))
  } else if (linetype==2){
    M1L<-approx(M1$year, M1$Flow50/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L<-approx(M2$year, M2$Flow50/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L25<-approx(M1$year, M1$Flow25/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L25<-approx(M2$year, M2$Flow25/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L75<-approx(M1$year, M1$Flow75/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L75<-approx(M2$year, M2$Flow75/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L05<-approx(M1$year, M1$Flow05/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L05<-approx(M2$year, M2$Flow05/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    M1L95<-approx(M1$year, M1$Flow95/1000, xout=seq(min(M1$year),max(M1$year)+1, length.out=1000), method = 'constant',rule=2)
    M2L95<-approx(M2$year, M2$Flow95/1000, xout=seq(min(M2$year),max(M2$year)+1, length.out=1000), method = 'constant',rule=2)
    
    polygon(c(M1L25$x,rev(M1L75$x)),c(M1L25$y,rev(M1L75$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.75))
    polygon(c(M1L05$x,rev(M1L95$x)),c(M1L05$y,rev(M1L95$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.95))
    if (m1!=m2) {
      polygon(c(M2L25$x,rev(M2L75$x)),c(M2L25$y,rev(M2L75$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.75))
      polygon(c(M2L05$x,rev(M2L95$x)),c(M2L05$y,rev(M2L95$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.95))
    }
    
    lines(M1L, lwd=3, col=adjustcolor(col1,alpha.f = 0.9))
    if (m1!=m2) lines(M2L, lwd=3, col=adjustcolor(col2,alpha.f = 0.9))
    #lines(M1$year, M1$Flow50/1000, lwd=2, col=adjustcolor(col1,alpha.f = 0.8),type='s')
    #lines(M2$year, M2$Flow50/1000, lwd=2, col=adjustcolor(col2,alpha.f = 0.8),type='s')
  } else if (linetype==1){
    M1L<-approx(M1$year, M1$Flow50/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L<-approx(M2$year, M2$Flow50/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L25<-approx(M1$year, M1$Flow25/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L25<-approx(M2$year, M2$Flow25/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L75<-approx(M1$year, M1$Flow75/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L75<-approx(M2$year, M2$Flow75/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L05<-approx(M1$year, M1$Flow05/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L05<-approx(M2$year, M2$Flow05/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    M1L95<-approx(M1$year, M1$Flow95/1000, xout=seq(min(M1$year),max(M1$year), length.out=100), method = 'linear')
    M2L95<-approx(M2$year, M2$Flow95/1000, xout=seq(min(M2$year),max(M2$year), length.out=100), method = 'linear')
    
    polygon(c(M1L25$x,rev(M1L75$x)),c(M1L25$y,rev(M1L75$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.75))
    polygon(c(M1L05$x,rev(M1L95$x)),c(M1L05$y,rev(M1L95$y)), border=NA, col=adjustcolor(col1,alpha.f = alpha.95))
    
    if (m1!=m2) {
      polygon(c(M2L25$x,rev(M2L75$x)),c(M2L25$y,rev(M2L75$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.75))
      polygon(c(M2L05$x,rev(M2L95$x)),c(M2L05$y,rev(M2L95$y)), border=NA, col=adjustcolor(col2,alpha.f = alpha.95))
    }
    
    #lines(M1$year, M1$Flow50/1000, lwd=2, col=adjustcolor(col1,alpha.f = 0.8),type='l')
    #if (m1!=m2) lines(M2$year, M2$Flow50/1000, lwd=2, col=adjustcolor(col2,alpha.f = 0.8),type='l')
    lines(M1L, lwd=3, col=adjustcolor(col1,alpha.f = 0.9))
    if (m1!=m2) lines(M2L, lwd=3, col=adjustcolor(col2,alpha.f = 0.9))
    
  }
  lines(M1$year+0.5, M1$r_sen/1000, ylim=c(0,YLIM[2])/1000, 
        col='white',
        bg='white',
        pch=M1$pch_sen, 
        cex=(M1$cex_sen-1)/2+1, type='p')
  lines(M1$year+0.5, M1$r_rec/1000, ylim=c(0,YLIM[2])/1000, 
        col='white', 
        bg='white', 
        pch=M1$pch_rec, 
        cex=(M1$cex_rec-1)/2+1, type='p')
  if (plot_LFS) lines(M1$year+0.5, M1$r_lfs/1000, ylim=c(0,YLIM[2])/1000, 
                      col='white', 
                      bg = 'white', 
                      pch=M1$pch_lfs, 
                      cex=(M1$cex_lfs-1)/2+1, type='p')
  
  lines(M1$year+0.5, M1$r_sen/1000, ylim=c(0,YLIM[2])/1000, 
        col=sapply(M1$alpha_sen, function(a) adjustcolor(2,alpha.f = a, red.f = 0.8, green.f = 0.8, blue.f = 0.8)),
        bg=sapply(M1$alpha_sen, function(a) adjustcolor(2,alpha.f = a)),
        pch=M1$pch_sen, 
        cex=(M1$cex_sen-1)/2+1, type='p')
  lines(M1$year+0.5, M1$r_rec/1000, ylim=c(0,YLIM[2])/1000, 
        col=sapply(M1$alpha_rec, function(a) adjustcolor(4,alpha.f = a, red.f = 0.8, green.f = 0.8, blue.f = 0.8)), 
        bg=sapply(M1$alpha_rec, function(a) adjustcolor(4,alpha.f = a)), 
        pch=M1$pch_rec, 
        cex=(M1$cex_rec-1)/2+1, type='p')
  
  if (plot_LFS) lines(M1$year+0.5, M1$r_lfs/1000, ylim=c(0,YLIM[2])/1000, 
                      col=sapply(M1$alpha_lfs, function(a) adjustcolor(3,alpha.f = a, red.f = 0.8, green.f = 0.8, blue.f = 0.8)), 
                      bg = sapply(M1$alpha_lfs, function(a) adjustcolor(3,alpha.f = a)), 
                      pch=M1$pch_lfs, 
                      cex=(M1$cex_lfs-1)/2+1, type='p')
  
  
  #mark accession !!!
  top_text(paste0('Migration flows from ',cntr_sen,' to ',cntr_rec), fill='lightgray')
  
  usr<-par('usr')
  size=0.05
  usr[4]<-usr[4]+size*diff(usr[3:4])
  usr[2]<-usr[2]+0.25
  x0<-max(sapply(c(letters,LETTERS),strwidth))
  y0<-max(sapply(c(letters,LETTERS),strheight))
  if (linetype==2) facx<-(length(unique(M1$year))+1)/length(unique(M1$year)) else facx<-1
  
  if (plotLegend) {
    #rect(usr[2],usr[3],usr[2]+max(sapply(c(letters,LETTERS),strwidth))*10,usr[4],xpd=TRUE,lwd=1.5, col=0, border=1)
    text(usr[2]+(x0*0.5)*facx, usr[4] - y0, 'Accuracy and data source', xpd=TRUE, adj=0)
    text(usr[2]+(0:2+x0*1.5)*facx ,rep(usr[4] - y0*2.6,3), c('low','med','high'), xpd=TRUE, cex=0.7, adj=c(0.5,0))
    lines(usr[2]+(0:2+x0*1.5)*facx ,rep(usr[4] - y0*4,3),
          col=sapply((1:3)/3, function(a) adjustcolor(2,alpha.f = a, red.f = 0.8, green.f = 0.8, blue.f = 0.8)), 
          bg = sapply((1:3)/3, function(a) adjustcolor(2,alpha.f = a)), 
          pch=20, xpd=TRUE, type='p', cex=3)
    lines(usr[2]+(0:2+x0*1.5)*facx ,rep(usr[4] - y0*6,3),
          col=sapply((1:3)/3, function(a) adjustcolor(4,alpha.f = a, red.f = 0.8, green.f = 0.8, blue.f = 0.8)), 
          bg = sapply((1:3)/3, function(a) adjustcolor(4,alpha.f = a)), 
          pch=20, xpd=TRUE, type='p', cex=3)
    if (plot_LFS) lines(usr[2]+(c(0:1)+x0*1.5)*facx ,rep(usr[4] - y0*8,2),
                        col=sapply((1:3)/3, function(a) adjustcolor(3,alpha.f = a, red.f = 0.8, green.f = 0.8, blue.f = 0.8)), 
                        bg = sapply((1:3)/3, function(a) adjustcolor(3,alpha.f = a)), 
                        pch=20, xpd=TRUE, type='p', cex=3)
    text(usr[2]+(2.5+x0*1.5)*facx ,usr[4] - y0*4, paste(cntr_sen,'(sending)'), cex=0.7, adj=0, xpd=TRUE)
    text(usr[2]+(2.5+x0*1.5)*facx ,usr[4] - y0*6, paste(cntr_rec,'(receiving)'), cex=0.7, adj=0, xpd=TRUE)
    if (plot_LFS) text(usr[2]+(2.5+x0*1.5)*facx ,usr[4] - y0*8, paste(cntr_rec,'(LFS, receiving)'), cex=0.7, adj=0, xpd=TRUE)
    
    sy=3
    text(usr[2]+(x0*0.5)*facx, usr[4] - y0*(7.5+sy), 'Undercounting and duration of stay', xpd=TRUE, adj=0)
    text(usr[2]+(0:4+x0*1.5)*facx ,rep(usr[4] - y0*(8.8+sy),5), c('v. high','high','med','low','v. low'), xpd=TRUE, cex=0.7, adj=c(0.5,1))
    
    lines(usr[2]+(0:4+x0*1.5)*facx ,rep(usr[4] - y0*(11+sy),5),
          col= adjustcolor(1,alpha.f = 2/3, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg = adjustcolor(1,alpha.f = 2/3), 
          pch=pch.values['0-2'], xpd=TRUE, type='p', cex=rev(((5-1:5)*(max.cex.sen-1)/4)/2+1)
    )
    lines(usr[2]+(0:4+x0*1.5)*facx ,rep(usr[4] - y0*(14+sy),5),
          col= adjustcolor(1,alpha.f = 2/3, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg = adjustcolor(1,alpha.f = 2/3), 
          pch=pch.values['3'], xpd=TRUE, type='p', cex=rev(((5-1:5)*(max.cex.sen-1)/4)/2+1)
    )
    lines(usr[2]+(0:4+x0*1.5)*facx ,rep(usr[4] - y0*(17+sy),5),
          col= adjustcolor(1,alpha.f = 2/3, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg = adjustcolor(1,alpha.f = 2/3), 
          pch=pch.values['6'], xpd=TRUE, type='p', cex=rev(((5-(1:5))*(max.cex.sen-1)/4)/2+1)
    )
    lines(usr[2]+(0:4+x0*1.5)*facx ,rep(usr[4] - y0*(20+sy),5),
          col= adjustcolor(1,alpha.f = 2/3, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg = adjustcolor(1,alpha.f = 2/3), 
          pch=pch.values['8-12'], xpd=TRUE, type='p', cex=rev(((5-1:5)*(max.cex.sen-1)/4)/2+1)
    )
    lines(usr[2]+(0:4+x0*1.5)*facx ,rep(usr[4] - y0*(23+sy),5),
          col= adjustcolor(1,alpha.f = 2/3, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
          bg = adjustcolor(1,alpha.f = 2/3), 
          pch=pch.values['P'], xpd=TRUE, type='p', cex=rev(((5-1:5)*(max.cex.sen-1)/4)/2+1)
    )
    text(usr[2]+(4.5+x0*1.5)*facx ,usr[4] - y0*(11+sy), '0-2 months', cex=0.7, adj=0, xpd=TRUE)
    text(usr[2]+(4.5+x0*1.5)*facx ,usr[4] - y0*(14+sy), '3 months', cex=0.7, adj=0, xpd=TRUE)
    text(usr[2]+(4.5+x0*1.5)*facx ,usr[4] - y0*(17+sy), '6 months', cex=0.7, adj=0, xpd=TRUE)
    text(usr[2]+(4.5+x0*1.5)*facx ,usr[4] - y0*(20+sy), '8-12 months', cex=0.7, adj=0, xpd=TRUE)
    text(usr[2]+(4.5+x0*1.5)*facx ,usr[4] - y0*(23+sy), 'Permanent', cex=0.7, adj=0, xpd=TRUE)
    
    if (plot_LFS) {
      text(usr[2]+(x0*0.5)*facx, usr[4] - y0*(25.25+sy), 'Undercounting of LFS data', xpd=TRUE, adj=0)
      lines(usr[2]+(0:1+x0*1.5)*facx ,rep(usr[4] - y0*(28.5+sy),2),
            col= adjustcolor(3,alpha.f = 2/3, red.f = 0.8, green.f = 0.8, blue.f = 0.8), 
            bg = adjustcolor(3,alpha.f = 2/3), 
            pch=pch.values['8-12'], xpd=TRUE, type='p', 
            cex=rev((1-(0:1))*(max.cex.lfs-1))/2+1 )
      text(usr[2]+(0:1+x0*1.5)*facx ,rep(usr[4] - y0*(27.25+sy),2), c('high','low'), xpd=TRUE, cex=0.7, adj=c(0.5,0))
    }
    
    lines(c(usr[2]+(x0*0.5*facx),usr[2]+(x0*0.5+1)*facx), rep(usr[4] - y0*(32+sy),2), lwd=2, col=col1, xpd=TRUE)
    if (m1!=m2) lines(c(usr[2]+(x0*0.5*facx),usr[2]+(x0*0.5+1)*facx), rep(usr[4] - y0*(36.5+sy+(m1>4)*1.5),2), lwd=2, col=col2, xpd=TRUE)
    
    if (m1==m2) text(usr[2]+(x0*0.5*facx), usr[4] - y0*(30.5+sy), 'Model', xpd=TRUE, adj=0) else text(usr[2]+(x0*0.5*facx), usr[4] - y0*(30.5+sy), 'Models', xpd=TRUE, adj=0)
    #print(MODELS2)
    #print(m1)
    #print(MODELS2[m1])
    text(usr[2]+(1.5+x0*0.5)*facx ,usr[4] - y0*(32+sy-0.5), paste('#1',MODELS2[m1]), cex=0.8, adj=c(0,1), xpd=TRUE)
    if (m1!=m2) text(usr[2]+(1.5+x0*0.5) ,usr[4] - y0*(36.5+sy+(m1>4)*1.5-0.5), paste('#2',MODELS2[m2]), cex=0.8, adj=c(0,1), xpd=TRUE)
  }
}

na2zero<-function(x) {x[is.na(x)]<-0; x}

factor.matrix<-function(mat, levels){
  rn<-rownames(mat); cn<-colnames(mat); d<-dim(mat)
  res<-factor(mat, levels)
  dim(res)<-d; rownames(res)<-rn; colnames(res)<-cn
  res
}

as.numeric.matrix<-function(mat){
  rn<-rownames(mat); cn<-colnames(mat); d<-dim(mat)
  res<-as.numeric(mat)
  dim(res)<-d; rownames(res)<-rn; colnames(res)<-cn
  res
}

cut_matrix<-function(mat, breaks, labels = NULL, include.lowest = FALSE){
  rn<-rownames(mat); cn<-colnames(mat); d<-dim(mat)
  res<-base:::cut(mat, breaks=breaks, labels=labels, include.lowest=include.lowest)
  dim(res)<-d; rownames(res)<-rn; colnames(res)<-cn
  res
}

level2num<-function(x, LEVELS, bounds=NULL){
  mat<-as.numeric.matrix(factor.matrix(x,LEVELS))
  if (!is.null(bounds)) {
    num<-seq(bounds[1],bounds[2],length.out=length(LEVELS))
    for (k in seq_along(LEVELS)) mat[paste(mat)==paste(k)]<-num[k]
  }
  mat
}

my2dplot<-function(mat, LEVELS, txtLEVELS = paste(LEVELS), namat=NULL, cexx=1, cexy=1, lox=1, loy=1,
                   groups=length(LEVELS),colors=rev(brewer.pal(n = groups, name = "Spectral"))[1:groups],
                   nodata='Missing data', naalpha=0.2, diagwhite=FALSE, legoffs=0){
  if (length(LEVELS)!=groups) stop('Nb of levels and groups differ.')
  if (min(mat, na.rm = TRUE)>=1 && max(mat, na.rm = TRUE)<=length(LEVELS) && all(unlist(mat)%in%c(NA,seq_along(LEVELS)))) {
    #print('numeric')
  } else {
    ThreshCol = seq(min(mat-1,na.rm = TRUE),max(mat+1,na.rm = TRUE), length.out=groups+1)
    mat<-cut_matrix(mat,breaks=ThreshCol,labels=FALSE,include.lowest = TRUE)
  } 
  dimm<-dim(mat)
  rn<-rownames(mat)
  cn<-colnames(mat)
  lcolors<-adjustcolor(colors,alpha.f = naalpha)
  plot(NA,NA, xlim=c(1-0.5,length(cn)+0.5), ylim=c(1-0.5,length(rn)+0.5),
       xaxs='i',yaxs='i',axes=FALSE, xlab='',ylab='')
  axis(1, at=seq_along(cn), cn,las=3, cex.axis=cexx)
  axis(2, at=seq_along(rn), rn,las=1, cex.axis=cexy)
  box()
  for(cl in seq_along(cn)) for (ro in seq_along(rn)){
    if (diagwhite && ro==cl){
      # do nothing
    } else if (is.na(mat[ro,cl])) {
      lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
      lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
    } else {
      if (!is.null(namat) && namat[ro,cl]){
        rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=lcolors[mat[ro,cl]])
        lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
        lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
      } else
        rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
    }
  }
  if (length(namat)) {
    # l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors,NA), border = NA, legend = c(txtLEVELS,nodata))
    # legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE,cex=1.45)
    l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors,NA), border = NA, legend = c(txtLEVELS,''))
    legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE, cex=1.45)
    legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))+legoffs,xpd=TRUE,bty='n',fill=NA, border = NA, legend = c(nodata))
  } else {
    legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors), border = NA, legend = txtLEVELS)
  }
}

get_src_stats<-function(direction='E'){
  if (direction=='E') DAA<-rsm.data$x.emi.src else DAA<-rsm.data$x.imm.src
  res<-table(DAA, useNA = 'always')
  names(res)[is.na(names(res))]<-'Unknown'
  res['Unknown']<-res['Unknown']-dim(DAA)[1]*dim(DAA)[3]
  res
}

get_lfs_stats<-function(){
  DAA<-rsm.data$k.imm
  if (any(diag(DAA[,,1])!=0)) stop()
  res<-table(is.na(DAA))
  names(res)<-c('Known','Unknown')
  res['Known']<-res['Known']-dim(DAA)[1]*dim(DAA)[3] #diag is 0
  res
}

plot_src_summary<-function (data){
  srcpalette<- c(EUROSTAT='green3',UN='blue2',DE_NSO='orange2',UK_NSO='red2',IMEM='yellow',Unknown='white')
  srcpalette<-srcpalette[names(data)]
  labels <- paste(names(data), ':  ',sprintf("%.1f%%", data / sum(data) * 100), " (", data, ")", sep = "")
  par(mar=c(2,0,0,0))
  pie(data, main = NULL, col = srcpalette, labels = names(data), cex = 1)
  legend("bottomleft", legend = labels, ncol=3, bty = "n", cex = 1, fill = srcpalette,
         xpd=TRUE,inset = c(0, -0.05))
  
}

plot_lfs_stats<-function (){
  data <- get_lfs_stats()
  srcpalette<- c(Known='green3',Unknown='white')
  srcpalette<-srcpalette[names(data)]
  labels <- paste(names(data), ':  ',sprintf("%.1f%%", data / sum(data,na.rm = TRUE) * 100), " (", data, ")", sep = "")
  par(mar=c(2,0,0,0))
  pie(data, main = NULL, col = srcpalette, labels = names(data), cex = 1)
  legend("bottomleft", legend = labels, ncol=3, bty = "n", cex = 1, fill = srcpalette,
         xpd=TRUE,inset = c(0, -0.05))
  
}


plot_Sources<-function(direction='E',year=2002,cexx=1, cexy=1, lox=1, loy=1,NoDataTxt='Missing data') {
  if (direction=='E') DAA<-rsm.data$x.emi.src else DAA<-rsm.data$x.imm.src
  
  LEVELS<-paste(unique(as.vector(c(rsm.data$x.imm.src,rsm.data$x.emi.src))))
  #colors=paletteer::paletteer_d("ggsci::hallmarks_light_cosmic")[-(1:3)]
  DAAY<-DAA[,,paste(year)]
  diag(DAAY)<-' '
  colors<-c('black','green3','blue2','orange2','red2','yellow','white')
  DAAY[is.na(DAAY)]<-''
  LEVELS[LEVELS=='NA']<-''
  mat=toNuM(DAAY, c(LEVELS,' '))
  rn<-rownames(mat)
  cn<-colnames(mat)
  #lcolors<-adjustcolor(colors,alpha.f = naalpha)
  plot(NA,NA, xlim=c(1-0.5,length(cn)+0.5), ylim=c(1-0.5,length(rn)+0.5),
       xaxs='i',yaxs='i',axes=FALSE, xlab='',ylab='')
  axis(1, at=seq_along(cn), cn,las=3, cex.axis=cexx)
  axis(2, at=seq_along(rn), rn,las=1, cex.axis=cexy)
  box()
  for(cl in seq_along(cn)) for (ro in seq_along(rn)){
    if ((mat[ro,cl])==1) {
      lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4),col=1)
      lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4),col=1)
    } else {
      rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
    }
  }
  if (direction=='E') {
    mtext(expression(bold('Receiving country')),1,2.5,cex=1.1)
    mtext(expression(bold('Sending (reporting) country')),2,2.5,cex=1.1)
  } else if (direction=='I') {
    mtext(expression(bold('Receiving (reporting) country')),1,2.5,cex=1.1)
    mtext(expression(bold('Sending country')),2,2.5,cex=1.1)
  } 
  #if (length(namat)) {
  l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors[-1],NA), border = NA, legend = c(LEVELS[-1],NoDataTxt))
  legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE,cex=1.45)
}  

get_empty<-function(direction){
  if (direction=='E') {
    DF<-rsm.data$x.emi.src 
    Empty<-apply(DF, c(1,3), function(k) all(is.na(k)))
  } else if (direction=='I'){
    DF<-rsm.data$x.imm.src
    Empty<-apply(DF, c(2,3), function(k) all(is.na(k)))
  } else stop()
  Empty
}

remove_empty<-function(v,direction){
  em<-get_empty(direction)
  v[em]<-paste0('Imputed ',v[em])
  v
}

empty2NA<-function(v,direction){
  em<-get_empty(direction)
  v[em]<-NA
  v
}

FormatUnder <- function(data,direction){
  dn <- dimnames(data)
  di <- dim(data)
  tmp<-remove_empty(c('Very low','Low','Medium','High','Very high')[data],direction)
  dim(tmp)<-di
  dimnames(tmp)<-dn
  tmp
}

plot_Undercounting<-function(direction){
  if (direction=='E') udata<-rsm.data$U4_E.r else udata<-rsm.data$U4_I.r
  #udata<-empty2NA(udata, direction)
  em<-get_empty(direction)
  my2dplot(udata,LEVELS=1:5, c('Very low','Low','Medium','High','Very high'), namat = em, naalpha = 0)
}

FormatAccu <- function(data,direction){
  dn <- dimnames(data)
  di <- dim(data)
  tmp<-remove_empty(c('High','Medium','Low')[data],direction)
  dim(tmp)<-di
  dimnames(tmp)<-dn
  tmp
}

plot_Accuracy<-function(direction='E',adjusted=FALSE, showNA=TRUE, cexx=1, cexy=1, lox=1, loy=1) {
  if (adjusted) ACU<-rsm.data_updated else ACU<-rsm.data
  if (direction=='E') DAA<-ACU$A_E.r else DAA<-ACU$A_I.r
  
  LEVELS<-sort(paste(unique(as.vector(c(ACU$A_I.r,ACU$A_E.r)))))
  #colors=paletteer::paletteer_d("ggsci::hallmarks_light_cosmic")[-(1:3)]
  
  colors<-c('green4','orange','red3')
  mat=toNuM(DAA, c(LEVELS))
  rn<-rownames(mat)
  cn<-colnames(mat)
  #lcolors<-adjustcolor(colors,alpha.f = naalpha)
  plot(NA,NA, xlim=c(1-0.5,length(cn)+0.5), ylim=c(1-0.5,length(rn)+0.5),
       xaxs='i',yaxs='i',axes=FALSE, xlab='',ylab='')
  axis(1, at=seq_along(cn), cn,las=3, cex.axis=cexx)
  axis(2, at=seq_along(rn), rn,las=1, cex.axis=cexy)
  box()
  if (showNA){
    Empty <- get_empty(direction)
    for(cl in seq_along(cn)) for (ro in seq_along(rn)){
      if ((Empty[ro,cl])==1) {
        #rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=adjustcolor(colors[mat[ro,cl]],alpha.f = 0.2))
        lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4),col=1)
        lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4),col=1)
      } else rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
    }
  } else {
    for(cl in seq_along(cn)) for (ro in seq_along(rn)){
      rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
    }
  }
  if (direction=='E') {
    mtext(expression(bold('Sending country')),2,2.5,cex=1.1)
  } else if (direction=='I') {
    mtext(expression(bold('Receiving country')),2,2.5,cex=1.1)
  } 
  #if (length(namat)) {
  l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors,NA), border = NA, legend = c('High','Medium','Low','Missing data'))
  legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE,cex=1.45)
}  


# plot_duration<-function(DURATION, LEVELS=sortdur(unique(as.vector(unlist(DURATION)))),
#                         AllLevels=LEVELS, cexx=1, cexy=1, lox=1, loy=1, colors=paletteer::paletteer_d("ggsci::hallmarks_light_cosmic")){
#   #naalpha=0.2
#   #cexx=1; cexy=1; #lox=1; loy=1
#   #namat=NULL
#   LEVELS<-sortdur(unique(as.vector(unlist(DURATION))))
#   groups=length(AllLevels)
#    #c("#000000FF",paletteer_d("ggthemes::Hue_Circle",groups-1, type='continuous'))
#   #class(colors)<-'colors'
#   colindx<-AllLevels%in%LEVELS
#   LEVELS[LEVELS=='-1']<-'no data'
#   LEVELS[LEVELS=='60']<-'permanent'
#   
#   mat=toNuM(DURATION, AllLevels)
#   rn<-rownames(mat)
#   cn<-colnames(mat)
#   #lcolors<-adjustcolor(colors,alpha.f = naalpha)
#   plot(NA,NA, xlim=c(1-0.5,length(cn)+0.5), ylim=c(1-0.5,length(rn)+0.5),
#        xaxs='i',yaxs='i',axes=FALSE, xlab='',ylab='')
#   axis(1, at=seq_along(cn), cn,las=3, cex.axis=cexx)
#   axis(2, at=seq_along(rn), rn,las=1, cex.axis=cexy)
#   box()
#   for(cl in seq_along(cn)) for (ro in seq_along(rn)){
#     if ((mat[ro,cl])==1) {
#       lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
#       lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
#     } else {
#       # if
#       # (!is.null(namat) && namat[ro,cl]){
#       #   rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=lcolors[mat[ro,cl]])
#       #   lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
#       #   lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
#       # } else
#       rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
#     }
#   }
#   #if (length(namat)) {
#   l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors[colindx][-1],NA), border = NA, legend = c(LEVELS[-1],'no data'))
#   legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE,cex=1.45)
# }

my2dplotClassify<-function(mat, ncat=20, digits=2, rr=seq(min(unlist(mat),na.rm = TRUE), max(unlist(mat), na.rm=T), length.out=ncat+1), labels=NULL, include_zero=TRUE, nodata='', include.lowest = TRUE, use.na=TRUE,...){
  
  dn<-dimnames(mat)
  #print(dn)
  di<-dim(mat)
  #print(di)
  mat <- unlist(mat)
  mat <- round(mat,digits)
  if (include_zero) if (rr[1]>0) rr[1]<-0
  tmat<-cut(mat,rr, labels=labels, include.lowest = include.lowest)  
  #print(table(tmat))
  ntmat<-as.numeric(tmat)
  #print(table(ntmat))
  dim(ntmat)<-di
  dimnames(ntmat)<-dn
  if (use.na) namat<-is.na(ntmat) else namat<-NULL
  my2dplot(ntmat, namat=namat, LEVELS = levels(tmat), colors=colorRampPalette(rev(brewer.pal(11, "Spectral")))(length(levels(tmat))),nodata=nodata,...)
}

toNuM<-function(df, levels=NULL){
  ddf<-dim(df)
  cn<-colnames(df)
  rn<-rownames(df)
  vec<-as.vector(unlist(df))
  if (!length(levels)) levels<-sortdur(unique(vec))
  vec<-as.numeric(factor(vec, levels = levels))
  dim(vec)<-ddf
  colnames(vec)<-cn
  rownames(vec)<-rn
  as.matrix(vec)
}

plot_duration<-function(DURATION, LEVELS=sortdur(unique(as.vector(unlist(DURATION)))),
                        AllLevels=LEVELS, cexx=1, cexy=1, lox=1, loy=1, colors=paletteer_d("ggsci::hallmarks_light_cosmic")){
  #naalpha=0.2
  #cexx=1; cexy=1; #lox=1; loy=1
  #namat=NULL
  LEVELS<-sortdur(unique(as.vector(unlist(DURATION))))
  groups=length(AllLevels)
  #c("#000000FF",paletteer_d("ggthemes::Hue_Circle",groups-1, type='continuous'))
  #class(colors)<-'colors'
  colindx<-AllLevels%in%LEVELS
  LEVELS[LEVELS=='-1']<-'Missing data'
  LEVELS[LEVELS=='60']<-'Permanent'
  
  mat=toNuM(DURATION, AllLevels)
  rn<-rownames(mat)
  cn<-colnames(mat)
  #lcolors<-adjustcolor(colors,alpha.f = naalpha)
  plot(NA,NA, xlim=c(1-0.5,length(cn)+0.5), ylim=c(1-0.5,length(rn)+0.5),
       xaxs='i',yaxs='i',axes=FALSE, xlab='',ylab='')
  axis(1, at=seq_along(cn), cn,las=3, cex.axis=cexx)
  axis(2, at=seq_along(rn), rn,las=1, cex.axis=cexy)
  box()
  for(cl in seq_along(cn)) for (ro in seq_along(rn)){
    if ((mat[ro,cl])==1) {
      lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
      lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
    } else {
      # if
      # (!is.null(namat) && namat[ro,cl]){
      #   rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=lcolors[mat[ro,cl]])
      #   lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
      #   lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
      # } else
      rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
    }
  }
  #if (length(namat)) {
  l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors[colindx][-1],NA), border = NA, legend = c(LEVELS[-1],'Missing data'))
  legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE,cex=1.45)
}

plot_circular<-function(data, year, orig, dest, alt=FALSE, link.lwd=1, border.alpha=0.5, arrow.alpha=0.5, link.arr.length=0.1, showscale=TRUE, show.big.perc=100){
  # Create example migration flow data
  
  # data <- Data_input_80a
  # 
  # orig<-c('PL','DE','UK','NL','FR','AT')
  # dest<-orig#c('PL','DE','FR')
  # year<-2010
  # alt=FALSE
  
  #ncolo<-length(unique(c(orig,dest)))
  #PAL<-palette[round(seq(1,length(palette),length.out=ncolo))]
  
  #column.col<-palette[1:length(dest)]
  #column.col<-palette[round(seq(1,length(palette),length.out=length(dest)))]
  
  
  
  data$orig<-as.factor(data$orig)
  data$dest<-as.factor(data$dest)
  #palette<-palette[round(seq(1,length(palette),length.out=length(levels(data$orig))))]
  
  data <- data[data$orig %in% orig,] 
  data <- data[data$dest %in% dest,] 
  data <- data[data$year == year, ]
  #if (alt) pred <- "pred_alt_q50" else 
  pred<-"pred_q50"
  data <-data[,c('orig','dest', pred)]
  colnames(data)<-c('orig','dest','value')
  # data$origcol<-palette[as.numeric(data$orig)]
  # data$destcol<-palette[as.numeric(data$dest)]
  # datacol<-xtabs(origcol ~ orig + dest, data = data)
  data<-xtabs(value ~ orig + dest, data = data)
  datatest<-quantile(data[data!=0],show.big.perc/100)
  #data[data<datatest]<-0
  pal<-palette[1:nrow(data)]
  names(pal)<-rownames(data)
  
  # Create Circular Migration Flow Plot
  circos.clear()
  circos.par(cell.padding = c(0, 0, 0, 0))
  if (showscale) {
    annotationTrack = c("name", "grid", "axis")
    annotationTrackHeight = mm_h(c(8, 2))
  } else {
    annotationTrack = c("name", "grid")
    annotationTrackHeight = mm_h(c(1, 2))
  }
  chordDiagram(data, 
               directional = 1, 
               direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow", 
               link.arr.length = link.arr.length, 
               #column.col = palette[1:ncol(data)],
               #row.col = palette[1:nrow(data)],
               #link.visible = data > datatest,
               transparency = 1-arrow.alpha,
               grid.col='lightgray',
               annotationTrack = annotationTrack,
               annotationTrackHeight = annotationTrackHeight,
               link.lwd = link.lwd,    # Line width
               link.lty = 1,    # Line type
               link.border = adjustcolor('black',alpha.f = border.alpha*0.1))
  par(new=TRUE)
  chordDiagram(data, 
               directional = 1, 
               direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow", 
               link.arr.length = link.arr.length, 
               #column.col = palette[1:ncol(data)],
               #row.col = palette[1:nrow(data)],
               link.visible = data >= datatest,
               transparency = 1-arrow.alpha,
               grid.col=pal,
               annotationTrack = annotationTrack,
               annotationTrackHeight = annotationTrackHeight,
               link.lwd = link.lwd,    # Line width
               link.lty = 1,    # Line type
               link.border = adjustcolor('black',alpha.f = border.alpha))
  #chordDiagram(m, directional = 1, transparency = 0.7, col=1:25, grid.col=1:5)#, col = rep(rainbow(N),N), row.col= rainbow(N), column.col= rainbow(N))
  
}

plot_lfs_undercounting<-function(z4, du, dm){
  Y<-as.numeric(colnames(du))
  plot(NA,NA,xlim=c(0.5,dim(du)[1]+0.5),ylim=c(min(Y)-0.5, max(Y)+0.5),xlab='',
       ylab='',axes=FALSE,xaxs='i',yaxs='i')
  x<-axis(1,at=seq_len(dim(du)[1]),labels = rownames(du), las=3)
  y<-axis(2,las=1,at=as.numeric(colnames(du)))
  abline(v=c(0.5,x+0.5),lty=1, col='gray',lwd=1, xpd=FALSE);
  abline(h=c(min(y)-0.5,y+0.5),lty=1, col='gray',lwd=1, xpd=FALSE)
  box()
  for(px in x){
    for(py in y){
      x_<- px-x[1]+1
      y_<- py-y[1]+1
      try(mypie(px-0.5,py-0.5,px+1-0.5,py+1-0.5,cex=0.8,
                fractions = c(z4[x_,y_],1-z4[x_,y_]), col=c(4,'white'),border=1,lty=1)
          ,silent = TRUE)
      if (is.na(dm[x_,y_]) || dm[x_,y_]==0){
        rect(px-0.5,py-0.5,px+1-0.5,py+1-0.5,border=rgb(1,0,0),lty=2)
        #lines(c(px-0.4,px+0.4),c(py-0.4,py+0.4),col=2, lwd=1.5,lty=3)
        #lines(c(px-0.4,px+0.4),c(py+0.4,py-0.4),col=2, lwd=1.5,lty=3)
      }
    }
  }
}

plot_lfs_accuracy<-function(cv.2, dm){
  Y<-as.numeric(colnames(cv.2))
  plot(NA,NA,xlim=c(0.5,dim(cv.2)[1]+0.5),ylim=c(min(Y)-0.5, max(Y)+0.5),xlab='',
       ylab='',axes=FALSE,xaxs='i',yaxs='i')
  x<-axis(1,at=seq_len(dim(cv.2)[1]),labels = rownames(cv.2), las=3)
  y<-axis(2,las=1,at=as.numeric(colnames(cv.2)))
  abline(v=c(0.5,x+0.5),lty=1, col='gray',lwd=1, xpd=FALSE);
  abline(h=c(min(y)-0.5,y+0.5),lty=1, col='gray',lwd=1, xpd=FALSE)
  box()
  for(px in x){
    for(py in y){
      x_<- px-x[1]+1
      y_<- py-y[1]+1
      try(mypie(px-0.5,py-0.5,px+1-0.5,py+1-0.5,cex=0.8,
                fractions = c(cv.2[x_,y_],1-cv.2[x_,y_]), col=c(4,'white'),border=1,lty=1)
          ,silent = TRUE)
      if (is.na(cv.2[x_,y_]) || is.na(dm[x_,y_]) || dm[x_,y_]==0){
        rect(px-0.5,py-0.5,px+1-0.5,py+1-0.5,border=rgb(1,0,0),lty=2)
        #lines(c(px-0.4,px+0.4),c(py-0.4,py+0.4),col=2, lwd=1.5,lty=3)
        #lines(c(px-0.4,px+0.4),c(py+0.4,py-0.4),col=2, lwd=1.5,lty=3)
      }
    }
  }
}