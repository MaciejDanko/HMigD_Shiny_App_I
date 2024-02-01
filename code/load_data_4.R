library(countrycode)

fill_missing_years<-function(x){
  if(length(x)) {
    all_combinations <- expand.grid(orig = unique(x$orig), dest = unique(x$dest), year = 2002:2021)
    all_combinations %>%  left_join(x, by = c("orig", "dest", "year"))
  } else x
}

fill_missing_years_a<-function(x, y=2002:2021){
  dims <- dim(x)
  k <- length(y)-dims[4]
  if(k>0) {
    extended_array <- array(NA, dim = c(dims[-length(dims)], dims[length(dims)] + k))
    extended_array[,,,(1:dims[length(dims)])] <- x
    extended_array
  } else x
}

get_data<-function(nam){
  decode_and_load_rda(nam)
  Data_input$year<-as.numeric(Data_input$year)
  Data_input
}

get_data_raw<-function(nam){
  decode_and_load_rda(nam)
  raw_int_flows
}

sortdur<-function(k){
  k<-sort(k)
  c(k[!k%in%c('12','60')],'12','60')
}

getsheetsizes<-function(xlsxcells, what='width'){
  if (what=='width') {
    sapply(1:max(xlsxcells$col), function(k) mean(xlsxcells$width[xlsxcells$col==k]))
  } else if (what=='height'){
    sapply(1:max(xlsxcells$row), function(k) mean(xlsxcells$height[xlsxcells$row==k]))
  } else stop()
}
HMigD_COLUMNS_widths<<-getsheetsizes(tidyxl::xlsx_cells('./data/HMigD_columns.xlsx',1))
HMigD_COLUMNS<<-read.xlsx('./data/HMigD_columns.xlsx')

decode_and_load_rda('./data/LFS_lim.RDA')

decode_and_load_rda('./data/input_data_23_XI_2023.RDA')
rsm.data$A_I_3.r_base[c('HU','MT','PT','GR'),]<-3
rsm.data$A_E_3.r_base[c('HU','MT','PT','GR'),]<-3
#new.rsm.data<-rsm.data
decode_and_load_rda('./data/ACCURACY_META_ADM.RDA')
META<<-META[,-c(ncol(META)-(0:1))]
META_I<<-META[,c(1:11)]
META_E<<-META[,c(1:2,12:20)]
META_I$ind_I<-paste(META_I$country,META_I$year,sep='_')
META_E$ind_E<-paste(META_E$country,META_E$year,sep='_')

suppressWarnings(rm(Data_input))
Data_input_a<<-fill_missing_years(get_data(nam='./data/DataAndFit2_34e.RDA'))
suppressWarnings(rm(Data_input))
Data_input_c<<-fill_missing_years(get_data('./data/DataAndFit2_34f.RDA'))
suppressWarnings(rm(Data_input))
Data_input_e<<-fill_missing_years(get_data('./data/DataAndFit2_34d.RDA'))
suppressWarnings(rm(Data_input))
Data_input_b<<-fill_missing_years(get_data('./data/DataAndFit2_35e.RDA'))
suppressWarnings(rm(Data_input))
Data_input_d<<-fill_missing_years(get_data('./data/DataAndFit2_35f.RDA'))
suppressWarnings(rm(Data_input))
Data_input_f<<-fill_missing_years(get_data('./data/DataAndFit2_35d.RDA'))
suppressWarnings(rm(Data_input))
gc()

if (!length(Data_input_a$pop_orig)) {
  source('./.secrets/getpass.R')
  suppressWarnings(rm(Data_input))
  Data_input_a<<-fill_missing_years(get_data(nam='./data/DataAndFit2_34e.RDA'))
  suppressWarnings(rm(Data_input))
  Data_input_c<<-fill_missing_years(get_data('./data/DataAndFit2_34f.RDA'))
  suppressWarnings(rm(Data_input))
  Data_input_e<<-fill_missing_years(get_data('./data/DataAndFit2_34d.RDA'))
  suppressWarnings(rm(Data_input))
  Data_input_b<<-fill_missing_years(get_data('./data/DataAndFit2_35e.RDA'))
  suppressWarnings(rm(Data_input))
  Data_input_d<<-fill_missing_years(get_data('./data/DataAndFit2_35f.RDA'))
  suppressWarnings(rm(Data_input))
  Data_input_f<<-fill_missing_years(get_data('./data/DataAndFit2_35d.RDA'))
  suppressWarnings(rm(Data_input))
  gc()
  
  Data_input_a$pop_orig<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_a$orig, year=Data_input_a$year)
  Data_input_b$pop_orig<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_b$orig, year=Data_input_b$year)
  Data_input_c$pop_orig<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_c$orig, year=Data_input_c$year)
  Data_input_d$pop_orig<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_d$orig, year=Data_input_d$year)
  Data_input_e$pop_orig<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_e$orig, year=Data_input_e$year)
  Data_input_f$pop_orig<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_f$orig, year=Data_input_f$year)
  
  Data_input_a$pop_dest<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_a$dest, year=Data_input_a$year)
  Data_input_b$pop_dest<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_b$dest, year=Data_input_b$year)
  Data_input_c$pop_dest<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_c$dest, year=Data_input_c$year)
  Data_input_d$pop_dest<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_d$dest, year=Data_input_d$year)
  Data_input_e$pop_dest<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_e$dest, year=Data_input_e$year)
  Data_input_f$pop_dest<- mapply(function (cntr,year) rsm.data$p_midy[cntr,paste(year)], cntr=Data_input_f$dest, year=Data_input_f$year)
  
  Data_input_a$a_imm.s_base<- rsm.data$a_imm.s_base[Data_input_a$orig]
  Data_input_b$a_imm.s_base<- rsm.data$a_imm.s_base[Data_input_b$orig]
  Data_input_c$a_imm.s_base<- rsm.data$a_imm.s_base[Data_input_c$orig]
  Data_input_d$a_imm.s_base<- rsm.data$a_imm.s_base[Data_input_d$orig]
  Data_input_e$a_imm.s_base<- rsm.data$a_imm.s_base[Data_input_e$orig]
  Data_input_f$a_imm.s_base<- rsm.data$a_imm.s_base[Data_input_f$orig]
  
  Data_input_a$a_imm.s<- rsm.data$a_imm.s[Data_input_a$orig]
  Data_input_b$a_imm.s<- rsm.data$a_imm.s[Data_input_b$orig]
  Data_input_c$a_imm.s<- rsm.data$a_imm.s[Data_input_c$orig]
  Data_input_d$a_imm.s<- rsm.data$a_imm.s[Data_input_d$orig]
  Data_input_e$a_imm.s<- rsm.data$a_imm.s[Data_input_e$orig]
  Data_input_f$a_imm.s<- rsm.data$a_imm.s[Data_input_f$orig]
  
  decode_and_load_rda(file='./data/UNDERCOUNT_MODEL_ADM.RDA',envir = .GlobalEnv)
  
  emptyU<-Countries[!Countries %in% rownames(UNDERCOUNT_DANKO_MODEL$Iraw)]
  emptyUm<-matrix(NA,nrow=length(emptyU),ncol = ncol(UNDERCOUNT_DANKO_MODEL$Iraw))
  rownames(emptyUm)<-emptyU
  UNDERCOUNT_DANKO_MODEL$Eraw<-rbind(UNDERCOUNT_DANKO_MODEL$Eraw,emptyUm)
  UNDERCOUNT_DANKO_MODEL$Iraw<-rbind(UNDERCOUNT_DANKO_MODEL$Iraw,emptyUm)
  UNDERCOUNT_DANKO_MODEL$Eimp<-rbind(UNDERCOUNT_DANKO_MODEL$Eimp,emptyUm)
  UNDERCOUNT_DANKO_MODEL$Iimp<-rbind(UNDERCOUNT_DANKO_MODEL$Iimp,emptyUm)
  colnames(UNDERCOUNT_DANKO_MODEL$Eimp)<-colnames(UNDERCOUNT_DANKO_MODEL$Iimp)<-colnames(UNDERCOUNT_DANKO_MODEL$Eraw)
  
  Data_input_a$u_a_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eraw[cntr,paste(year)], cntr=Data_input_a$orig, year=Data_input_a$year)
  Data_input_b$u_a_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eraw[cntr,paste(year)], cntr=Data_input_b$orig, year=Data_input_b$year)
  Data_input_c$u_a_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eraw[cntr,paste(year)], cntr=Data_input_c$orig, year=Data_input_c$year)
  Data_input_d$u_a_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eraw[cntr,paste(year)], cntr=Data_input_d$orig, year=Data_input_d$year)
  Data_input_e$u_a_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eraw[cntr,paste(year)], cntr=Data_input_e$orig, year=Data_input_e$year)
  Data_input_f$u_a_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eraw[cntr,paste(year)], cntr=Data_input_f$orig, year=Data_input_f$year)
  
  Data_input_a$u_a_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iraw[cntr,paste(year)], cntr=Data_input_a$dest, year=Data_input_a$year)
  Data_input_b$u_a_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iraw[cntr,paste(year)], cntr=Data_input_b$dest, year=Data_input_b$year)
  Data_input_c$u_a_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iraw[cntr,paste(year)], cntr=Data_input_c$dest, year=Data_input_c$year)
  Data_input_d$u_a_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iraw[cntr,paste(year)], cntr=Data_input_d$dest, year=Data_input_d$year)
  Data_input_e$u_a_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iraw[cntr,paste(year)], cntr=Data_input_e$dest, year=Data_input_e$year)
  Data_input_f$u_a_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iraw[cntr,paste(year)], cntr=Data_input_f$dest, year=Data_input_f$year)
 
  Data_input_a$u_i_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eimp[cntr,paste(year)], cntr=Data_input_a$orig, year=Data_input_a$year)
  Data_input_b$u_i_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eimp[cntr,paste(year)], cntr=Data_input_b$orig, year=Data_input_b$year)
  Data_input_c$u_i_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eimp[cntr,paste(year)], cntr=Data_input_c$orig, year=Data_input_c$year)
  Data_input_d$u_i_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eimp[cntr,paste(year)], cntr=Data_input_d$orig, year=Data_input_d$year)
  Data_input_e$u_i_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eimp[cntr,paste(year)], cntr=Data_input_e$orig, year=Data_input_e$year)
  Data_input_f$u_i_orig<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Eimp[cntr,paste(year)], cntr=Data_input_f$orig, year=Data_input_f$year)
  
  Data_input_a$u_i_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iimp[cntr,paste(year)], cntr=Data_input_a$dest, year=Data_input_a$year)
  Data_input_b$u_i_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iimp[cntr,paste(year)], cntr=Data_input_b$dest, year=Data_input_b$year)
  Data_input_c$u_i_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iimp[cntr,paste(year)], cntr=Data_input_c$dest, year=Data_input_c$year)
  Data_input_d$u_i_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iimp[cntr,paste(year)], cntr=Data_input_d$dest, year=Data_input_d$year)
  Data_input_e$u_i_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iimp[cntr,paste(year)], cntr=Data_input_e$dest, year=Data_input_e$year)
  Data_input_f$u_i_dest<- mapply(function (cntr,year) UNDERCOUNT_DANKO_MODEL$Iimp[cntr,paste(year)], cntr=Data_input_f$dest, year=Data_input_f$year)
  
  
   
  for(i in 34:35) for (j in c('e','f','d')) file.remove(paste0('./data/DataAndFit2_',i,j,'.enc'))
  Data_input<-Data_input_a; save(file='./data/DataAndFit2_34e.RDA',list='Data_input')
  Data_input<-Data_input_b; save(file='./data/DataAndFit2_35e.RDA',list='Data_input')
  Data_input<-Data_input_c; save(file='./data/DataAndFit2_34f.RDA',list='Data_input')
  Data_input<-Data_input_d; save(file='./data/DataAndFit2_35f.RDA',list='Data_input')
  Data_input<-Data_input_e; save(file='./data/DataAndFit2_34d.RDA',list='Data_input')
  Data_input<-Data_input_f; save(file='./data/DataAndFit2_35d.RDA',list='Data_input')
  suppressWarnings(rm(Data_input))
  source('./.secrets/getpass.R')
}
#correct mistake

#raw_int_flows_a<<- raw_int_flows_b<<-raw_int_flows_c<<-raw_int_flows_d<<-raw_int_flows_e<<-raw_int_flows_f<<-fill_missing_years_a(get_data_raw('./data/raw_int_flows_34e.enc'))[seq(1,1000,2),,,]#no mov no lfs
# raw_int_flows_a <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_34e.enc'))[seq(1, 1000, 10),,,]; gc()#no mov no lfs
# raw_int_flows_c <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_34f.enc'))[seq(1, 1000, 10),,,]; gc() #simple mov no lfs
# raw_int_flows_e <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_34d.enc'))[seq(1, 1000, 10),,,]; gc() #a8 mov no lfs
# raw_int_flows_b <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_35e.enc'))[seq(1, 1000, 10),,,]; gc() #no mov  lfs
# raw_int_flows_d <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_35f.enc'))[seq(1, 1000, 10),,,]; gc() #simple mov lfs
# raw_int_flows_f <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_35d.enc'))[seq(1, 1000, 10),,,]; gc() #a8 mov lfs

decode_and_load_rda('./data/raw_int_abcdef_short.RDA')
gc()



decode_and_load_rda('./data/LFS_Undercounting_2.RDA')
decode_and_load_rda('./data/LFS_Accuracy.RDA')

decode_and_load_rda('./data/QuantMigEst.RDA')
QuantMigEst$origin2[QuantMigEst$origin2=='GB']<-'UK'
QuantMigEst$dest2[QuantMigEst$dest2=='GB']<-'UK'
QuantMigEst<<-QuantMigEst[QuantMigEst$origin2%in%Countries,]
QuantMigEst<<-QuantMigEst[QuantMigEst$dest2%in%Countries,]
QuantMigEst<<-QuantMigEst[QuantMigEst$year%in%Data_input_a$year,]
all(Countries %in% QuantMigEst$dest2)
all(Countries %in% QuantMigEst$origin2)


LFS_ACCU_ADJ<-rsm.data$a_imm.s_base
LFS_ACCU_ADJ[rsm.data$LUI.s==0]<-2



# correct_CH_acceu<-function(rsm.data){
#   #rsm.data$acceu[,'CH',]
#   # For the countries that joined the European Union before 2004, plus Cyprus and Malta, restrictions on freedom of movement were initially lifted on 1 June 2007, but Switzerland decided to reimpose them from 1 June 2013 to 31 May 2014 under the safeguard clause of the Agreement on the Free Movement of Persons (AFMP) with the EU. Similarly, for the countries that joined the EU in 2004, except Cyprus and Malta, restrictions on freedom of movement were initially lifted on 1 May 2011, but Switzerland decided to reimpose them from 1 May 2012 to 30 April 2014 under the safeguard clause. Also, according to the Protocol to the Agreement between the European Community and Switzerland regarding the participation of Bulgaria and Romania, Switzerland applied the 2+3+2 transitional period formula to these two countries starting from 1 June 2009. Restrictions were consequently lifted on 1 June 2016, but Switzerland decided to reimpose them from 1 June 2017 to 31 May 2019 under the safeguard clause. Switzerland decided in November 2022 to invoke the safeguard clause for 2023 with respect to Croatia. The clause can not be used after the end of 2026.[45]
#   ACCE2004<-c('CY','CZ','EE','HU','LV','LT','MT','PL','SI','SK')
#   rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004) ,'CH',paste(2013:2014)]<-
#     0.5 *(rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004),'CH',paste(2013:2014)]>0)  #june-June
#   rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004) ,'CH',paste(2007)]<-
#     0.5 *(rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004),'CH',paste(2013:2014)]>0)  #june-June
#   
#   rsm.data$acceu[,'CH',]
# }

# update_accuacy<-function(rsm.data){
#   U<-(rsm.data$U_E.r-1)/4
#   A<-(rsm.data$A_E.r-1)/2
#   nA<-(U/2.25+1)*(A+1)-1 
#   i1<-nA<0.3333333
#   i3<-nA>=0.6666667
#   i2<-!(i1|i3)
#   nA[i1]<-1
#   nA[i2]<-2
#   nA[i3]<-3
#   # nA['AT',]
#   # A['AT',]
#   # U['AT',]
#   rsm.data$A_E.r<-nA
#   
#   U<-(rsm.data$U_I.r-1)/4
#   A<-(rsm.data$A_I.r-1)/2
#   nA<-(U/2.25+1)*(A+1)-1 
#   i1<-nA<0.3333333
#   i3<-nA>=0.6666667
#   i2<-!(i1|i3)
#   nA[i1]<-1
#   nA[i2]<-2
#   nA[i3]<-3
#   # nA['AT',]
#   # A['AT',]
#   # U['AT',]
#   rsm.data$A_I.r<-nA
#   rsm.data
# }
# 
# rsm.data_updated <<- update_accuacy(rsm.data)


# FREEDOM <<- as.matrix(t(apply(rsm.data$acceu, 1, function(k) 2019-rowSums(k))))
# FREEDOM.txtLEVELS <<- FREEDOM.LEVELS <<- sort(unique(as.vector(unlist(FREEDOM))))
# FREEDOM.txtLEVELS[FREEDOM.LEVELS==2002] <- '2002 or earlier'
# FREEDOM.txtLEVELS[FREEDOM.LEVELS==2019] <- '2019 or later'


decode_and_load_rda(file='./data/DurEmi.RDA',envir = .GlobalEnv)
decode_and_load_rda(file='./data/DurImm.RDA',envir = .GlobalEnv)

dE<-dE[,dimnames(rsm.data$x.emi)[[3]]]
dI<-dI[,dimnames(rsm.data$x.imm)[[3]]]

# rsm.data$I_R[,'DE',]
# rsm.data$I_S['DE',,]
# unique(as.vector(rsm.data$I_R))
# # unique(as.vector(rsm.data$I_S))
# 
# rsm.data$tmi[,'DE',]
# rsm.data$I_S['DE',,]
# unique(as.vector(rsm.data$tmi*12))
# unique(as.vector(rsm.data$tme*12))
rsm.data<<-rsm.data

Countries <<- sort(unique(Data_input_a$orig))
CountriesU <<- Countries
CountriesU[CountriesU=='UK'] <- 'GB'
CountriesFull <<- countrycode(CountriesU, 'iso2c','country.name')
CountriesFull <<- paste0(CountriesFull, ' (',Countries,')')

levelsE<<-paste(sort(unique(unlist(dE))))
levelsI<<-paste(sort(unique(unlist(dI))))

AllLevels<<-sortdur(unique(c(levelsE,levelsI)))

DTCoverage<-data.frame('Country name'= substr(CountriesFull,1,nchar(CountriesFull)-4), Iso2=Countries, Coverage='Low', check.names = FALSE, check.rows = FALSE)
DTCoverage$Coverage[rsm.data$ic.exc.r]<-'High'

DTCoverageTr<-data.frame('Country name'= substr(CountriesFull,1,nchar(CountriesFull)-4), Iso2=Countries, Coverage='Low', check.names = FALSE, check.rows = FALSE)
DTCoverageTr$Coverage[rsm.data$ic.exc.s]<-'High'
rownames(DTCoverageTr) <- DTCoverageTr$Iso2

DTUndercountingTr<-data.frame('Country name'= substr(CountriesFull,1,nchar(CountriesFull)-4), 
                              Iso2=Countries, 
                              'Fraction of non-response' = format(round(rowMeans(toSHINY_LFS_UNDERCOUNTING$nrs_02_20,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE),
                              'Fraction of missing' = format(round(rowMeans(toSHINY_LFS_UNDERCOUNTING$frac_miss_02_20,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE), 
                              'Combined measure' = format(round(rowMeans(toSHINY_LFS_UNDERCOUNTING$combined_02_20,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE), 
                              Undercounting='Low', 
                              check.names = FALSE, check.rows = FALSE)
DTUndercountingTr$Undercounting[rsm.data$LUI.s==0]<-'High'
DTUndercountingTr$Undercounting[Countries=='IE']<-''
DTUndercountingTr$`Fraction of non-response`[Countries=='IE']<-
  DTUndercountingTr$`Fraction of missing`[Countries=='IE']<-
  DTUndercountingTr$`Combined measure`[Countries=='IE']<-''

DTAccuracyTr<-data.frame('Country name'= substr(CountriesFull,1,nchar(CountriesFull)-4), Iso2=Countries, 
                         CV = format(round(rowMeans(toSHINY_LFS_ACCU$cv.2,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE),
                         'Non-adjusted accuracy'='Low',
                         'Undercounting-adjusted accuracy'='Low',
                         check.names = FALSE, check.rows = FALSE)
DTAccuracyTr$`Non-adjusted accuracy`[rsm.data$a_imm.s_base==1]<-'High'
DTAccuracyTr$`Undercounting-adjusted accuracy`[rsm.data$a_imm.s==1]<-'High'
DTAccuracyTr$`Undercounting-adjusted accuracy`[Countries=='IE']<-DTAccuracyTr$`Non-adjusted accuracy`[Countries=='IE']<-''
DTAccuracyTr$CV[Countries=='IE']<-''

closeAllConnections()
gc()