library(countrycode)

get_data<-function(nam){
  decode_and_load_rda(nam)
  Data_input
}

sortdur<-function(k){
  k<-sort(k)
  c(k[!k%in%c('12','60')],'12','60')
}

load_old <- FALSE
if (load_old){ #not available on GitHub
  Data_input_80a<<-get_data('./data/old/DataAndFit_80a.RDA')
  Data_input_80b<<-get_data('./data/old/DataAndFit_80b.RDA')
  Data_input_80c<<-get_data('./data/old/DataAndFit_80c.RDA')
  Data_input_80d<<-get_data('./data/old/DataAndFit_80d.RDA')
  Data_input_80e<<-get_data('./data/old/DataAndFit_80e.RDA')
  Data_input_80f<<-get_data('./data/old/DataAndFit_80f.RDA')
  
} else {
  Data_input_80a<<-get_data('./data/DataAndFit2_85a.RDA')
  Data_input_80b<<-get_data('./data/DataAndFit2_85b.RDA')
  Data_input_80c<<-get_data('./data/DataAndFit2_85c.RDA')
  Data_input_80d<<-get_data('./data/DataAndFit2_85d.RDA')
  Data_input_80e<<-get_data('./data/DataAndFit2_85e.RDA')
  Data_input_80f<<-get_data('./data/DataAndFit2_85f.RDA')
}

decode_and_load_rda('./data/input_data_VI_2023.RDA')
decode_and_load_rda('./data/LFS_Undercounting.RDA')
toSHINY_LFS_UNDERCOUNTING_OLD <- toSHINY_LFS_UNDERCOUNTING
decode_and_load_rda('./data/LFS_Undercounting_2.RDA')

decode_and_load_rda('./data/LFS_Accuracy.RDA')

LFS_ACCU_ADJ<-rsm.data$a_imm.s
LFS_ACCU_ADJ[rsm.data$LUI.s==0]<-2

rsm.data$A_E.r['AT',paste((2002:2018))]<-2
rsm.data$A_I.r['AT',paste((2002:2018))]<-2

# MODIFICATIONS 2023
# accuracy of German data from 1 to 2
rsm.data$A_E.r['DE',paste((2002:2018))]<-2
rsm.data$A_I.r['DE',paste((2002:2018))]<-2


unique(rsm.data$tmi*12)
rsm.data$tmi[rsm.data$tmi*12==1.25]<-1
unique(rsm.data$tmi*12)

rsm.data$U_E.r<-rsm.data$U4_E.r #model only
rsm.data$U_I.r<-rsm.data$U4_I.r #model only

correct_CH_acceu<-function(rsm.data){
  #rsm.data$acceu[,'CH',]
  # For the countries that joined the European Union before 2004, plus Cyprus and Malta, restrictions on freedom of movement were initially lifted on 1 June 2007, but Switzerland decided to reimpose them from 1 June 2013 to 31 May 2014 under the safeguard clause of the Agreement on the Free Movement of Persons (AFMP) with the EU. Similarly, for the countries that joined the EU in 2004, except Cyprus and Malta, restrictions on freedom of movement were initially lifted on 1 May 2011, but Switzerland decided to reimpose them from 1 May 2012 to 30 April 2014 under the safeguard clause. Also, according to the Protocol to the Agreement between the European Community and Switzerland regarding the participation of Bulgaria and Romania, Switzerland applied the 2+3+2 transitional period formula to these two countries starting from 1 June 2009. Restrictions were consequently lifted on 1 June 2016, but Switzerland decided to reimpose them from 1 June 2017 to 31 May 2019 under the safeguard clause. Switzerland decided in November 2022 to invoke the safeguard clause for 2023 with respect to Croatia. The clause can not be used after the end of 2026.[45]
  ACCE2004<-c('CY','CZ','EE','HU','LV','LT','MT','PL','SI','SK')
  rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004) ,'CH',paste(2013:2014)]<-
    0.5 *(rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004),'CH',paste(2013:2014)]>0)  #june-June
  rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004) ,'CH',paste(2007)]<-
    0.5 *(rsm.data$acceu[!(rownames(rsm.data$acceu)%in%ACCE2004),'CH',paste(2013:2014)]>0)  #june-June
  
  rsm.data$acceu[,'CH',]
}

update_accuacy<-function(rsm.data){
  U<-(rsm.data$U_E.r-1)/4
  A<-(rsm.data$A_E.r-1)/2
  nA<-(U/2.25+1)*(A+1)-1 
  i1<-nA<0.3333333
  i3<-nA>=0.6666667
  i2<-!(i1|i3)
  nA[i1]<-1
  nA[i2]<-2
  nA[i3]<-3
  # nA['AT',]
  # A['AT',]
  # U['AT',]
  rsm.data$A_E.r<-nA
  
  U<-(rsm.data$U_I.r-1)/4
  A<-(rsm.data$A_I.r-1)/2
  nA<-(U/2.25+1)*(A+1)-1 
  i1<-nA<0.3333333
  i3<-nA>=0.6666667
  i2<-!(i1|i3)
  nA[i1]<-1
  nA[i2]<-2
  nA[i3]<-3
  # nA['AT',]
  # A['AT',]
  # U['AT',]
  rsm.data$A_I.r<-nA
  rsm.data
}

rsm.data_updated <<- update_accuacy(rsm.data)


FREEDOM <<- as.matrix(t(apply(rsm.data$acceu, 1, function(k) 2019-rowSums(k))))
FREEDOM.txtLEVELS <<- FREEDOM.LEVELS <<- sort(unique(as.vector(unlist(FREEDOM))))
FREEDOM.txtLEVELS[FREEDOM.LEVELS==2002] <- '2002 or earlier'
FREEDOM.txtLEVELS[FREEDOM.LEVELS==2019] <- '2019 or later'

decode_and_load_rda(file='./data/DurEmi.RDA',envir = .GlobalEnv)
decode_and_load_rda(file='./data/DurImm.RDA',envir = .GlobalEnv)

# rsm.data$I_R[,'DE',]
# rsm.data$I_S['DE',,]
# unique(as.vector(rsm.data$I_R))
# unique(as.vector(rsm.data$I_S))

rsm.data$tmi[,'DE',]
rsm.data$I_S['DE',,]
unique(as.vector(rsm.data$tmi*12))
unique(as.vector(rsm.data$tme*12))
rsm.data<<-rsm.data

Countries <<- sort(unique(Data_input_80a$orig))
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

DTUndercountingTr<-data.frame('Country name'= substr(CountriesFull,1,nchar(CountriesFull)-4), 
                              Iso2=Countries, 
                              'Fraction of non-response' = format(round(rowMeans(toSHINY_LFS_UNDERCOUNTING$nrs_02_18,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE),
                              'Fraction of missing' = format(round(rowMeans(toSHINY_LFS_UNDERCOUNTING$frac_miss_02_18,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE), 
                              'Combined measure' = format(round(rowMeans(toSHINY_LFS_UNDERCOUNTING$combined_02_18,na.rm=T)[Countries],5),nsmall = 5, scientific = FALSE), 
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
DTAccuracyTr$`Non-adjusted accuracy`[rsm.data$a_imm.s==1]<-'High'
DTAccuracyTr$`Undercounting-adjusted accuracy`[LFS_ACCU_ADJ==1]<-'High'
DTAccuracyTr$`Undercounting-adjusted accuracy`[Countries=='IE']<-DTAccuracyTr$`Non-adjusted accuracy`[Countries=='IE']<-''
DTAccuracyTr$CV[Countries=='IE']<-''

closeAllConnections()
