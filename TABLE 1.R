#SPECIES RICHNESS MEASURES

#SPINY THICKET

rPRI<- A[1:73,c(8:11,36)]
rPRI<-colSums(rPRI)
as.data.frame(rPRI)
rCAR<- A[76:87,c(8:11,36)]
rCAR<-colSums(rCAR)
as.data.frame(rCAR)
rAFR<- A[90:120,c(8:11,36)]
rAFR<-colSums(rAFR)
as.data.frame(rAFR)
rROD<- A[123:146,c(8:11,36)]
rROD<-colSums(rROD)
as.data.frame(rROD)
rTLNP<- A[c(76:87,90:120,123:146),c(8:11,36)]
rTLNP<-colSums(rTLNP)
as.data.frame(rTLNP)
SP<- cbind(rPRI,rCAR,rAFR,rROD,rTLNP)
mean_spiny_thicket<-colMeans(SP)
SP<- rbind(SP,mean_spiny_thicket)
SP<- rbind(SP)

#SUCCULENT WOODLAND

rPRI<- A[1:73,c(6:7,33:34)]
rPRI<-colSums(rPRI)
as.data.frame(rPRI)
rCAR<- A[76:87,c(6:7,33:34)]
rCAR<-colSums(rCAR)
as.data.frame(rCAR)
rAFR<- A[90:120,c(6:7,33:34)]
rAFR<-colSums(rAFR)
as.data.frame(rAFR)
rROD<- A[123:146,c(6:7,33:34)]
rROD<-colSums(rROD)
as.data.frame(rROD)
rTLNP<- A[c(76:87,90:120,123:146),c(6:7,33:34)]
rTLNP<-colSums(rTLNP)
as.data.frame(rTLNP)
SUC<- cbind(rPRI,rCAR,rAFR,rROD,rTLNP)
mean_succulent_woodland<-colMeans(SUC)
SUC<- rbind(SUC,mean_succulent_woodland)
TABLE1<- rbind(SP,SUC)

#DRY DECIDUOUS FOREST
names(A)
rPRI<- A[1:73,c(2:5,31)]
rPRI<-colSums(rPRI)
as.data.frame(rPRI)
rCAR<- A[76:87,c(2:5,31)]
rCAR<-colSums(rCAR)
as.data.frame(rCAR)
rAFR<- A[90:120,c(2:5,31)]
rAFR<-colSums(rAFR)
as.data.frame(rAFR)
rROD<- A[123:146,c(2:5,31)]
rROD<-colSums(rROD)
as.data.frame(rROD)
rTLNP<- A[c(76:87,90:120,123:146),c(2:5,31)]
rTLNP<-colSums(rTLNP)
as.data.frame(rTLNP)
DRY<- cbind(rPRI,rCAR,rAFR,rROD,rTLNP)
mean_dry_deciduous_forest<-colMeans(DRY)
DRY<- rbind(DRY,mean_dry_deciduous_forest)
TABLE1<-rbind(SP,SUC,DRY)

#HUMID FOREST
rPRI<- A[1:73,c(24:30)]
rPRI<-colSums(rPRI)
as.data.frame(rPRI)
rCAR<- A[76:87,c(24:30)]
rCAR<-colSums(rCAR)
as.data.frame(rCAR)
rAFR<- A[90:120,c(24:30)]
rAFR<-colSums(rAFR)
as.data.frame(rAFR)
rROD<- A[123:146,c(24:30)]
rROD<-colSums(rROD)
as.data.frame(rROD)
rTLNP<- A[c(76:87,90:120,123:146),c(24:30)]
rTLNP<-colSums(rTLNP)
as.data.frame(rTLNP)
HUM<- cbind(rPRI,rCAR,rAFR,rROD,rTLNP)
mean_humid_forest<-colMeans(HUM)
HUM<- rbind(HUM,mean_humid_forest)
TABLE1<- rbind(SP,SUC,DRY,HUM)

#SUBHUMID FOREST
rPRI<- A[1:73,c(12:23,35)]
rPRI<-colSums(rPRI)
as.data.frame(rPRI)
rCAR<- A[76:87,c(12:23,35)]
rCAR<-colSums(rCAR)
as.data.frame(rCAR)
rAFR<- A[90:120,c(12:23,35)]
rAFR<-colSums(rAFR)
as.data.frame(rAFR)
rROD<- A[123:146,c(12:23,35)]
rROD<-colSums(rROD)
as.data.frame(rROD)
rTLNP<- A[c(76:87,90:120,123:146),c(12:23,35)]
rTLNP<-colSums(rTLNP)
as.data.frame(rTLNP)
SUB<- cbind(rPRI,rCAR,rAFR,rROD,rTLNP)
mean_subhumid_forest<-colMeans(SUB)
SUB<- rbind(SUB,mean_subhumid_forest)
TABLE1<- rbind(SP,SUC,DRY,HUM,SUB)
TABLE1<-as.data.frame(TABLE1)
TABLE1

library(skimr)
library(kableExtra)
TABLE1<- skim(TABLE1)  # the main `skimr()` function
TABLE1<- skim_to_wide(TABLE1) 
TABLE1
#For the Subhumid forest measures, the author and I came up with the same values for rPRI, rCAR, rAFR, rROD, and rTLNP, but different means

#COMPLEMENTARITY MEASURES

#Spiny Thinket
cPRI<- C[,c(7:10,35)]
cPRI<-colSums(cPRI)
as.data.frame(cPRI)
#SUCCULENT WOODLAND
cPRI<- C[,c(5:6,32:33)]
cPRI<-colSums(cPRI)
as.data.frame(cPRI)
#DRY DECIDUOUS
cPRI<- C[,c(1:4,30)]
cPRI<-colSums(cPRI)
as.data.frame(cPRI)
#HUMID FOREST
cPRI<- C[,c(23:29)]
cPRI<-colSums(cPRI)
as.data.frame(cPRI)
#SUBHUMID FOREST
cPRI<- C[,c(11:22,34)]
cPRI<-colSums(cPRI)
as.data.frame(cPRI)

COMP_PRI<-cbind()










cCAR<- E[,c(1:4,30)]
cCAR<-colSums(cCAR)
as.data.frame(cCAR)
cAFR<- F[,c(1:4,30)]
cAFR<-colSums(cAFR)
as.data.frame(cAFR)
cROD<- G[,c(1:4,30)]
cROD<-colSums(cROD)
as.data.frame(cROD)
cTLNP<- D[,c(1:4,30)]
cTLNP<-colSums(cTLNP)
as.data.frame(cTLNP)
DRY<- cbind(cPRI,cCAR,cAFR,cROD,cTLNP)
mean_dry<-colMeans(DRY)
DRY<- rbind(DRY,mean_dry)
DRY


