library(car)
library(tidyverse)

Table2<- TABLE1
Table2
df<- as.data.frame(Table2)
is.data.frame(df)
df<-df[-c(6,11,17,25,39),]
##DUMMY CODING ECOREGION
df<- cbind(df, ecoregion=c("Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Succulent Woodland","Succulent Woodland","Succulent Woodland","Succulent Woodland","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest"))
df
str(df$ecoregion)


#FINDING AND REMOVING THOSE WHERE Di > 4/n (4/34): "We removed outliers from the analysis if Di > 4 / n, where n = 34 (i.e., the number of mammal communities,"
library(olsrr)
m<-lm(data=df,rTLNP ~ ecoregion + rPRI)
ols_plot_cooksd_chart(m)
df<-df[-c(34),]
##REMOVE ANALAVELONA FROM ANALYSIS

##TABLE AND INEFRENTIAL STATS
library(AutoModel)
#TLNP vs. PRI
m1<-lm(data=df,rTLNP ~ ecoregion) 
summary(m1)
m2<- lm(data=df, rTLNP ~ rPRI)
summary(m2)
m<-lm(data=df,rTLNP ~ ecoregion + rPRI)
summary(m)
#OR
run_model("rTLNP", c("ecoregion"), c("rPRI"), dataset = df)

#CAR vs PRI
m1<-lm(data=df,rCAR ~ ecoregion) 
summary(m1)
m2<- lm(data=df, rCAR ~ rPRI)
summary(m2)
m<-lm(data=df,rCAR ~ ecoregion+rPRI)
summary(m)
#OR
run_model("rCAR", c("ecoregion"), c("rPRI"), dataset = df)


#AFR vs PRI
m1<-lm(data=df,rAFR ~ ecoregion) 
summary(m1)
m2<- lm(data=df, rAFR ~ rPRI)
summary(m2)
m<-lm(data=df,rAFR ~ rPRI + ecoregion)
summary(m)
#OR
run_model("rAFR", c("ecoregion"), c("rPRI"), dataset = df)

#ROD vs PRI
m1<-lm(data=df,rROD ~ ecoregion) 
summary(m1)
m2<- lm(data=df, rROD ~ rPRI)
summary(m2)
m<-lm(data=df,rROD ~ as.factor(ecoregion) + rPRI)
summary(m)
#OR
run_model("rROD", c("ecoregion"), c("rPRI"), dataset = df)



##TABLE AND INEFRENTIAL STATS With numbers they came up with for rPRI

TABLE2<- TABLE1
TABLE2
df<- as.data.frame(TABLE2)
is.data.frame(df)
df<-df[-c(6,11,17,25,39),]

##DUMMY CODING ECOREGION
df<- cbind(df, ecoregion=c("Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Succulent Woodland","Succulent Woodland","Succulent Woodland","Succulent Woodland","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest"))
df
str(df$ecoregion)

#FINDING AND REMOVING THOSE WHERE Di > 4/n (4/34): "We removed outliers from the analysis if Di > 4 / n, where n = 34 (i.e., the number of mammal communities,"
library(olsrr)
m<-lm(data=df,rTLNP ~ ecoregion + rPRI)
ols_plot_cooksd_chart(m)
df<-df[-c(34),]

##REMOVE ANALAVELONA FROM ANALYSIS
#INSERTING VALUES WE HAD THAT WERE DIFFERENT
df[1,1]<-4
df[4,1]<-7
df[11,1]<-8
df

#TLNP vs PRI
m1<-lm(data=df,rTLNP ~ ecoregion) 
summary(m1)
m<-lm(data=df,rTLNP ~ ecoregion + rPRI)
summary(m)


#OR
rTLNP_STATS<-run_model("rTLNP", c("ecoregion"), c("rPRI"), dataset = df)
rTLNP_STATS<-rTLNP_STATS$SummaryDF

#CAR vs PRI
m1<-lm(data=df,rCAR ~ ecoregion) 
summary(m1)
m<-lm(data=df,rCAR ~ ecoregion+rPRI)
summary(m)
#OR
rCAR_STATS<-run_model("rCAR", c("ecoregion"), c("rPRI"), dataset = df)
rCAR_STATS<-rCAR_STATS$SummaryDF

#AFR vs PRI
m1<-lm(data=df,rAFR ~ ecoregion) 
summary(m1)
m<-lm(data=df,rAFR ~ rPRI + ecoregion)
summary(m)
#OR
rAFR_STATS<-run_model("rAFR", c("ecoregion"), c("rPRI"), dataset = df)
rAFR_STATS<-rAFR_STATS$SummaryDF

#ROD vs PRI
m1<-lm(data=df,rROD ~ ecoregion) 
summary(m1)
m<-lm(data=df,rROD ~ ecoregion + rPRI)
summary(m)
#OR
rROD_STATS<- run_model("rROD", c("ecoregion"), c("rPRI"), dataset = df)
rROD_STATS<-rROD_STATS$SummaryDF


#MAKING FINAL VERION OF TABLE 2
rTLNP_STATS<- rTLNP_STATS[,c("R2","SE","DeltaR2","Fch","DF1","DF2","pval")]
rCAR_STATS<- rCAR_STATS[,c("R2","SE","DeltaR2","Fch","DF1","DF2","pval")]
rAFR_STATS<- rAFR_STATS[,c("R2","SE","DeltaR2","Fch","DF1","DF2","pval")]
rROD_STATS<- rROD_STATS[,c("R2","SE","DeltaR2","Fch","DF1","DF2","pval")]
FINALTABLE2<- rbind(rTLNP_STATS,rCAR_STATS,rAFR_STATS,rROD_STATS)
row.names(TABLE2)<- c("rTLNP","  ","rCAR","   ","rAFR","    ","rROD","     ")
TABLE2<- cbind(TABLE2, Block=c("1","2","1","2","1","2","1","2"))
TABLE2<-TABLE2[c(8,1,2,3,4,5,6,7)]
colnames(TABLE2)[5]<- "DeltaF"
TABLE2
