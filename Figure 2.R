library(car)

#FIGURE 2

#Even though the stats model was run with the rPRI values the authors used, I still wanted to make Partial Regression Plots of the data I compiled. The next section I adjusted the values back to the ones the authors used

TABLE2<- TABLE1
TABLE2
df_figure<- as.data.frame(TABLE2)
is.data.frame(df_figure)
df_figure<-df_figure[-c(6,11,17,25,39),]
df_figure<- cbind(df_figure, ecoregion=c("Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Succulent Woodland","Succulent Woodland","Succulent Woodland","Succulent Woodland","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest"))
df_figure
df_figure<-df_figure[-c(34),]

#Fig2a
avPlots(lm(rTLNP ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI")


#Fig2b
avPlots(lm(rCAR ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI")


#Fig2c
avPlots(lm(rAFR ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI")


#Fig2d
avPlots(lm(rROD ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI")


##WITH VALUES TO MATCH AUTHORS (list values we have that differ)
Figure2_values<- TABLE1
Figure2_values
df_figure<- as.data.frame(Figure2_values)
df_figure<-df_figure[-c(6,11,17,25,39),]
df_figure<- cbind(df_figure, ecoregion=c("Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Succulent Woodland","Succulent Woodland","Succulent Woodland","Succulent Woodland","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest"))
df_figure<-df_figure[-c(34),]
df_figure[1,1]<-4
df_figure[4,1]<-7
df_figure[11,1]<-8
df_figure

par(mfrow=c(2,2))

#Fig2a
avPlots(lm(rTLNP ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="TLNP Species Richness",ylim=c(-20,10), xlim=c(-6,4))
text(-6,9,labels = "a")

#Fig2b
avPlots(lm(rCAR ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="Carnivoran Species Richness",ylim=c(-3,2), xlim=c(-6,4))
text(-6,2,labels = "b")

#Fig2c
avPlots(lm(rAFR ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="Afrosoricid Species Richness",ylim=c(-6,4), xlim=c(-6,4))
text(-6,4,labels = "c")

#Fig2d
avPlots(lm(rROD ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="Rodent Species Richness",ylim=c(-6,6), xlim=c(-6,4))
text(-6,6,labels = "d")

#REDOING FIGURE 2 to include Analevelona data in Fig 2a (TLNP vs Primate), the other graphs had Analevelona omitted
Figure2_values<- TABLE1
Figure2_values
df_figure<- as.data.frame(Figure2_values)
df_figure<-df_figure[-c(6,11,17,25,39),]
df_figure<- cbind(df_figure, ecoregion=c("Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Spiny Thicket","Succulent Woodland","Succulent Woodland","Succulent Woodland","Succulent Woodland","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Dry Deciduous Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Humid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest","Subhumid Forest"))
df_figure[1,1]<-4
df_figure[4,1]<-7
df_figure[11,1]<-8
df_figure
par(mfrow=c(2,2))
#Updated Fig 2a
avPlots(lm(rTLNP ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="TLNP Species Richness",ylim=c(-20,10), xlim=c(-6,4))
text(-6,9,labels = "a")

df_figure<-df_figure[-c(34),]

#Fig2b
avPlots(lm(rCAR ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="Carnivoran Species Richness",ylim=c(-3,2), xlim=c(-6,4))
text(-6,2,labels = "b")

#Fig2c
avPlots(lm(rAFR ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="Afrosoricid Species Richness",ylim=c(-6,4), xlim=c(-6,4))
text(-6,4,labels = "c")

#Fig2d
avPlots(lm(rROD ~ ecoregion+rPRI ,data=df_figure),grid=FALSE,id=FALSE, terms = "rPRI",xlab="Primate Species Richness",ylab="Rodent Species Richness",ylim=c(-6,6), xlim=c(-6,4))
text(-6,6,labels = "d")



