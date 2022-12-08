### Loading for all data
rm(list=ls())
setwd("N:/TZN_data/Collembola_brand_new")
#setwd("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new")
#setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop
#Plot description
plot_description <- read.table("Data/plot_description.txt", header = T)
plot_description$Plot_Nr <-as.factor(plot_description$Plot_Nr)

# packages
library(lme4)
library(gridExtra)
library(effects)
library(ggplot2)
library(corrplot)
library(dplyr) ## to calculate mean

# functions
source("N:/TZN_data/Functions/stepba function.R")
source("N:/TZN_data/Functions/graphic_themes.R")

#source("~/Documents/ThuZar/TZN_data/Functions/stepba function.R")## For TZN laptop
#source("~/Documents/ThuZar/TZN_data/Functions/graphic_themes.R")## For TZN laptop

## Load the data files
# functional traits
#fun_trait17<-read.table("Data/Fun_trait_collembola2017.txt", header = T,stringsAsFactors = F) [c(2,8:9,12:17)]
fun_trait17<-read.table("Data/Fun_trait_collembola2017.txt", header = T) 


str(fun_trait17)
fun_trait17$Body_length <- as.numeric(fun_trait17$Body_length)
str(fun_trait17)

# change NA instead of zero because there is no collembola in some plots
fun_trait17[fun_trait17==0] <- NA
str(fun_trait17)

names(fun_trait17)
#aggregate(fun_trait17[,-1],list(fun_trait17$Plot_Nr),mean,na.rm=T)# This is the same way
plotmean17 <- aggregate(fun_trait17[,c(8:9,12:17)],list(fun_trait17$Plot_Nr),mean,na.rm=T)
names(plotmean17)[1]<-"Plot_Nr" ## why Group-1 is chaning?
str(plotmean17)

# Individual raw functional traits 2018
fun_trait18<-read.table("Data/Fun_trait_collembola2018.txt", header = T)
str(fun_trait18)

# change NA instead of zero because there is no collembola in some plots
fun_trait18[fun_trait18==0] <- NA
str(fun_trait18)

#aggregate(fun_trait18[,-1],list(fun_trait18$Plot_Nr),mean,na.rm=T)# This is the same way
plotmean18 <- aggregate(fun_trait18[,c(3:4,7:12)],list(fun_trait18$Plot_Nr),mean,na.rm=T)
names(plotmean18)[1]<-"Plot_Nr" 
str(plotmean18)

# do mean all data
Fun_traits1718 <- rbind(plotmean17, plotmean18)

Fun_traits_2yrs <- aggregate(Fun_traits1718[,2:9], by= list(Fun_traits1718$Plot_Nr), mean, na.rm=T)
names(Fun_traits_2yrs)[1] <- "Plot_Nr"
head(Fun_traits_2yrs)

##----
# Abundance 2017 and 2018
### for main effect without changing Na instead of zero, didn't work
Col_Abundance17<-read.table("Data/Collembola_abundance 2017.txt", header = T)
str(Col_Abundance17)

Col_Abundance18<-read.table("Data/Collembola_abundance 2018.txt", header = T)
str(Col_Abundance18)


All_Abundance <- rbind(Col_Abundance17, Col_Abundance18)
Abundance_2yrs <- aggregate(All_Abundance[,8], by= list(All_Abundance$Plot_Nr), mean, na.rm=T)
names(Abundance_2yrs)[1] <- "Plot_Nr"
names(Abundance_2yrs)[2] <- "Abundance"
str(Abundance_2yrs)

# Feeding 2018 and 2022
FR_18<-read.table("Data/Feeding activity_2018.txt", header = T)
str(FR_18)
# change NA instead of zero 
#FR_18[FR_18==0] <- NA # No need to change except functional traits of col
str(FR_18)

FR_22<-read.table("Data/Feeding activity_2022.txt", header = T)
str(FR_22)
# change NA instead of zero 
#FR_22[FR_22==0] <- NA # No need to change except functional traits of col
str(FR_22)

All_FR <- rbind(FR_18, FR_22)
FR_2yrs <- aggregate(All_FR[,3], by= list(All_FR$Plot_Nr), mean, na.rm=T)
names(FR_2yrs)[1:2] <- c("Plot_Nr", "Feeding_rate") 
head(FR_2yrs)

## SLA,LDMC A2017 and A2018
SLA_LDMC17<-read.table("Data/CWM_SLA_LDMC_A17.txt", header = T)
SLA_LDMC18<-read.table("Data/CWM_SLA_LDMC_A18.txt", header = T)

All_SLA_LDMC <- rbind(SLA_LDMC17, SLA_LDMC18)
SLA_LDMC_2yrs <- aggregate(All_SLA_LDMC[,2:3], by= list(All_SLA_LDMC$Plot_Nr), mean, na.rm=T)
names(SLA_LDMC_2yrs)[1] <- "Plot_Nr" 
head(SLA_LDMC_2yrs)


##MPD A2017 and A2018
MPD17<-read.table("Data/MPD_A17.txt", header = T)
MPD18<-read.table("Data/MPD_A18.txt", header = T)
All_MPD <- rbind(MPD17, MPD18)
MPD_2yrs <- aggregate(All_MPD[,2:3], by= list(All_MPD$Plot_Nr), mean, na.rm=T)
names(MPD_2yrs)[1] <- "Plot_Nr" 
head(MPD_2yrs)
## Merge Plant traits
Plant_traits <- merge(SLA_LDMC_2yrs, MPD_2yrs)

### NIRS_2yrs (August 2017 and 2018)
Nut17<- read.table("Data/Nirs_ThuZar_2017.txt", header = T)[c(1:10)]
Nut18<- read.table("Data/Nirs_ThuZar_2018.txt", header = T)

#### To fill the mean value for NA
nutrients17 <- merge(Nut17, plot_description)
nutrients18 <- merge(Nut18, plot_description)

# fill missing values for Pt, Hs, Ra, As 2017

Ptmean <- apply(nutrients17[nutrients17$Comb=="Pt",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "Pt" & is.na(nutrients17$ADF)), c(2:10)] <- Ptmean 

Hsmean <- apply(nutrients17[nutrients17$Comb=="Hs",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "Hs" & is.na(nutrients17$ADF)), c(2:10)] <- Hsmean 

Ramean <- apply(nutrients17[nutrients17$Comb=="Ra",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "Ra" & is.na(nutrients17$ADF)), c(2:10)] <- Ramean 

Asmean <- apply(nutrients17[nutrients17$Comb=="As",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "As" & is.na(nutrients17$ADF)), c(2:10)] <- Asmean 


# fill missing values for Ga, 81S, 810F, 810M 2018
Gamean <- apply(nutrients18[nutrients18$Comb=="Ga",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "Ga" & is.na(nutrients18$ADF)), c(2:10)] <- Gamean 

Smean <- apply(nutrients18[nutrients18$Comb=="81S",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "81S" & is.na(nutrients18$ADF)), c(2:10)] <- Smean 

Fmean <- apply(nutrients18[nutrients18$Comb=="810F",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "810F" & is.na(nutrients18$ADF)), c(2:10)] <- Fmean 

Mmean <- apply(nutrients18[nutrients18$Comb=="810M",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "810M" & is.na(nutrients18$ADF)), c(2:10)] <- Mmean 


## Merge

All_nutrients<- merge(nutrients17, nutrients18, all= T)
nutrients_2yrs <- aggregate(All_nutrients[,2:10], by= list(All_nutrients$Plot_Nr), mean, na.rm=T)

names(nutrients_2yrs)[1]<-"Plot_Nr" 
str(nutrients_2yrs)
#Calculate C:N ratio2017
nutrients_2yrs$CN_ratio <- nutrients_2yrs$C/nutrients_2yrs$Bio_N

## Biomass A2017 and A2018
biomass17<- read.table("Data/201708_Biomass_raw.txt", header = T)[c(1:3)]## If want to pick some column, do this [c(1:4)]
biomass17 <- merge(biomass17, plot_description)
str(biomass17)

biomass17$Biomass_raw <- rowMeans(biomass17[c("Sample_A", "Sample_B")], na.rm = T) 
biomass17$Harvest <- rep("A17",length(biomass17[,1])) ## Add one more column
## Make txt file to be easy
write.table(biomass17,"Data/biomass_A17.txt",row.names = FALSE)

biomass18<- read.table("Data/201808_Biomass_raw.txt", header = T)[c(2:4)]

#fill missing values for 81S, 17F,810M (A&B),11F(A),  43S, 86M, 15S  (B) 2018
biomass18 <- merge(biomass18, plot_description)
str(biomass18)

S8Biomean <- apply(biomass18[biomass18$Comb=="81S",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$Comb== "81S" & is.na(biomass18$Sample_A)), c(2:3)] <- S8Biomean #28.82000 24.16333  

S4Biomean <- apply(biomass18[biomass18$Comb=="43S",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$Comb== "43S" & is.na(biomass18$Sample_B)), c(2:3)] <- S4Biomean #9.53250 16.05667

M8Biomean <- apply(biomass18[biomass18$Comb=="86M",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$Comb== "86M" & is.na(biomass18$Sample_B)), c(2:3)] <- M8Biomean #21.75000 23.24333 

M82Biomean <- apply(biomass18[biomass18$Comb=="810M",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$Comb== "810M" & is.na(biomass18$Sample_B)), c(2:3)] <- M82Biomean #7.753333 18.816667


### Can't run with Comb(it show Na), so take FC mean
F17Biomean <- apply(biomass18[biomass18$FC=="F",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$FC== "F" & is.na(biomass18$Sample_A)), c(2:3)] <- F17Biomean # 14.63441 17.16143 



### Can't run with Comb(it show Na), so take FC mean
F11Biomean <- apply(biomass18[biomass18$FC=="F",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$FC== "F" & is.na(biomass18$Sample_A)), c(2:3)] <- F11Biomean #14.63441 17.16143 


### Can't run with Comb(it show Na), so take FC mean
S15Biomean <- apply(biomass18[biomass18$FC=="S",c(2:3)],2,mean, na.rm=T)
biomass18[which(biomass18$FC== "S" & is.na(biomass18$Sample_B)), c(2:3)] <- S15Biomean #22.88644 29.27227 


biomass18$Biomass_raw <- rowMeans(biomass18[c("Sample_A", "Sample_B")], na.rm = T) 
biomass18$Harvest <- rep("A18",length(biomass18[,1]))



## Make txt file to be easy
write.table(biomass18,"Data/biomass_A18.txt",row.names = FALSE)

All_Biomass <- rbind(biomass17, biomass18)
biomass_2yrs <- aggregate(All_Biomass[ ,10], by = list(All_Biomass$Plot_Nr), mean, na.rm=T)
names(biomass_2yrs)[1] <- "Plot_Nr"
names(biomass_2yrs)[2] <- "Biomass"
str(biomass_2yrs)

## Root biomass # only one year
Root_biomass <- read.table("Data/2017_Root_biomass.txt", header = T) [-c(3:5)]

### Cover A2017 and A2018
cover17<-read.table("Data/Relative_perc_cover_A17.txt", header = T)
str(cover17)
totcover17 <- aggregate(cover17[,c(5)],list(cover17$Plot_Nr),mean,na.rm=T)
names(totcover17)[1]<-"Plot_Nr"
names(totcover17)[2]<-"Cover"
str(totcover17)

cover18<-read.table("Data/Relative_perc_cover_A18.txt", header = T)
str(cover18)
totcover18 <- aggregate(cover18[,c(5)],list(cover18$Plot_Nr),mean,na.rm=T)
names(totcover18)[1]<-"Plot_Nr"
names(totcover18)[2]<-"Cover"
str(totcover18)

# do mean all data
Tcover1718 <- rbind(totcover17, totcover18)
Tcover_2yrs <- aggregate(Tcover1718[,2], by= list(Tcover1718$Plot_Nr), mean, na.rm=T)
names(Tcover_2yrs)[1] <- "Plot_Nr"
names(Tcover_2yrs)[2] <- "Cover"
head(Tcover_2yrs)
str(Tcover_2yrs)


### Merge all data files
mean17_A<- merge(Fun_traits_2yrs, Abundance_2yrs, all  = T)
str(mean17_A)

## add feeding rate 
mean17_AF<-merge(mean17_A,FR_2yrs, all.x = T)
str(mean17_AF)

## add SLA/LDMC/MPD 
Traits_Pt_Col<-merge(mean17_AF,Plant_traits)
str(Traits_Pt_Col)

## add NIRS 
Traits_Pt_Col_Nir<-merge(Traits_Pt_Col, nutrients_2yrs, all.x=TRUE)
str(Traits_Pt_Col_Nir)

## add biomass 
alldata_B<-merge(Traits_Pt_Col_Nir, biomass_2yrs, all.x=TRUE)
str(alldata_B)

### add root biomass
alldata_B_R<-merge(alldata_B, Root_biomass, all.x=TRUE)
str(alldata_B_R)

## add R_perc_cover 
All_fulltraits<-merge(alldata_B_R, Tcover_2yrs, all.x=TRUE)
str(All_fulltraits)

### Write a new text file for clean dataset

write.table(All_fulltraits,"Data/Full_Mean_Traits_2yrs.txt",row.names = FALSE)

#### Check NA plot
#AllTraits_1718[AllTraits_1718$Body_length17<0,]$Plot ###(Total  plots (216) - NA (122)= 94 plots for body length17)
#AllTraits_1718[All_fulltraits$Body_length<0,]$Plot ###(Total  plots (216) - NA (90)= 126 plots for body length17)


