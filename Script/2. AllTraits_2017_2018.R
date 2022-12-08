## Make 2017 and 2018 separately
rm(list=ls())
setwd("N:/TZN_data/Collembola_brand_new")
#setwd("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new")
#setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop

#library(dplyr) ## to calculate mean

# functions
source("N:/TZN_data/Functions/stepba function.R")
source("N:/TZN_data/Functions/graphic_themes.R")
#source("~/Documents/ThuZar/TZN_data/Functions/stepba function.R")## For TZN laptop
#source("~/Documents/ThuZar/TZN_data/Functions/graphic_themes.R")## For TZN laptop

## Load the data files
## Full traits (see for detail at Data_Frame_all)
#All_fulltraits<-read.table("Data/Full_Mean_Traits_2yrs.txt", header = T,stringsAsFactors = F)
#str(All_fulltraits)

#Plot description
plot_description<- read.table("Data/plot_description.txt", header = T)
plot_description$Plot_Nr<-as.factor(plot_description$Plot_Nr)

# Individual raw functional traits 2017
fun_trait17<-read.table("Data/Fun_trait_collembola2017.txt", header = T) # 
str(fun_trait17)
fun_trait17$Body_length <- as.numeric(fun_trait17$Body_length)

#fun_trait17$Plot_Nr
# change NA instead of zero because there is no collembola in some plots
fun_trait17[fun_trait17==0] <- NA
str(fun_trait17)


plotmean17 <- aggregate(fun_trait17[,c(8:9,12:17)],list(fun_trait17$Plot_Nr),mean,na.rm=T)
names(plotmean17)[1]<-"Plot_Nr"
str(plotmean17)
names(plotmean17)[2:9] <- c("Body_length17", "Head_length17", "Mandible_length17", "Mandible_head_ratio17", "Mechanical_advantage17", "Deployment_capacity17","Apical_development17", "Molar_plate_length17")

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
names(plotmean18)[2:9] <- c("Body_length18", "Head_length18","Mandible_length18", "Mandible_head_ratio18", "Mechanical_advantage18", "Deployment_capacity18","Apical_development18", "Molar_plate_length18")
Mean_funTraits <- merge(plotmean17, plotmean18)

### Change NaN as NA
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Mean_funTraits[is.nan(Mean_funTraits)]<-NA
str(Mean_funTraits)


# Abundance 
### for main effect without changing Na instead of zero, didn't work
#2017
Col_Abundance17<-read.table("Data/Collembola_abundance 2017.txt", header = T) [-c(3:7)]
str(Col_Abundance17)
names(Col_Abundance17)[3] <- "Abundance17"
str(Col_Abundance17)

#2018
Col_Abundance18<-read.table("Data/Collembola_abundance 2018.txt", header = T) [-c(3:7)]
str(Col_Abundance18)
names(Col_Abundance18)[3] <- "Abundance18"
str(Col_Abundance18)


Abundance <- merge(Col_Abundance17, Col_Abundance18)

# Feeding 
#2018 a
FR_18<-read.table("Data/Feeding activity_2018.txt", header = T)[c(1:3)]
str(FR_18)
names(FR_18)[3] <- "Feeding_rate18"
str(FR_18)


# 2022
FR_22<-read.table("Data/Feeding activity_2022.txt", header = T)[c(1:3)]
str(FR_22)
names(FR_22)[3] <- "Feeding_rate22"
str(FR_22)

Feeding_rate <- merge(FR_18, FR_22)


## SLA_LDMC 
#2017
SLA_LDMC17<-read.table("Data/CWM_SLA_LDMC_A17.txt", header = T)
names(SLA_LDMC17)[2:3] <- c("SLA17", "LDMC17") 

#2018
SLA_LDMC18<-read.table("Data/CWM_SLA_LDMC_A18.txt", header = T)
names(SLA_LDMC18)[2:3] <- c("SLA18", "LDMC18") 

SLA_LDMC <- merge(SLA_LDMC17, SLA_LDMC18)

### MPD
#2017
MPD17<-read.table("Data/MPD_A17.txt", header = T)
names(MPD17)[2:3] <- c("MPD_Pre17", "MPD_Sown17")

###2018
MPD18<-read.table("Data/MPD_A18.txt", header = T)
names(MPD18)[2:3] <- c("MPD_Pre18", "MPD_Sown18")

MPD <- merge(MPD17, MPD18)

## Plant nutrients
## NIRS 2017
Nut17<- read.table("Data/Nirs_ThuZar_2017.txt", header = T)[c(1:10)]
nutrients17 <- merge(Nut17, plot_description)


#### To fill the mean value for NA

# fill missing values for Pt, Hs, Ra, As 2017

Ptmean <- apply(nutrients17[nutrients17$Comb=="Pt",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "Pt" & is.na(nutrients17$ADF)), c(2:10)] <- Ptmean 

Hsmean <- apply(nutrients17[nutrients17$Comb=="Hs",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "Hs" & is.na(nutrients17$ADF)), c(2:10)] <- Hsmean 

Ramean <- apply(nutrients17[nutrients17$Comb=="Ra",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "Ra" & is.na(nutrients17$ADF)), c(2:10)] <- Ramean 

Asmean <- apply(nutrients17[nutrients17$Comb=="As",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients17[which(nutrients17$Comb== "As" & is.na(nutrients17$ADF)), c(2:10)] <- Asmean 



names(nutrients17)[2:10] <- c("ADF17", "ADL17", "Ca17", "K17", "C17", "Mg17", "NDF17", "Bio_P17", "Bio_N17")
str(nutrients17)
#Calculate C:N ratio2017
nutrients17$CN_ratio17 <- nutrients17$C17/nutrients17$Bio_N17
str(nutrients17)


#Nirs 2018
Nut18<- read.table("Data/Nirs_ThuZar_2018.txt", header = T)[-c(11)]
nutrients18 <- merge(Nut18, plot_description)
# fill missing values for Ga, 81S, 810F, 810M 2018
Gamean <- apply(nutrients18[nutrients18$Comb=="Ga",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "Ga" & is.na(nutrients18$ADF)), c(2:10)] <- Gamean 

Smean <- apply(nutrients18[nutrients18$Comb=="81S",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "81S" & is.na(nutrients18$ADF)), c(2:10)] <- Smean 

Fmean <- apply(nutrients18[nutrients18$Comb=="810F",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "810F" & is.na(nutrients18$ADF)), c(2:10)] <- Fmean 

Mmean <- apply(nutrients18[nutrients18$Comb=="810M",c(2:10)],2,mean, na.rm=T)### 2 means 2 values.
nutrients18[which(nutrients18$Comb== "810M" & is.na(nutrients18$ADF)), c(2:10)] <- Mmean 

#Calculate C:N ratio2018

names(nutrients18)[2:10] <- c("NDF18", "ADF18", "ADL18", "Ca18", "K18", "Mg18", "Bio_P18", "Bio_N18", "C18")
str(nutrients18)
nutrients18$CN_ratio18 <- nutrients18$C18/nutrients18$Bio_N18

nutrients <- merge(nutrients17, nutrients18)
str(nutrients)


## Biomass 

#A2017 
biomass17<- read.table("Data/biomass_A17.txt", header = T)[c(1,10)]## If want to pick some column, do this [c(1:4)]
names(biomass17)[2] <- "Biomass17"
str(biomass17)

## A2018 
biomass18<- read.table("Data/biomass_A18.txt", header = T)[c(1,10)]#
names(biomass18)[2] <- "Biomass18"
str(biomass18)

biomass <- merge(biomass17, biomass18)

## Cover
cover17<-read.table("Data/Relative_perc_cover_A17.txt", header = T)

str(cover17)

Cover17 <- with(cover17, tapply(Total_cover, Plot_Nr, mean))

cover18<-read.table("Data/Relative_perc_cover_A18.txt", header = T)
str(cover18)
Cover18 <- with(cover18, tapply(Total_cover, Plot_Nr, mean))

cover <- data.frame("Plot_Nr" =unique(cover17$Plot_Nr), Cover17, Cover18)


Dat1 <- merge(Mean_funTraits, plot_description, all.x=T)
Dat2 <- merge(Dat1,Abundance)
Dat3 <- merge(Dat2, Feeding_rate )
Dat4 <- merge(Dat3, SLA_LDMC)
Dat5 <- merge(Dat4, MPD)
Dat6 <- merge(Dat5, nutrients)
Dat7 <- merge(Dat6, biomass)
AllTraits_1718 <- merge(Dat7, cover)
str(AllTraits_1718)  

### Write a new text file for clean dataset

write.table(AllTraits_1718,"Data/Full_Traits_1718.txt",row.names = FALSE)



