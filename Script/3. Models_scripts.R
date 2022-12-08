
## Full clean data
rm(list=ls())
setwd("N:/TZN_data/Collembola_brand_new")
#setwd("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new")
#setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop

## Install the package
library(lme4)
library(gridExtra)
library(effects)## for plot
library(ggplot2)
library(corrplot)
library(dplyr) ## to calculate mean

# functions
source("N:/TZN_data/Functions/stepba function.R")
source("N:/TZN_data/Functions/graphic_themes.R")
#source("~/Documents/ThuZar/TZN_data/Functions/stepba function.R")## For TZN laptop
#source("~/Documents/ThuZar/TZN_data/Functions/graphic_themes.R")## For TZN laptop

## Load the data files
## Full traits (see for detail at Data_Frame_all)
All_fulltraits<-read.table("Data/Full_Mean_Traits_2yrs.txt", header = T)

str(All_fulltraits)

AllTraits_1718<-read.table("Data/Full_Traits_1718.txt", header = T)
str(AllTraits_1718)

#Plot description
plot_description<- read.table("Data/plot_description.txt", header = T)
plot_description$Plot_Nr<-as.factor(plot_description$Plot_Nr)



## Mearge the data file
data1<- merge(All_fulltraits,plot_description, all.x = T )
str(data1)

mydata<-merge(data1,AllTraits_1718, na.rm=T, all.x= T)
str(mydata)  
  
mydata$Fz<- as.factor(mydata$Fz)
mydata$Ni<- as.factor(mydata$Ni)
mydata$Plot_Nr<- as.factor(mydata$Plot_Nr)
mydata$Block<- as.factor(mydata$Block)
mydata$Abundance <- as.numeric(mydata$Abundance)
mydata$Abundance17 <- as.numeric(mydata$Abundance17)
mydata$Abundance18 <- as.numeric(mydata$Abundance18)
names(mydata)[1]<-"Plot"
str(mydata)
names(mydata)[5]<-"N"
names(mydata)[6]<-"F"
str(mydata)

# logtransform species diversity and also all the variables
mydata$logBody_length <-log(mydata$ Body_length)
mydata$logHead_length <-log(mydata$ Head_length)
mydata$logMandible_length <-log(mydata$ Mandible_length)
mydata$logMandible_head_ratio <-log(mydata$ Mandible_head_ratio)
mydata$logMechanical_advantage <-log(mydata$ Mechanical_advantage)
mydata$logDeployment_capacity <-log(mydata$ Deployment_capacity)
mydata$logApical_development <-log(mydata$ Apical_development)
mydata$logMolar_plate_length <-log(mydata$ Molar_plate_length)

mydata$logBody_length17 <-log(mydata$ Body_length17)
mydata$logHead_length17 <-log(mydata$ Head_length17)
mydata$logMandible_length17 <-log(mydata$Mandible_length17)
mydata$logMandible_head_ratio17 <-log(mydata$Mandible_head_ratio17)
mydata$logMechanical_advantage17 <-log(mydata$Mechanical_advantage17)
mydata$logDeployment_capacity17 <-log(mydata$Deployment_capacity17)
mydata$logApical_development17 <-log(mydata$Apical_development17)
mydata$logMolar_plate_length17 <-log(mydata$Molar_plate_length17)

mydata$logBody_length18 <-log(mydata$ Body_length18)
mydata$logHead_length18 <-log(mydata$ Head_length18)
mydata$logMandible_length18 <-log(mydata$Mandible_length18)
mydata$logMandible_head_ratio18 <-log(mydata$Mandible_head_ratio18)
mydata$logMechanical_advantage18 <-log(mydata$Mechanical_advantage18)
mydata$logDeployment_capacity18 <-log(mydata$Deployment_capacity18)
mydata$logApical_development18 <-log(mydata$Apical_development18)
mydata$logMolar_plate_length18 <-log(mydata$Molar_plate_length18)

mydata$logAbundance <-log(mydata$ Abundance)
mydata$logAbundance17 <-log(mydata$ Abundance17)
mydata$logAbundance18 <-log(mydata$ Abundance18)

mydata$logFeeding_rate<-log(mydata$ Feeding_rate)
mydata$logFeeding_rate18<-log(mydata$ Feeding_rate18)
mydata$logFeeding_rate22<-log(mydata$ Feeding_rate22)

# Square root all the variables because 4 traits of them are the best AIC.
mydata$sqrtBody_length <-sqrt(mydata$ Body_length)
mydata$sqrtHead_length <-sqrt(mydata$ Head_length)
mydata$sqrtMandible_length <- sqrt(mydata$Mandible_length)
mydata$sqrtMandible_head_ratio <- sqrt(mydata$Mandible_head_ratio)
mydata$sqrtMechanical_advantage <- (mydata$ Mechanical_advantage)
mydata$sqrtDeployment_capacity <- sqrt(mydata$ Deployment_capacity)
mydata$sqrtApical_development <- sqrt(mydata$ Apical_development)
mydata$sqrtMolar_plate_length <- sqrt(mydata$ Molar_plate_length)

mydata$sqrtBody_length17 <-sqrt(mydata$ Body_length17)
mydata$sqrtHead_length17 <-sqrt(mydata$ Head_length17)
mydata$sqrtMandible_length17 <- sqrt(mydata$Mandible_length17)
mydata$sqrtMandible_head_ratio17 <- sqrt(mydata$Mandible_head_ratio17)
mydata$sqrtMechanical_advantage17 <- sqrt(mydata$Mechanical_advantage17)
mydata$sqrtDeployment_capacity17 <- sqrt(mydata$Deployment_capacity17)
mydata$sqrtApical_development17 <- sqrt(mydata$Apical_development17)
mydata$sqrtMolar_plate_length17 <- sqrt(mydata$Molar_plate_length17)

mydata$sqrtBody_length18 <-sqrt(mydata$ Body_length18)
mydata$sqrtHead_length18 <-sqrt(mydata$ Head_length18)
mydata$sqrtMandible_length18 <- sqrt(mydata$Mandible_length18)
mydata$sqrtMandible_head_ratio18 <- sqrt(mydata$Mandible_head_ratio18)
mydata$sqrtMechanical_advantage18 <- sqrt(mydata$Mechanical_advantage18)
mydata$sqrtDeployment_capacity18 <- sqrt(mydata$Deployment_capacity18)
mydata$sqrtApical_development18 <- sqrt(mydata$Apical_development18)
mydata$sqrtMolar_plate_length18 <- sqrt(mydata$Molar_plate_length18)

mydata$sqrtAbundance <- sqrt(mydata$ Abundance)
mydata$sqrtAbundance17 <- sqrt(mydata$ Abundance17)
mydata$sqrtAbundance18 <- sqrt(mydata$ Abundance18)

mydata$sqrtFeeding_rate<- sqrt(mydata$ Feeding_rate)
mydata$sqrtFeeding_rate18<- sqrt(mydata$ Feeding_rate18)
mydata$sqrtFeeding_rate22<- sqrt(mydata$ Feeding_rate22)


mydata<-as.data.frame(mydata)
#mydata <- na.omit(mydata) #dropped every rows  only 80 plots
str(mydata)

#Scale dataset
mydatas<- mydata %>% mutate_if(is.numeric, scale)
str(mydatas)


