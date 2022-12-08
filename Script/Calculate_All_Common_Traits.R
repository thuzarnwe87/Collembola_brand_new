## 29062022

rm(list=ls())
#library(plyr)
#library(melt)
library(FD) # for CWM

setwd("N:/TZN_data/Collembola_brand_new")
#setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop

### Load SLA, LDMC data, 2017 and 2018
PlantTraits17<- read.table("Data/201708_SLA-LDMC.txt", header = T)
PlantTraits18<- read.table("Data/201808_SLA-LDMC.txt", header = T)###♦ Missing data, 400 obs. only
PlantTraits<- merge(PlantTraits17, PlantTraits18, all= T)


#Plot description
plot_description <- read.table("Data/plot_description.txt", header = T)
plot_description$Plot_Nr <-as.factor(plot_description$Plot_Nr)

plot_description2 <- plot_description
str(plot_description2)

plot_description2$Ni[plot_description2$Ni==0] <- "no_N"
plot_description2$Ni[plot_description2$Ni==1] <- "N"
plot_description2$Fz[plot_description2$Fz==0] <- "no_F"
plot_description2$Fz[plot_description2$Fz==1] <- "F"

trt <- with(plot_description2, paste(Ni, Fz, sep = ".")) ## to add next treatment row by coping the data

plot_description2 <- cbind(plot_description2, "Treatment" =trt)

### Cover_2yrs, Ok----
#### load the cover data 2017 and 2018, Hl(Plot 103) is NA in the orginal file
cover17 <-read.table("Data/201708_percentage_cover.txt", header = T)[c(3:6)]
cover18 <-read.table("Data/201808_percentage_cover.txt", header = T)[c(3:6)]

##2017
head(cover17)
cover17 <- subset(cover17, Species!="bg")
cover17 <- subset(cover17, Species!= "legume")
cover17 <- subset(cover17, Species!= "grass")
cover17 <- subset(cover17, Species!= "herb")
cover17 <- subset(cover17, Species!= "dead")


total_cover17<- aggregate(cover17$cover, by= list(cover17$Plot_Nr), FUN= sum)
names(total_cover17)[1] <- "Plot_Nr" 

cover17 <- merge(cover17, total_cover17, by= c("Plot_Nr"), all.x = T)
cover17$Relative_perc_cover <- (cover17$cover/cover17$x)*100
names(cover17)[5] <- "Total_cover"
head(cover17)

write.table(cover17,"Data/Relative_perc_cover_A17.txt",row.names = FALSE)

##2018
head(cover18)
cover18 <- subset(cover18, Species!= "bg")
cover18 <- subset(cover18, Species!= "legume")
cover18 <- subset(cover18, Species!= "grass")
cover18 <- subset(cover18, Species!= "herb")
cover18 <- subset(cover18, Species!= "dead")

total_cover18 <- aggregate(cover18$cover, by= list(cover18$Plot_Nr), FUN= sum)
names(total_cover18)[1] <- "Plot_Nr"

cover18 <- merge(cover18, total_cover18, by= c("Plot_Nr"), all.x = T)
cover18$Relative_perc_cover <- (cover18$cover/cover18$x)*100
names(cover18)[5] <- "Total_cover"

write.table(cover18,"Data/Relative_perc_cover_A18.txt",row.names = FALSE)


#All_cover <- rbind(cover17, cover18)
#cover_2yrs <- aggregate(All_cover[ ,6], by = list(All_cover$Plot_Nr), mean, na.rm=T)
#names(cover_2yrs)[1] <- "Plot_Nr"
#names(cover_2yrs)[2] <- "cover"
#str(cover_2yrs)
#cover_2yrs

## create datasets for CWM estimates----
## Load the cover data, first
#2018
cover18w <- data.frame(with(cover18, tapply(Relative_perc_cover,list(Plot_Nr, Species), mean)))
cover18wlist <- split(cover18w, plot_description2$Treatment)

## Merge SLA and LDMC data
traits18 <- merge(PlantTraits18, plot_description2, by= "Plot_Nr")
traits18 <- aggregate(PlantTraits18 [, c(3,4)], list(traits18$Species, traits18$Treatment), mean, na.rm=T)## NA is still
names(traits18)[1:2] <- c("Species", "Treatment")

# fill missing values for Poa and Anthoxanthum
Ptmean <- apply(traits18[traits18$Species=="Pt",c(3:4)],2,mean, na.rm=T)### 2 means 2 values.
Aomean <- apply(traits18[traits18$Species=="Ao",c(3:4)],2, mean, na.rm=T)

traits18[which(traits18$Species== "Pt" & is.na(traits18$SLA)), c(3,4)] <- matrix(rep(Ptmean, 2), nrow = 2, byrow = T)
#traits18[which(traits18$Species== "Pt" & is.na(traits18$LDMC)), c(3,4)] <- matrix(rep(Ptmean, 2), nrow = 2, byrow = T)# why LDMC is automatic?

traits18[which(traits18$Species== "Ao" & is.na(traits18$SLA)), c(3:4)] <- Aomean # This is another way "matrix(rep(Aomean, 2), nrow = 2, byrow = T)
## Make a split
traits18.list <- split(traits18, traits18$Treatment)

CWM.list18 <- list()
for (i in 1:4) {
  tt18 <- traits18.list[[i]]
  tt18 <- tt18 [, - c(1,2)]
  tt18 <- as.matrix(tt18)
  row.names(tt18) <- traits18.list[[i]]$Species
  ss18 <- as.matrix(cover18wlist[[i]])
  CWM.list18[[i]] <- functcomp(tt18,ss18)
}

CWM.mat18 <- do.call(rbind, CWM.list18) ## Not clear what happened
CWM.mat18 <- data.frame("Plot_Nr"= rownames(CWM.mat18), CWM.mat18)

#CWM.mat18$Plot_Nr.1<-NULL
#CWM.mat18$Plot_Nr.2<-NULL
head(CWM.mat18)

write.table(CWM.mat18,"Data/CWM_SLA_LDMC_A18.txt",row.names = FALSE)

#2017
#cover17w <- data.frame(with(cover17, tapply(Relative_perc_cover,list(Plot_Nr, Species))))# Without mean function, it didn't work

#cover17w <- with(cover17, tapply(Relative_perc_cover,list(Plot_Nr, Species), mean)) ## Here, don't use by= list....# Matrix
cover17w <- data.frame(with(cover17, tapply(Relative_perc_cover,list(Plot_Nr, Species), mean)))# Data frame and look at the column in the dataframe
cover17wlist <- split(cover17w, plot_description2$Treatment)
## Merge SLA and LDMC data
traits17 <- merge(PlantTraits17, plot_description2, by= "Plot_Nr")
traits17 <- aggregate(PlantTraits17 [, c(3,4)], list(traits17$Species, traits17$Treatment), mean, na.rm=T)## NA is still
names(traits17)[1:2] <- c("Species", "Treatment")

# fill missing values for Hs from 2018 data because there is no value in 2017
Hsmean <- apply(traits18[traits18$Species=="Hs",c(3:4)],2,mean, na.rm=T)### 
 
traits17[which(traits17$Species== "Hs" & is.na(traits17$SLA)), c(3,4)] <- matrix(rep(Hsmean, 4), nrow = 4, byrow = T)
traits17.list <- split(traits17, traits17$Treatment)

CWM.list17 <- list()
for (i in 1:4) {
  tt17 <- traits17.list[[i]]
  tt17 <- tt17 [, - c(1,2)]
  tt17 <- as.matrix(tt17)
  row.names(tt17) <- traits17.list[[i]]$Species
  ss17 <- as.matrix(cover17wlist[[i]])
  CWM.list17[[i]] <- functcomp(tt17,ss17)
}

CWM.mat17 <- do.call(rbind, CWM.list17) ## To have the value in all plots
CWM.mat17 <- data.frame("Plot_Nr"= rownames(CWM.mat17), CWM.mat17)

#CWM.mat17$Plot_Nr.1<-NULL
#CWM.mat17$Plot_Nr.2<-NULL
head(CWM.mat17)

write.table(CWM.mat17,"Data/CWM_SLA_LDMC_A17.txt",row.names = FALSE)

### MPD (Ok)----

library(FD)
#library(psych)
library(picante)

##MEan DIssimilarity Components##
#samp:  community matrix; sites in lines, species in columns
#dis:   dissimilarity matrix
#type:  "both" for results with abundances weighted and non-weighted
#       "abundance" for results with abundances weighted
#       "presence" for results with abundances non-weighted

source("N:/TZN_data/Functions/melodic.R")

## 2017
MPD.list17pr <- list()
MPD.list17sown <- list()

for (i in 1:4) {
  tt17 <- traits17.list[[i]]
  tt17 <- tt17 [, 3]
  tt17 <- as.matrix(tt17)
  row.names(tt17) <- traits17.list[[i]]$Species
  ## dissimilarity matrix
  diss <- gowdis(tt17)
  ss17 <- as.matrix(cover17wlist[[i]])
  ## for MPD based on species present
  ss17b <- ss17
  ss17b[is.na(ss17b)] <- 0
  ## for sown MPD
  ss17c <- ss17
  ss17c[!is.na(ss17c)] <- 1
  ss17c[is.na(ss17c)] <- 0
  
  mpdp <- melodic(ss17b,diss, type = "presence")
  mpdp <- as.matrix(mpdp$presence$mpd)
  rownames(mpdp) <- rownames(ss17) 
  MPD.list17pr[[i]] <- mpdp
  
  mpds <- melodic(ss17c,diss, type = "presence")
  mpds <- as.matrix(mpds$presence$mpd)
  rownames(mpds) <- rownames(ss17) 
  MPD.list17sown[[i]] <- mpds
}

MPD.mat17pr <- do.call(rbind, MPD.list17pr) ## To have the value in all plots
MPD.mat17pr <- data.frame("Plot_Nr"= rownames(MPD.mat17pr), MPD.mat17pr)


MPD.mat17sw <- do.call(rbind, MPD.list17sown) ## To have the value in all plots
MPD.mat17sw <- data.frame("Plot_Nr"= rownames(MPD.mat17sw), MPD.mat17sw)

MPD17<- merge(MPD.mat17pr, MPD.mat17sw)
names(MPD17)[2:3] <- c("MPD_Pre", "MPD_Sown")
str(MPD17)
MPD17[is.na(MPD17)] = 0


write.table(MPD17,"Data/MPD_A17.txt",row.names = FALSE)

## Check whether monoculture are NA
#Check_MPD_data17<-merge(MPD.mat17pr, plot_description, all = T)
  
## 2018


MPD.list18pr <- list()
MPD.list18sown <- list()

for (i in 1:4) {
  tt18 <- traits18.list[[i]]
  tt18 <- tt18 [, 3]
  tt18 <- as.matrix(tt18)
  row.names(tt18) <- traits18.list[[i]]$Species
  ## dissimilarity matrix
  diss <- gowdis(tt18)
  ss18 <- as.matrix(cover18wlist[[i]])
  ## for MPD based on species present
  ss18b <- ss18
  ss18b[is.na(ss18b)] <- 0
  ## for sown MPD
  ss18c <- ss18
  ss18c[!is.na(ss18c)] <- 1
  ss18c[is.na(ss18c)] <- 0
  
  mpdp <- melodic(ss18b,diss, type = "presence")
  mpdp <- as.matrix(mpdp$presence$mpd)
  rownames(mpdp) <- rownames(ss18) 
  MPD.list18pr[[i]] <- mpdp
  
  mpds <- melodic(ss18c,diss, type = "presence")
  mpds <- as.matrix(mpds$presence$mpd)
  rownames(mpds) <- rownames(ss18) 
  MPD.list18sown[[i]] <- mpds
}

MPD.mat18pr <- do.call(rbind, MPD.list18pr) ## To have the value in all plots
MPD.mat18pr <- data.frame("Plot_Nr"= rownames(MPD.mat18pr), MPD.mat18pr)

MPD.mat18sw <- do.call(rbind, MPD.list18sown) ## To have the value in all plots
MPD.mat18sw <- data.frame("Plot_Nr"= rownames(MPD.mat18sw), MPD.mat18sw)

MPD18<- merge(MPD.mat18pr, MPD.mat18sw)
names(MPD18)[2:3] <- c("MPD_Pre", "MPD_Sown")
MPD18[is.na(MPD18)] = 0 # Change NA in monoculture as zero

write.table(MPD18,"Data/MPD_A18.txt",row.names = FALSE)

## Check whether monoculture are NA
#Check_MPD_data18<-merge(MPD.mat18pr, plot_description, all = T)



