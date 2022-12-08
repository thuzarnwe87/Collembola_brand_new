
rm(list=ls())

library(factoextra) ## To run PCA
library(FactoMineR)## To run PCA
library(cowplot) ## To combine PCA graph
library(dplyr) ## Mean group by function
setwd("N:/TZN_data/Collembola_brand_new")
#setwd("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new")
#setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop


## Mean functional traits and individual2017, 18082022, 16092022, 19102022
fun_trait17<-read.table("Data/Fun_trait_collembola2017.txt", header = T,stringsAsFactors = F) [c(1:2,8:18)]
str(fun_trait17)

fun_trait17$Block<-as.factor(fun_trait17$Block)
fun_trait17$Plot_Nr<-as.factor(fun_trait17$Plot_Nr)
fun_trait17$Body_length<-as.numeric(fun_trait17$Body_length) # NAS warning
fun_trait17$Chewing_Group<-as.numeric(fun_trait17$Chewing_Group)
fun_trait17$Sucking_Group<-as.numeric(fun_trait17$Sucking_Group)
str(fun_trait17)


# change NA instead of zero because there is no collembola in some plots

fun_trait17t <- fun_trait17[,c(3:4,7:12)]
fun_trait17t[fun_trait17t==0] <- NA
str(fun_trait17t)

fun_trait17 <- cbind(fun_trait17[,c(1:2,5:6,13)], fun_trait17t)

str(fun_trait17)

fun_trait18<-read.table("Data/Fun_trait_collembola2018.txt", header = T)
str(fun_trait18)

fun_trait18$Block<-as.factor(fun_trait18$Block)
fun_trait18$Plot_Nr<-as.factor(fun_trait18$Plot_Nr)
fun_trait18$Chewing_Group<-as.numeric(fun_trait18$Chewing_Group)
fun_trait18$Sucking_Group<-as.numeric(fun_trait18$Sucking_Group)
#summary(fun_trait18)
str(fun_trait18)


fun_trait18t <- fun_trait18[,c(3:4,7:12)]
fun_trait18t[fun_trait18t==0] <- NA
str(fun_trait18t)
str(fun_trait18)

fun_trait18 <- cbind(fun_trait18[,c(1:2,5:6,13)], fun_trait18t)

## combined
fun_traitall <- rbind(fun_trait17, fun_trait18)
str(fun_traitall)


### PCA Individuls for two years and mean----
str(fun_traitall)
names(fun_traitall)
names(fun_traitall)[6] <- "Body length"
names(fun_traitall)[8:13] <- c("Mandible length", "Mandible/head ratio","Mechanical advantage", "Deployment capacity","Apical development", "Molar plate length") 



Col_pca<-fun_traitall[,c(6, 8:13)]

Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:7)]<-scale(Col_pca[,c(1:7)])

pca_fun_traitall<-prcomp(Col_pca[,c(1:7)])
plot1 <- fviz_pca_biplot(pca_fun_traitall,label='var',geom =c( "text", "point"),
                MAP = "symbiplot",
                labelsize = 7,
                repel = TRUE,
                title = " " ) +
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        axis.title.x=element_text(size=16, face = "bold",colour = "black"),
        axis.text.x=element_text(size=16, face = "bold",colour = "black"),
        axis.title.y=element_text(size=16, face = "bold",colour = "black"),
        axis.text.y=element_text(size=16, face = "bold",colour = "black"),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_rect(color = "black",fill = NA, size = 1.2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
plot1



# do plot mean all data
#aggregate(fun_trait17[,-1],list(fun_trait17$Plot_Nr),mean,na.rm=T)# This is the same way
plotmean17 <- aggregate(fun_trait17[,c(6:13)],list(fun_trait17$Plot_Nr),mean,na.rm=T)
names(plotmean17)[1]<-"Plot_Nr"
str(plotmean17)

#aggregate(fun_trait18[,-1],list(fun_trait18$Plot_Nr),mean,na.rm=T)# This is the same way
plotmean18 <- aggregate(fun_trait18[,c(6:13)],list(fun_trait18$Plot_Nr),mean,na.rm=T)
names(plotmean18)[1]<-"Plot_Nr" 
str(plotmean18)


Fun_traits1718 <- rbind(plotmean17, plotmean18)
Fun_traits_2yrs <- aggregate(Fun_traits1718[,2:9], by= list(Fun_traits1718$Plot_Nr), mean, na.rm=T)
names(Fun_traits_2yrs)[1] <- "Plot_Nr"
str(Fun_traits_2yrs)


#Mean functional traits 2017 and 2018
head(Fun_traits_2yrs)
names(Fun_traits_2yrs)
names(Fun_traits_2yrs)[2] <- "Body length"
names(Fun_traits_2yrs)[4:9] <- c("Mandible length", "Mandible/head ratio","Mechanical advantage", "Deployment capacity","Apical development", "Molar plate length") 


Col_pca<-Fun_traits_2yrs[,c(2, 4:9)]
#pdf("Figure/pca_mean functional traits.pdf",6,8)
#tiff("FigureTiff/pca_mean functional traits.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#


Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:7)]<-scale(Col_pca[,c(1:7)])

pca_Fun_traits_2yrs<-prcomp(Col_pca[,c(1:7)])

plot2 <- fviz_pca_biplot(pca_Fun_traits_2yrs,label='var',geom =c( "text", "point"),
                         MAP = "symbiplot",
                         labelsize = 7,
                         repel = TRUE,
                         title = " ") +
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        axis.title.x=element_text(size=16, face = "bold",colour = "black"),
        axis.text.x=element_text(size=16, face = "bold",colour = "black"),
        axis.title.y=element_text(size=16, face = "bold",colour = "black"),
        axis.text.y=element_text(size=16, face = "bold",colour = "black"),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_rect(color = "black",fill = NA, size = 1.2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
plot2

#fviz_pca_biplot(pca_Fun_traits_2yrs,label='var',gridlines.major = FALSE, gridlines.minor = FALSE)# can't remove gridlines
#fviz_pca_biplot(pca_Fun_traits_2yrs,label='var', axes = c(1,3))
summary(pca_Fun_traits_2yrs)
pca_Fun_traits_2yrs$rotation

tiff("Figure_all/Figure_PCA/pca_functional traits_mean_indi2017.tiff", width = 35, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot2,plot1, align = "h",  axis = "b",
          rel_widths = c(1, 1), ncol=2, nrow=1, labels = c("(a)", "(b)"))

dev.off()




##only 94 plots 2017 individual (Done)15082022, 18082022, 16092022,19102022----

str(fun_trait17)
library(dplyr)### have to install for nonwanted level

nonWantedLevels<-c(6,11,16,17,18,20,24,25,26,32,33,36,40,44,47,48,49,50,54,66,
                   67,68,69,70,71,74,75,78,79,81,82,90,97,98,100,171,174,178,180,
                   182,183,184,185,190,191,195,198,199,200,201,202,205,206,208,210,212,215,216,218,
                   219,224,225,228,233,234,241,242,247,250,251,291,
                   
                   21,42,56,119,129,150,158,222,229,232,262,266,273,298,312,326,334) 
## (The first 4 lines are label lost plots and The last line is NA plots in mounting process)

indi17_94<-fun_trait17 %>%
  filter(!Plot_Nr %in% nonWantedLevels)%>%
  droplevels()

str(indi17_94)

levels(indi17_94$Plot_Nr)

head(indi17_94)
names(indi17_94)
Col_pca<-indi17_94[,c(6, 8:13)]
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:7)]<-scale(Col_pca[,c(1:7)])

pca_indi17_94<-prcomp(Col_pca[,c(1:7)])

tiff("Figure_all/Figure_PCA/pca_individual_functional traits2017_94plots.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

fviz_pca_biplot(pca_indi17_94,label='var',geom =c( "text", "point"),
                MAP = "symbiplot",
                labelsize = 7,
                repel = TRUE,
                title = " ") +
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        axis.title.x=element_text(size=16,face = "bold", colour = "black"),
        axis.text.x=element_text(size=16, face = "bold",colour = "black"),
        axis.title.y=element_text(size=16, face = "bold",colour = "black"),
        axis.text.y=element_text(size=16, face = "bold",colour = "black"),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_rect(color = "black",fill = NA, size = 1.2),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

dev.off()


fviz_pca_biplot(pca_indi17_94,label='var',axes = c(1,3))
summary(pca_indi17_94)
pca_indi17_94$rotation

#### Old----
# For 2017 individual
names(fun_trait17)
Col_pca<-fun_trait17[,c(6, 8:13)]
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:7)]<-scale(Col_pca[,c(1:7)])

pca_fun_trait17<-prcomp(Col_pca[,c(1:7)])
fviz_pca_biplot(pca_fun_trait17,label='var')
fviz_pca_biplot(pca_fun_trait17,label='var', axes = c(1,3))
summary(pca_fun_trait17)
pca_fun_trait17$rotation


## For individual 2018
str(fun_trait18)
names(fun_trait18)
Col_pca<-fun_trait18[,c(6, 8:13)]
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:7)]<-scale(Col_pca[,c(1:7)])

pca_fun_trait18<-prcomp(Col_pca[,c(1:7)])
fviz_pca_biplot(pca_fun_trait18,label='var')
fviz_pca_biplot(pca_fun_trait18,label='var', axes = c(1,3))
summary(pca_fun_trait18)
pca_fun_trait18$rotation

### PCA with/without Abundance with mean functional traits----
source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")
names(mydata)

Col_pca<-mydata[,c(8, 10:16)]
#pdf("Figure/pca_mean functional traits with Abundance.pdf",6,8)
tiff("FigureTiff/pca_mean functional traits with Abundance.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:8)]<-scale(Col_pca[,c(1:8)])

pca_Fun_Abund_2yrs<-prcomp(Col_pca[,c(1:8)])
fviz_pca_biplot(pca_Fun_Abund_2yrs,label='var')
#fviz_pca_biplot(pca_Fun_Abund_2yrs,label='var', axes = c(1,3))
summary(pca_Fun_Abund_2yrs)
pca_Fun_Abund_2yrs$rotation
dev.off()



## Nirs+ all traits, 06082022----
source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")
# load 3. Models_scripts.R
str(mydata)
head(mydata)
names(mydata)

#Mean Nirs
Col_pca<-mydata[,c(22:34)]
Col_pca<-na.omit(Col_pca)


Col_pca[,c(1:13)]<-scale(Col_pca[,c(1:13)])

pca_NIRS<-prcomp(Col_pca[,c(1:13)])
fviz_pca_biplot(pca_NIRS,label='var')
fviz_pca_biplot(pca_NIRS,label='var', axes = c(1,3))
summary(pca_NIRS)
pca_NIRS$rotation

#2017 Nirs
str(mydata)
head(mydata)
names(mydata)

Col_pca<-mydata[,c(33,63:72, 83,85)]
Col_pca<-na.omit(Col_pca)


Col_pca[,c(1:13)]<-scale(Col_pca[,c(1:13)])

pca_NIRS<-prcomp(Col_pca[,c(1:13)])
fviz_pca_biplot(pca_NIRS,label='var')
fviz_pca_biplot(pca_NIRS,label='var', axes = c(1,3))
summary(pca_NIRS)
pca_NIRS$rotation

#Mean SLA...
Col_pca<-mydata[,c(18:20)]
Col_pca<-na.omit(Col_pca)


Col_pca[,c(1:3)]<-scale(Col_pca[,c(1:3)])

pca_NIRS<-prcomp(Col_pca[,c(1:3)])
fviz_pca_biplot(pca_NIRS,label='var')
#fviz_pca_biplot(pca_NIRS,label='var', axes = c(1,3))
summary(pca_NIRS)
pca_NIRS$rotation

#2017 SLa...
str(mydata)
head(mydata)
names(mydata)

Col_pca<-mydata[,c(55:56,59)]
Col_pca<-na.omit(Col_pca)


Col_pca[,c(1:3)]<-scale(Col_pca[,c(1:3)])

pca_NIRS<-prcomp(Col_pca[,c(1:3)])
fviz_pca_biplot(pca_NIRS,label='var')
fviz_pca_biplot(pca_NIRS,label='var', axes = c(1,3))
summary(pca_NIRS)
pca_NIRS$rotation


### Abundance with functional traits and Nirs06082022----

source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")
str(mydata)
head(mydata)
names(mydata)

#Mean
#ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass
Col_pca<-mydata[,c(8:16,22,24,31:34)]## Abundance, fun traits, Nirs (AFN)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:15)]<-scale(Col_pca[,c(1:15)])

pca_AFN<-prcomp(Col_pca[,c(1:15)])
fviz_pca_biplot(pca_AFN,label='var')
fviz_pca_biplot(pca_AFN,label='var', axes = c(1,3))
summary(pca_AFN)
pca_AFN$rotation

#2017
names(mydata)
#ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass
Col_pca<-mydata[,c(33,35:42,51,63,65,72,83)]## Abundance, fun traits, Nirs (AFN17)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:14)]<-scale(Col_pca[,c(1:14)])

pca_AFN17<-prcomp(Col_pca[,c(1:14)])
fviz_pca_biplot(pca_AFN17,label='var')
fviz_pca_biplot(pca_AFN17,label='var', axes = c(1,3))
summary(pca_AFN17)
pca_AFN17$rotation

#2018
names(mydata)
#ADF18+ Ca18+ CN_ratio18+ Biomass18+ Rootbiomass
Col_pca<-mydata[,c(33,43:50,52,74,76,82,84)]## Abundance, fun traits, Nirs (AFN17)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:14)]<-scale(Col_pca[,c(1:14)])

pca_AFN18<-prcomp(Col_pca[,c(1:14)])
fviz_pca_biplot(pca_AFN18,label='var')
fviz_pca_biplot(pca_AFN18,label='var', axes = c(1,3))
summary(pca_AFN18)
pca_AFN18$rotation




### Feeding activity with functional traits and Nirs06082022----

names(mydata)

#Mean
#ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass
Col_pca<-mydata[,c(8:13,15,20,22,29:31)]## Feeding, fun traits, Nirs (AFN)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:12)]<-scale(Col_pca[,c(1:12)])

pca_FFN<-prcomp(Col_pca[,c(1:12)])
fviz_pca_biplot(pca_FFN,label='var')
fviz_pca_biplot(pca_FFN,label='var', axes = c(1,3))
summary(pca_FFN)
pca_FFN$rotation



#2018
names(mydata)
#ADF18+ Ca18+ CN_ratio18+ Biomass18+ Rootbiomass
Col_pca<-mydata[,c(31,40:45,48, 70,72,78,80)]## Feeding, fun traits, Nirs (AFN17)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:12)]<-scale(Col_pca[,c(1:12)])

pca_FFN18<-prcomp(Col_pca[,c(1:12)])
fviz_pca_biplot(pca_FFN18,label='var')
fviz_pca_biplot(pca_FFN18,label='var', axes = c(1,3))
summary(pca_FFN18)
pca_FFN18$rotation

### for 2022 is far with other data

#### Comparing every log data 06082022----
### Abundance with functional traits and Nirs

source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")
str(mydata)
head(mydata)
names(mydata)

#Mean
#ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass
Col_pca<-mydata[,c(20,22,31:33,87:94,111)]## Abundance, fun traits, Nirs (AFN)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:14)]<-scale(Col_pca[,c(1:14)])

pca_AFN<-prcomp(Col_pca[,c(1:14)])
fviz_pca_biplot(pca_AFN,label='var')
fviz_pca_biplot(pca_AFN,label='var', axes = c(1,3))
summary(pca_AFN)
pca_AFN$rotation

#2017
names(mydata)
#ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass
Col_pca<-mydata[,c(32:33,55,59,63,65,72,83,95:102,112)]## Abundance, fun traits, Nirs (AFN17)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:14)]<-scale(Col_pca[,c(1:14)])

pca_AFN17<-prcomp(Col_pca[,c(1:14)])
fviz_pca_biplot(pca_AFN17,label='var')
fviz_pca_biplot(pca_AFN17,label='var', axes = c(1,3))
summary(pca_AFN17)
pca_AFN17$rotation

#2018
names(mydata)
#ADF18+ Ca18+ CN_ratio18+ Biomass18+ Rootbiomass
Col_pca<-mydata[,c(33,57,72,61,74,76,82,103:110, 113)]## Abundance, fun traits, Nirs (AFN17)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:14)]<-scale(Col_pca[,c(1:14)])

pca_AFN18<-prcomp(Col_pca[,c(1:14)])
fviz_pca_biplot(pca_AFN18,label='var')
fviz_pca_biplot(pca_AFN18,label='var', axes = c(1,3))
summary(pca_AFN18)
pca_AFN18$rotation




### Feeding activity with functional traits and Nirs--

names(mydata)

#Mean
#ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass
Col_pca<-mydata[,c(83:88,104,20,22,29:31)]## Abundance, fun traits, Nirs (AFN)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:12)]<-scale(Col_pca[,c(1:12)])

pca_FFN<-prcomp(Col_pca[,c(1:12)])
fviz_pca_biplot(pca_FFN,label='var')
fviz_pca_biplot(pca_FFN,label='var', axes = c(1,3))
summary(pca_FFN)
pca_FFN$rotation



#2018
names(mydata)
#ADF18+ Ca18+ CN_ratio18+ Biomass18+ Rootbiomass
Col_pca<-mydata[,c(31,70,72,78,80,95:100,105)]## Abundance, fun traits, Nirs (AFN17)
Col_pca<-na.omit(Col_pca)

Col_pca[,c(1:12)]<-scale(Col_pca[,c(1:12)])

pca_FFN18<-prcomp(Col_pca[,c(1:12)])
fviz_pca_biplot(pca_FFN18,label='var')
fviz_pca_biplot(pca_FFN18,label='var', axes = c(1,3))
summary(pca_FFN18)
pca_FFN18$rotation

### for 2022 is far with data









