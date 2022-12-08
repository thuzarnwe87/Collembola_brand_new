
rm(list=ls())
library(gplots)
library(ggplot2)
library(cowplot)

source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")
source("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new/Script/3. Models_scripts.R")
setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop

## If source(-), didn't work, load 3. Models_scripts.R, first

### For 2017, Label lost plots (71), lost samples during mounting process (17),Zero plots (34)= Total 94 plots left (2017) and 
#For mean, = 103 plots total by adding 9 plots from 2018 in zero plots.
mydatas2 <- mydatas
str(mydatas2)
library(dplyr)### have to install for nonwanted level

nonWantedLevels<-c(6,11,16,17,18,20,24,25,26,32,33,36,40,44,47,48,49,50,54,66,
                   67,68,69,70,71,74,75,78,79,81,82,90,97,98,100,171,174,178,180,
                   182,183,184,185,190,191,195,198,199,200,201,202,205,206,208,210,212,215,216,218,
                   219,224,225,228,233,234,241,242,247,250,251,291,
                   
                   21,42,56,119,129,150,158,222,229,232,262,266,273,298,312,326,334) 
## (The first 4 lines are label lost plots and The last line is NA plots in mounting process)

mydatas2_Rmna<-mydatas2 %>%
  filter(!Plot %in% nonWantedLevels)%>%
  droplevels()

str(mydatas2_Rmna)

### For graph 94 plots (2017) and 103 plots (Mean)_rmNA
mydata2 <- mydata
str(mydata2)
library(dplyr)### have to install for nonwanted level

nonWantedLevels<-c(6,11,16,17,18,20,24,25,26,32,33,36,40,44,47,48,49,50,54,66,
                   67,68,69,70,71,74,75,78,79,81,82,90,97,98,100,171,174,178,180,
                   182,183,184,185,190,191,195,198,199,200,201,202,205,206,208,210,212,215,216,218,
                   219,224,225,228,233,234,241,242,247,250,251,291,
                   
                   21,42,56,119,129,150,158,222,229,232,262,266,273,298,312,326,334) 
## (The first 4 lines are label lost plots and The last line is NA plots in mounting process)

mydata2_Rmna<-mydata2 %>%
  filter(!Plot %in% nonWantedLevels)%>%
  droplevels()

str(mydata2_Rmna)

##◘ nice graph for Abundance and feeding 08082020,15082022----
#log(Abundance+1)
#log(Abundance17+1)
#log(Abundance18+1)

#Feeding_rate
#Feeding_rate18
#Feeding_rate22


# Choose the one with lower AIC and log transformation with log data
## Best transforation of y

Lm1 <- lmer(Abundance   ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
Lm2 <- lmer(sqrtAbundance  ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F) 
Lm3 <- lmer(log(Abundance+1)   ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
anova(Lm1,Lm2,Lm3) ## Anova didn't work with scaled data 
# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)

### Abundance
m <- lmer(log(Abundance+1) ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
summary(m)
stepba(m)# N(*)

Final_Adund <- lmer(formula = log(Abundance + 1) ~ N + (1 | Block), data = mydata, 
                    REML = F)
plot(allEffects(Final_Adund))
#N
coef(summary(Final_Adund))        #here you can see all estimates
ef <- effect("N", Final_Adund)	#select only the one for Fungicide
ef    #These are the estimates you want to plot
x <- as.data.frame(ef)  #now they are nicely in a dataframe
x

levels(x$N)[levels(x$N)=="0"] <- "no nitrogen"       #This is to change the 0 into "no fungicide"
levels(x$N)[levels(x$N)=="1"] <- "nitrogen"          
str(x)

tiff("Figure_all/Figure_mainEffect/nice_graph_Abundance_maineffect_N_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

#With the R package ggplot you can do nice graphs

ggplot(x, aes(y=fit, ymax=upper, ymin=lower, x=N, colour=N))+
  scale_colour_manual(values=c("#99CCFF", "#336699"))+
  geom_errorbar(aes(ymin= lower, ymax=upper, color=N),position = position_dodge(0.3),width=0.05, lwd=2)+
  geom_point(size=6, shape = 15, position = position_dodge(0.3))+
  labs(y = "Abundance of collembola")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title.y  = element_text(size = 16, face = "bold",colour = "black"),
        axis.title.x   = element_blank(),
        legend.text =element_blank(),
        legend.title=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black",size = 1.2),legend.position = "none")


dev.off()

m <- lmer(log(Abundance+1)~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17+ ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass17+ (1|Block), mydatas,REML = F)
summary(m)
stepba(m)# N(*)



### Feeding_rate
m <- lmer(Feeding_rate~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
summary(m)
stepba(m)#  Biomass(*), Rootbiomass(*)
plot(allEffects(final_m))

final_m <- lmer(formula = Feeding_rate ~ Biomass + Rootbiomass + (1 | Block), 
                data = mydata, REML = F)


### Biomass
ef <- effect("Biomass", final_m , xlevels=list(Biomass=seq(0.315,112.020,14.741)))
df_Biomass <- as.data.frame(ef)  #make a dataframe out of it

MFD_A <- ggplot(df_Biomass, aes(y=fit, ymin=lower, ymax=upper, x=Biomass))+ 
  geom_smooth(method="lm",lwd= 2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Feeding rate")+
  labs (x="Aboveground biomass [g]")+
  
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        axis.title.x=element_text(size=24, face = "bold",colour = "black"),
        axis.text.x=element_text(size=18, face = "bold",colour = "black"),
        axis.title.y=element_text(size=24, face = "bold",colour = "black"),
        axis.text.y=element_text(size=18, face = "bold",colour = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black",size = 1.2))


### Root Biomass
ef <- effect("Rootbiomass", final_m , xlevels=list(Rootbiomass=seq(9.774,257.635,2878.205)))
df_RtBiomass <- as.data.frame(ef)  #make a dataframe out of it

MFD_B <- ggplot(df_RtBiomass, aes(y=fit, ymin=lower, ymax=upper, x=Rootbiomass))+ 
  geom_smooth(method="lm", lwd= 2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Feeding rate")+
  labs (x="Belowground biomass [mg]")+
  
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        axis.title.x=element_text(size=24, face = "bold",colour = "black"),
        axis.text.x=element_text(size=18, face = "bold",colour = "black"),
        axis.title.y=element_text(size=24, face = "bold",colour = "black"),
        axis.text.y=element_text(size=18, face = "bold",colour = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black",size = 1.2))




library(cowplot)
tiff("Figure_all/Figure_mainEffect/nice_graph_Feeding_maineffects_unscale.tiff.tiff", width = 35, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(MFD_A,MFD_B, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=2, nrow=1, labels = c("(a)", "(b)"))

dev.off()


### ### Main effects on functional traits Mean 2017 and 2018----

# 05072022, 07072022,08082020, 09082020,15082022,19082022,19102022
# Model
# For meaN 2017 AND 2018
# sqrtBody_length
# logMandible_length
# Mandible_head_ratio
# Mechanical_advantage, sqrtMechanical_advantage
# Deployment_capacity
# logApical_development
# logMolar_plate_length


Lm1 <- lmer(Molar_plate_length ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtMolar_plate_length ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(logMolar_plate_length ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3)

# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)



### Bar graph for seven traits --
## Mean
### Body_length 
#m <- lmer(sqrtBody_length~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(sqrtBody_length ~  Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N +(1|Block), mydatas,REML = F) # reorder
#m <- lmer(sqrtBody_length ~  Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+F+Ca+CN_ratio+N +(1|Block), mydatas,REML = F) # reorder
str(mydata)
names(mydata)
## To run tiff, first run all, and second leave tiff code and third run again all and dev.off() will get plot

tiff("Figure_all/Figure_mainEffect/barplot.maineffects._mandible_traits.tiff", width = 39, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

m <- lmer(sqrtBody_length ~   Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F+(1|Block), mydatas,REML = F) # reorder

summary(m)
stepba(m)#  SD(.)


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)

#pdf("Figure/barplot.effects.Body_length.pdf",6,8)
#tiff("FigureTiff/barplot.effects.Body_length.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.Body_length.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Body length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))
             
#labels = c("Root biomass", "Biomass", "ADF","LDMC","SLA", "MPD","SD","Ca","C:N", "N","F"
xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot1 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()



### Mandible_length 
m <- lmer(logMandible_length~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(logMandible_length ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logMandible_length ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F+ (1|Block), mydatas,REML = F) # reorder

summary(m)
stepba(m)#  SD(*), SLA(*), CN_ratio(*) 


### for Bar graph
library(gplots)

coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)

#pdf("Figure/barplot.effects.mandible_length.pdf",6,8)
#tiff("FigureTiff/barplot.effects.mandible_length.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.mandible_length.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mandible length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot2 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


# Mandible_head_ratio
m <- lmer(Mandible_head_ratio ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(Mandible_head_ratio ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(Mandible_head_ratio ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F+ (1|Block), mydatas,REML = F) # reorder

summary(m)
stepba(m)# CN_ratio(*), N(.) 

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""

#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)


#pdf("Figure/barplot.effects.mandible_head_ratio.pdf",6,8)
#tiff("FigureTiff/barplot.effects.mandible_head_ratio.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.mandible_head_ratio.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mandible/head ratio",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot3 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()

# Deployment_capacity 
m <- lmer(Deployment_capacity ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(Deployment_capacity ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(Deployment_capacity ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F + (1|Block), mydatas,REML = F) # reorder

summary(m)
stepba(m) # N(**), CN_ratio(*) 

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)


#pdf("Figure/barplot.effects.deployment_capacity.pdf",6,8)
#tiff("FigureTiff/barplot.effects.deployment_capacity.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.deployment_capacity.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Deployment capacity",las=1, xlim=c(-1, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot4 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


#Mechanical_advantage
## showing singular fit and didn't give any
m <- lmer(Mechanical_advantage ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(Mechanical_advantage ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(Mechanical_advantage ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F + (1|Block), mydatas,REML = F) # reorder
summary(m)
stepba(m)#  ADF(*), SD(.)

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)


#pdf("Figure/barplot.effects.mechanical_advantage.pdf",6,8)
#tiff("FigureTiff/barplot.effects.mechanical_advantage.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.mechanical_advantage.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mechanical advantage",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot5 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()



# Apical_development
m <- lmer(logApical_development ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(logApical_development ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logApical_development ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F + (1|Block), mydatas,REML = F) # reorder

summary(m)
stepba(m) # No significant 


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)


#pdf("Figure/barplot.effects.apical_development.pdf",6,8)
#tiff("FigureTiff/barplot.effects.apical_development.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.apical_development.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Apical development",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot6 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


# Molar_plate_length 
m <- lmer(logMolar_plate_length ~  N+ F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
#m <- lmer(logMolar_plate_length ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logMolar_plate_length ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F+ (1|Block), mydatas,REML = F) # reorder

summary(m)
stepba(m)# N(*), SLA(*), ADF(*), CN:ratio(***)


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)

#pdf("Figure/barplot.effects.molar_plate_length.pdf",6,8)
#tiff("FigureTiff/barplot.effects.molar_plate_length.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
#tiff("Figure_reorder_tiff/barplot.effects.molar_plate_length.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Molar plate length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.03))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot7 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()

## To combine all plots
par(mfrow= c(2,4))

plot1 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot2 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot3 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot4 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot5 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot6 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot7 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


dev.off()

plot_grid(plot1,plot2,plot3,plot4,plot5,plot6,plot7,align = "h", axis = "b",
rel_widths = c(1, 1), ncol=4, nrow=2, labels = c("(a)", "(b)","(c)", "(d)","(e)", "(f)","(g)"))


### Bar graph for seven traits 94 plots only 2017 data ----
##18102022, 19102022
# logBody_length17
# logMandible_length17
# logMandible_head_ratio17
# logMechanical_advantage17
# logDeployment_capacity17
# logApical_development17
# logMolar_plate_length17


Lm1 <- lmer(Molar_plate_length17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
Lm2 <- lmer(sqrtMolar_plate_length17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
Lm3 <- lmer(logMolar_plate_length17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
anova(Lm1,Lm2,Lm3)

# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)

### Body_length 
str(mydata)

names(mydata)
## To run tiff, first run all, and second leave tiff code and third run again all and dev.off() will get plot
tiff("Figure_all/Figure_mainEffect/barplot.maineffects._mandible_traits2017_94plots.tiff", width = 39, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


m <- lmer(logBody_length17~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)

m <- lmer(logBody_length17 ~   Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+(1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m)#  Rootbiomass(.), SLA(.)

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.049&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.049] <- "(.)"


stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.049, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.049, "#055099", "#98AFC7") # (dark blue,blue gray)

#tiff("Figure_reorder_tiff/barplot.effects.Body_length_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Body length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))
xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot1 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()



### Mandible_length 
m <- lmer(logMandible_length17~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
#m <- lmer(logMandible_length ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logMandible_length17 ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m) ## SD(*) 

### for Bar graph
library(gplots)

coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.051, "#055099", "#98AFC7") # (dark blue,blue gray)


#tiff("Figure_reorder_tiff/barplot.effects.mandible_length_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mandible length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot2 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


# Mandible_head_ratio
m <- lmer(logMandible_head_ratio17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
#m <- lmer(Mandible_head_ratio ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logMandible_head_ratio17 ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m)# no significant

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.049&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""

#cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.05, "#055099", "#98AFC7") # (dark blue,blue gray)


#tiff("Figure_reorder_tiff/barplot.effects.mandible_head_ratio_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mandible/head ratio",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot3 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


# Deployment_capacity 
m <- lmer(logDeployment_capacity17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
#m <- lmer(logDeployment_capacity ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logDeployment_capacity17 ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m) # ADF(.), LDMC(**) 

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.0012] <- "**" ### can't do ** for 0.001
stars[stars<=0.01&stars>0.0012] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.05, "#055099", "#98AFC7") # (dark blue,blue gray)


#tiff("Figure_reorder_tiff/barplot.effects.deployment_capacity_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Deployment capacity",las=1, xlim=c(-1, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot4 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


#Mechanical_advantage
## showing singular fit and didn't give any
m <- lmer(logMechanical_advantage17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
#m <- lmer(logMechanical_advantage ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logMechanical_advantage17 ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m) ## no significant 

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.05, "#055099", "#98AFC7") # (dark blue,blue gray)


#tiff("Figure_reorder_tiff/barplot.effects.mechanical_advantage_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mechanical advantage",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot5 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()



# Apical_development
m <- lmer(logApical_development17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+(1|Block), mydatas2_Rmna,REML = F)
#m <- lmer(logApical_development ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(Apical_development17 ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m) # CN ratio (**) 

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.05, "#055099", "#98AFC7") # (dark blue,blue gray)


#tiff("Figure_reorder_tiff/barplot.effects.apical_development_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Apical development",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot6 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()


# Molar_plate_length 
m <- lmer(logMolar_plate_length17 ~  N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 + ADF17 + Ca17 + CN_ratio17 + Biomass17 + Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
#m <- lmer(logMolar_plate_length ~ Rootbiomass+Biomass+CN_ratio+Ca+ADF+LDMC+SLA +MPD_Pre+SD+F+N + (1|Block), mydatas,REML = F) # reorder
m <- lmer(logMolar_plate_length17 ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m)# SD(*), CN:ratio(.) 


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.049&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.049] <- "(.)"
stars[stars>0.1&stars<1] <- ""


#cols <- ifelse(sigs3<0.049, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.049, "#055099", "#98AFC7") # (dark blue,blue gray)

#tiff("Figure_reorder_tiff/barplot.effects.molar_plate_length_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=16,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Molar plate length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2,
             names.arg= c("BGB", "AGB", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.3), xvals+(ses+0.03))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plot7 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

#dev.off()

## To combine all plots

par(mfrow= c(2,4))


plot1 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot2 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot3 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot4 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot5 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot6 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


plot7 <- plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")


dev.off()


## Extra adding abundance2017(94 plots) and feeding activity and mean Main effects----

## Mean
##◘ 08082020,15082022,19082022
str(mydata)
### Abundance
m <- lmer(log(Abundance17+1) ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
m <- lmer(log(Abundance17+1) ~ Rootbiomass+Biomass17+ADF17+LDMC17+SLA17 +MPD_Pre17+SD+Ca17+CN_ratio17+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m)#  N(.)

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.049&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""

#cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.05, "#055099", "#98AFC7") # (dark blue,blue gray)


#pdf("Figure/barplot.effects.Abundance_94.pdf",6,8)
#tiff("FigureTiff/barplot.effects.Abundance_94.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Abundance",las=1, xlim=c(-0.7, 0.6),font.axis=2,cex.lab=2,
             names.arg= c("Rootbiomass", "Biomass", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.05), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()

### Feeding_rate
m <- lmer(Feeding_rate~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_Rmna,REML = F)
m <- lmer(Feeding_rate ~ Rootbiomass+Biomass+ADF+LDMC+SLA +MPD_Pre+SD+Ca+CN_ratio+N+F+ (1|Block), mydatas2_Rmna,REML = F) # reorder

summary(m)
stepba(m)#  SLA(*), Rootbiomass(*)


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[11:10] <- c("F", "N")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[11:10] <- c("F", "N")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.049&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""

#cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)
cols <- ifelse(sigs3<0.05, "#055099", "#98AFC7") # (dark blue,blue gray)


#pdf("Figure/barplot.effects.feeding_rate_94.pdf",6,8)
#tiff("FigureTiff/barplot.effects.feeding_rate_94.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Abundance",las=1, xlim=c(-0.7, 0.6),font.axis=2,cex.lab=2,
             names.arg= c("Rootbiomass", "Biomass", "ADF","LDMC","SLA", "MPD","SR","Ca","C:N", "N","F"))

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.05), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()


### Body_length 
m <- lmer(sqrtBody_length~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m)#  LDMC(.), ADF(*)


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.Body_length_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.Body_length_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Body Length",las=1, xlim=c(-0.5, 0.4),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.05), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()


### Mandible_length 
m <- lmer(logMandible_length~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m)#SLA(*), ADF(*), CN_ratio(**) 


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.mandible_length_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.mandible_length_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mandible Length",las=1, xlim=c(-0.7, 0.6),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.09), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()


# Mandible_head_ratio
m <- lmer(Mandible_head_ratio ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m)# CN_ratio(.) ## although it has 0.05, stepba gave (.)

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""

#♥ It didn't change the color with <0.05
cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.mandible_head_ratio_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.mandible_head_ratio_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#


par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mandible Head Ratio",las=1, xlim=c(-0.7, 0.6),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.06), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()


#Mechanical_advantage
## showing singular fit and didn't give any
m <- lmer(Mechanical_advantage ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m)## no significant 


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.mechanical_advantage_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.mechanical_advantage_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Mechanical Advantage",las=1, xlim=c(-0.7, 0.6),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.12), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()



# Deployment_capacity 
m <- lmer(Deployment_capacity ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m) # N(**)

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.deployment_capacity_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.deployment_capacity_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Deployment Capacity",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.15), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()

# Apical_development
m <- lmer(logApical_development ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m) # MPD(.)

### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


cols <- ifelse(sigs3<0.05, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.apical_development_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.apical_development_144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Apical Development",las=1, xlim=c(-0.7, 0.6),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.04), xvals+(ses+0.02))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()



# Molar_plate_length 
m <- lmer(logMolar_plate_length ~  N+ F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas2_144,REML = F)
summary(m)
stepba(m)# SLA(.) CN:ratio(*) 


### for Bar graph
coefs <- fixef(m)[-1]
names(coefs)[1:2] <- c("N", "F")

ses <- summary(m)$coefficients[,2][-1]
names(ses)[1:2] <- c("N", "F")

ss <- stepba(m)

sigs <- as.numeric(ss$result.table[,"Pr(Chi)"])
names(sigs) <- ss$result.table[,"factor"]
sigs2 <- sigs[complete.cases(sigs)]

sigs3 <- sigs2[match(names(coefs), names(sigs2))]

stars <- sigs3
stars[stars<=0.001] <- "***"
stars[stars<=0.01&stars>0.001] <- "**"
stars[stars<=0.05&stars>0.01] <- "*"
stars[stars<=0.1&stars>0.05] <- "(.)"
stars[stars>0.1&stars<1] <- ""


cols <- ifelse(sigs3<0.051, "#FF69B4", "#FFC0CB") # (hot pink,light pink)

#pdf("Figure/barplot.effects.molar_plate_length_144.pdf",6,8)
tiff("FigureTiff/barplot.effects.molar_plate_length144.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

par(mar=c(5,7,2,2))
y <- barplot(coefs,horiz = T, col = cols, xlab = "Molar Plate Length",las=1, xlim=c(-1.0, 1.0),font.axis=2,cex.lab=2)

xvals <- coefs

xvals <- ifelse(xvals<0, xvals-(ses+0.17), xvals+(ses+0.03))

text(xvals, y, stars, xpd=NA,cex=2,adj=0)

plotCI(coefs,y,ses,gap=0,pch="",add=T,err="x")

dev.off()




### Main effects on functional traits 2017----
#♦15082022
# Model
hist(mydata$Mandible_length17)
hist(log(mydata$Mandible_length17+1))
hist(sqrt(mydata$Mandible_length17))


## 1) Best transformation of continuous explanatory variables
# Choose the one with lower AIC
## Best transforation of y
#09082020
# For2017
# logBody_length17
# logMandible_length17
# logMandible_head_ratio17
# logMechanical_advantage17
# logDeployment_capacity17
# logApical_development17
# logMolar_plate_length17


Lm1 <- lmer(Molar_plate_length17 ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtMolar_plate_length17 ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(logMolar_plate_length17~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3)
# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)


Mymodel <- lmer(log(Mandible_length+1) ~ N*F*SD+ (1|Block) + (1|Comb), mydatas2_144, REML = F)

plot(Mymodel, N ~ resid(.), abline = 0)
plot(Mymodel, F ~ resid(.), abline = 0)

plot(Mymodel, resid(.) ~ fitted(.) | N, id = .05, adj = .3)
plot(Mymodel, resid(.) ~ fitted(.) | F, id = .05, adj = .3)

plot(Mymodel, log(Mandible_length+1) ~ fitted(.), id = .05, adj = .3)

# b - normal distribution of random effect

plot(ranef(Mymodel), grid = T)[["Block"]]
plot(ranef(Mymodel), grid = T)[["Comb"]]

##• Body_length 2017
m <- lmer(logBody_length17 ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# SLA (.), Rootbiomass(.) 


### Mandible_length 2017
## showing boundary (singular) fit: see ?isSingular 
m <- lmer(logMandible_length17 ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# SD(*) 

Final_m <- lmer(formula = logMandible_length17 ~ SD + (1 | Block) + (1 | 
             Comb), data = mydatas, REML = F)

plot(allEffects(Final_m))# 


# Mandible_head_ratio 2017
## showing boundary (singular) fit: see ?isSingular 
m <- lmer(logMandible_head_ratio17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 +ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# no significant 


#Mechanical_advantage 2017
## showing singular fit 
m <- lmer(logMechanical_advantage17 ~N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 +ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# no significant 


# Deployment_capacity 2017
## showing singular fit and didn't give any
m <- lmer(logDeployment_capacity17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 +ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m) ## LDMC(**), ADF(.) 
Final_m <- lmer(formula = logDeployment_capacity17 ~ LDMC17 + (1 | Block) + 
                  (1 | Comb), data = mydatas, REML = F)

plot(allEffects(Final_m))


# Apical_development 2017
## Showing singular fit 
m <- lmer(logApical_development17 ~ N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 +ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m) # F(.), C:N ratio(*) 
Final_m <- lmer(formula = logApical_development17 ~ F + (1 | Block) + (1 | 
            Comb), data = mydatas, REML = F)

plot(allEffects(Final_m))


# Molar_plate_length 2017
m <- lmer(logMolar_plate_length17 ~  N + F + SD+ SLA17 + LDMC17+ MPD_Pre17 +ADF17+ Ca17+ CN_ratio17+ Biomass17+ Rootbiomass+ (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# SD(*), CN_ratio(.) 
Final_m <- lmer(formula = logMolar_plate_length17 ~ CN_ratio17 + (1 | Block) + 
                  (1 | Comb), data = mydatas, REML = F)

plot(allEffects(Final_m))




