####13102022
rm(list=ls())
library(ggplot2)
library(cowplot)
source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")#### Traits analysis
#source("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new/Script/3. Models_scripts.R")
## If source(-), didn't work, load 3. Models_scripts.R, first
source("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new/Script/3. Models_scripts.R")
setwd("~/Documents/ThuZar/TZN_data/Collembola_brand_new") ## For TZN laptop

## If source(-), didn't work, load 3. Models_scripts.R, first

### To check only 103 plots for mean (2017&2018)
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



### Collembola mean functional traits 2017 and 2018 (126 plots)----
#05072022, 25072022, 26072022, 08082022 ## 09082020, 15082022, 27092022, 03102022,17102022
str(mydata)

# For mean 2017 and 2018
# Body_length
# logMandible_length
# logMandible_head_ratio
# logMechanical_advantage 
# Deployment_capacity 
# Apical_development 
# logMolar_plate_length 

## Check whether to use log data or unlog data--
hist(mydata$Deployment_capacity)
hist(log(mydata$Deployment_capacity+1))
hist(sqrt(mydata$Deployment_capacity))


# Choose the one with lower AIC and log transformation with log data
## Best transforation of y

Lm1 <- lmer(Molar_plate_length   ~ N*F*(SD*SLA + MPD_Pre) + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtMolar_plate_length  ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F) 
Lm3 <- lmer(logMolar_plate_length   ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) ## Anova didn't work with scaled data 
# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)


MPL <- lmer(logMolar_plate_length ~ N*F*(SD*SLA + MPD_Pre) + (1|Block) + (1|Comb), mydatas, REML = F)

# b - normal distribution of random effect

plot(ranef(MPL), grid = T)[["Block"]]
plot(ranef(MPL), grid = T)[["Comb"]]

plot(fitted(MPL),residuals(MPL))
abline(lm(residuals(MPL) ~ fitted(MPL)))


# Body_length 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(Body_length ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas, REML = F) 
summary(m)
stepba(m)# N:F(*)
Final_BL <- lmer(formula = Body_length ~ N + F + (1 | Block) + (1 | Comb) + 
                   N:F, data = mydata, REML = F)
plot(allEffects(Final_BL))

#Nx F

ef <- effect("N:F ", Final_BL) 
df<- as.data.frame(ef)

levels(df$F)[levels(df$F)=="0"] <- "no fungicide"       #This is to change the 0 into "no fungicide"
levels(df$F)[levels(df$F)=="1"] <- "fungicide" 
str(df)

tiff("Figure_all/Figure_Interaction/nice_graph_BL_126_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper,x= F, group = N, colour=N))+
  geom_errorbar(aes(ymin= lower, ymax=upper, color=N),position = position_dodge(0.3),width=0.05, lwd=2)+
  geom_point(size=6, shape = 15, position = position_dodge(0.3))+
  labs(y = "Body length (µm)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        axis.title.x = element_blank(),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )
dev.off()


# Mandible_length 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMandible_length ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas, REML = F) 
summary(m)
stepba(m)# SLA(.), MPD(.)
Final_ML <- lmer(formula = logMandible_length ~ MPD_Pre +SLA + (1 | Block) + (1 | 
            Comb), data = mydata, REML = F)

plot(allEffects(Final_ML))

## SLA
#ef <- effect("SLA", Final_ML , xlevels=list(SD=c(1,4,8,20)))
ef <- effect("SLA", Final_ML , xlevels=list(MPD_Pre=seq(9.488,18.191,43.070)))

df_SLA <- as.data.frame(ef)  #make a dataframe out of it
ML_SLA <- ggplot(df_SLA, aes(y=fit, ymin=lower, ymax=upper, x=SLA))+ 
  geom_smooth(method="lm",lwd=2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0 )+
  labs(y = "Mandible length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2),legend.position = "none")


### MPD
ef <- effect("MPD_Pre", Final_ML , xlevels=list(MPD_Pre=seq(0,0.5962,0.1213)))
df_MPD <- as.data.frame(ef)  #make a dataframe out of it

ML_MPD <- ggplot(df_MPD, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre))+ 
  geom_smooth(method="lm", lwd=2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mandible length (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2),legend.position = "none")
library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_ML_126_unscale.tiff", width = 26.25, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(ML_SLA,ML_MPD, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=2, nrow=1, labels = c("(a)", "(b)"))
dev.off()


# Mandible_head_ratio 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMandible_head_ratio ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# no significant 



# Deployment_capacity
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(Deployment_capacity ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# SD:SLA(*), N:SLA(*), F:SLA(*), N:F:SD(*) 
Final_DC <-lmer(formula = Deployment_capacity ~ N + F + SD + SLA + (1 | 
            Block) + (1 | Comb) + N:F + SD:SLA + N:SD + N:SLA + F:SD + 
                  F:SLA + N:F:SD, data = mydata, REML = F)
plot(allEffects(Final_DC))

## SDx SLA
ef <- effect("SD:SLA", Final_DC , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA))+ 
  geom_smooth(method="lm", lwd=2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~SD, labeller = labeller(SD=c("1"="monoculture","4"="4 species", "8"="8 species", "20"="20 species")))+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        #legend.title=element_text(size=14,face = "bold",colour = "black"),
        legend.title = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))

### Fx SLA
ef <- effect("F:SLA", Final_DC) 
df<- as.data.frame(ef)

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )


### Nx SLA
ef <- effect("N:SLA", Final_DC) 
df<- as.data.frame(ef)

plot3 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = N, colour=N))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )


## Nx Fx SD
ef <- effect("N:F:SD", Final_DC , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
df
plot4<- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SD, group = N, colour=N))+
  facet_wrap(~df)+theme(strip.text.x=element_text(size=14, face = "bold",colour = "black"))+
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~F, labeller=labeller(F=c("0"="no fungicide","1"="with fungicide")))+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Species richness")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        #plot.title = element_text(size = 14, face = "bold",colour = "black"),
        axis.title=element_text(size=16, face = "bold",colour = "black"),
        axis.text=element_text(size=14, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        #legend.position="bottom",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black",size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen")

library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_DC_126_unscale.tiff", width = 35, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2,plot3,plot4, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=2, nrow=2, labels = c("(a)", "(b)","(c)", "(d)"))
dev.off()


# Mechanical_advantage 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMechanical_advantage ~  N*F*(SD*SLA + MPD_Pre)  + (1|Block)+ (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# N:MPD(**), F:SLA(*)
Final_MA <- lmer(formula = logMechanical_advantage ~ N + F + SLA + MPD_Pre + 
                   (1 | Block) + (1 | Comb) + N:MPD_Pre + F:SLA, data = mydata, 
                 REML = F)

plot(allEffects(Final_MA))

summary(Final_MA)
plot(allEffects(Final_MA)) 

## N:MPD
ef <- effect("N:MPD_Pre ", Final_MA) 
df<- as.data.frame(ef) 
ef

plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre, color = N))+ 
  geom_smooth(method="lm" ,lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mechanical advantage (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks= Nbreaks, labels= Nlabels, name= "Nitrogen")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )


## Fx SLA
summary(Final_MA)
ef <- effect("F:SLA", Final_MA) 
df<- as.data.frame(ef)

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mechanical advantage (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_MA_126_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=1, nrow=2, labels = c("(a)", "(b)"))
dev.off()


# Apical_development 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(Apical_development ~  N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# no significant


# Molar_plate_length
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMolar_plate_length ~ N*F*(SD*SLA + MPD_Pre)+ (1|Block)+ (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# SD:SLA(**),  N:SLA(*),F:SLA(**), N:F:SD(*)
Final_MPL <-lmer(formula = logMolar_plate_length ~ N + F + SD + SLA + (1 | 
            Block) + (1 | Comb) + N:F + SD:SLA + N:SD + N:SLA + F:SD + 
                   F:SLA + N:F:SD, data = mydata, REML = F)
plot(allEffects(Final_MPL))

## SDx SLA
ef <- effect("SD:SLA", Final_MPL , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA))+ 
  geom_smooth(method="lm",lwd=2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~SD, labeller = labeller(SD=c("1"="monoculture","4"="4 species", "8"="8 species", "20"="20 species")))+
  labs(y = "Molar plate length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))

### Fx SLA
ef <- effect("F:SLA", Final_MPL) 
df<- as.data.frame(ef)

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Molar plate length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )


### Nx SLA
ef <- effect("N:SLA", Final_MPL) 
df<- as.data.frame(ef)

plot3 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group= N,colour=N))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Molar plate length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name="Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )


## Nx Fx SD
ef <- effect("N:F:SD", Final_MPL , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
df

plot4<- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SD, group = N, colour=N))+
  facet_wrap(~df)+theme(strip.text.x=element_text(size=14, face = "bold",colour = "black"))+
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~F, labeller=labeller(F=c("0"="no fungicide","1"="with fungicide")))+
  labs(y = "Molar plate length (µm)")+
  labs (x="Species richness")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        #plot.title = element_text(size = 14, face = "bold",colour = "black"),
        axis.title=element_text(size=16, face = "bold",colour = "black"),
        axis.text=element_text(size=14, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        #legend.position="bottom",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black",size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen")

library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_MPL_126_unscale.tiff", width = 35, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2,plot3,plot4, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=2, nrow=2, labels = c("(a)", "(b)","(c)", "(d)"))
dev.off()

### Collembola functional traits 2017 (94 plots), remove 4way----
### Use mydatas2_Rmna for 94 plots (2017 only) and 103 plots (mean)

#11102022, 13102022,17102022
str(mydata)
For2017 (94 plots)
# sqrtBody_length17
# logMandible_length17
# logMandible_head_ratio17 
# logMechanical_advantage17
# logDeployment_capacity17 
# sqrtApical_development17 
# logMolar_plate_length17

# Choose the one with lower AIC and log transformation with log data
## Best transforation of y

Lm1 <- lmer(Molar_plate_length17    ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
Lm2 <- lmer(sqrtMolar_plate_length17  ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
Lm3 <- lmer(logMolar_plate_length17   ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
anova(Lm1,Lm2,Lm3) ## Anova didn't work with scaled data 
# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)

#### Remove 4ways 2017 (94 plots)
## Do 3-way for all traits because MPL gave 4-way
# Body_length17
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(sqrtBody_length17 ~ N*F*(SD*SLA17 + MPD_Pre17) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (94)

## Remove 4-ways
m <- lmer(sqrtBody_length17 ~ N*F*(SD*SLA17 + MPD_Pre17) 
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# no significant 


# Mandible_length ### no significant after removing 4-3ways
## Showing boundary (singular) fit: see ?isSingular 
m <- lmer(logMandible_length17 ~ N*F*(SD*SLA17 + MPD_Pre17) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F) ## observation (87)

## Remove 4-ways
m <- lmer(logMandible_length17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# N:MPD(*), F:SLA(*), N:F:SD(**)

Final_ML <-lmer(formula = logMandible_length17 ~ N + F + SD + SLA17 + MPD_Pre17 + 
                  (1 | Block) + (1 | Comb) + N:F + N:SD + N:MPD_Pre17 + F:SD + 
                  F:SLA17 + N:F:SD, data = mydata2_Rmna, REML = F)
plot(allEffects(Final_ML))



## N:MPD
ef <- effect("N:MPD_Pre17 ", Final_ML) 
df<- as.data.frame(ef) 

plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, group = N, colour=N))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mandible_length (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks= Nbreaks, labels= Nlabels, name= "Nitrogen")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )


## Fx SLA
summary(Final_MA)
ef <- effect("F:SLA17", Final_ML) 
df<- as.data.frame(ef)

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mandible_length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )



## Nx Fx SD
ef <- effect("N:F:SD", Final_ML , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
df
plot3 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SD, group = N, colour=N))+
  facet_wrap(~df)+theme(strip.text.x=element_text(size=14, face = "bold",colour = "black"))+
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~F, labeller=labeller(F=c("0"="no fungicide","1"="with fungicide")))+
  labs(y = "Mandible_length (µm)")+
  labs (x="Species richness")+
  theme(plot.margin = unit(c(1,1,3,0.25), "cm"),
        #plot.title = element_text(size = 14, face = "bold",colour = "black"),
        axis.title=element_text(size=16, face = "bold",colour = "black"),
        axis.text=element_text(size=14, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        #legend.position="bottom",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black",size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen")

library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_ML_94_unscale.tiff", width = 35, height = 25, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2,plot3, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=2, nrow=2, labels = c("(a)", "(b)","(c)"))
dev.off()



# Mandible_head_ratio
m <- lmer(logMandible_head_ratio17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

## Remove 4-ways
m <- lmer(logMandible_head_ratio17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)## no significant



# Deployment_capacity # no significant 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logDeployment_capacity17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

## Remove 4-ways
m <- lmer(logDeployment_capacity17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# no significant 



#Mechanical_advantage # N:MPD(*)
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMechanical_advantage17 ~ N*F*(SD*SLA17 + MPD_Pre17) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (85)

## Remove 4-ways
m <- lmer(logMechanical_advantage17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# N:MPD(*)
Final_MA <-lmer(formula = logMechanical_advantage17 ~ N + MPD_Pre17 + (1 | 
               Block) + (1 | Comb) + N:MPD_Pre17, data = mydata2_Rmna, 
                REML = F)
plot(allEffects(Final_MA))

## N:MPD
ef <- effect("N:MPD_Pre17 ", Final_MA) 
df<- as.data.frame(ef) 
tiff("Figure_all/Figure_Interaction/nice_graph_MA_94_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#


ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, group = N, colour=N))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mechanical advantage (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks= Nbreaks, labels= Nlabels, name= "Nitrogen")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )

dev.off()


# Apical_development # F:SLA (*)
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(sqrtApical_development17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

## Remove 4-ways
m <- lmer(sqrtApical_development17 ~ N*F*(SD*SLA17 + MPD_Pre17) 
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# F:SLA (*) 
Final_AD <-lmer(formula = sqrtApical_development17 ~ F + SLA17 + (1 | Block) + 
                  (1 | Comb) + F:SLA17, data = mydata2_Rmna, REML = F)


plot(allEffects(Final_AD))

### Fx SLA
ef <- effect("F:SLA17", Final_AD) 
df<- as.data.frame(ef)
tiff("Figure_all/Figure_Interaction/nice_graph_AD_94_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Apical development (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

dev.off()

# Molar_plate_length ## It gives 4ways
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMolar_plate_length17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

## Remove 4-ways # F:SLA (*)
m <- lmer(logMolar_plate_length17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:F:SD:SLA17  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)

Final_MPL <- lmer(formula = logMolar_plate_length17 ~ F + SLA17 + (1 | Block) + 
                    (1 | Comb) + F:SLA17, data = mydata2_Rmna, REML = F)


plot(allEffects(Final_MPL))

### Fx SLA
ef <- effect("F:SLA17", Final_MPL) 
df<- as.data.frame(ef)
tiff("Figure_all/Figure_Interaction/nice_graph_MPL_94_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Molar plate length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

dev.off()


### Remove 4ways and 3ways (2017 94 plots) ----
# Body_length17
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(sqrtBody_length17 ~ N*F*(SD*SLA17 + MPD_Pre17) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (94)

### Remove 4 way and 3way
m <- lmer(sqrtBody_length17 ~ N*F*(SD*SLA17 + MPD_Pre17) 
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# no significant 


# Mandible_length ### F:SLA(*), N:F:MPD(*) after removing 4-3ways
## Showing boundary (singular) fit: see ?isSingular 
m <- lmer(logMandible_length17 ~ N*F*(SD*SLA17 + MPD_Pre17) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F) ## observation (87)

### Remove 4 way and 3way
m <- lmer(logMandible_length17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# ### # F:MPD(*)

Final_ML <- lmer(formula = logMandible_length17 ~ F + MPD_Pre17 + (1 | Block) + 
                   (1 | Comb) + F:MPD_Pre17, data = mydatas2_Rmna, REML = F)
plot(allEffects(Final_ML))


## F:MPD
ef <- effect("F:MPD_Pre17 ", Final_ML) 
df<- as.data.frame(ef) 
tiff("Figure_all/Figure_Interaction/nice_graph_ML_94(two-way)_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mandible_length (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks= Fbreaks, labels= Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

dev.off()



# Mandible_head_ratio
m <- lmer(logMandible_head_ratio17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

### Remove 4 way and 3way
m <- lmer(logMandible_head_ratio17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)## no significant





# Deployment_capacity # no significant 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logDeployment_capacity17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

### Remove 4 way and 3way
m <- lmer(logDeployment_capacity17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# no significant 


#Mechanical_advantage # N:MPD(*)
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMechanical_advantage17 ~ N*F*(SD*SLA17 + MPD_Pre17) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (85)

### Remove 4 way and 3way
m <- lmer(logMechanical_advantage17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# N:MPD(*)
Final_MA<-lmer(formula = logMechanical_advantage17 ~ N + MPD_Pre17 + (1 | 
                                                                        Block) + (1 | Comb) + N:MPD_Pre17, data = mydata2_Rmna, 
               REML = F)
plot(allEffects(Final_MA))

## N:MPD
ef <- effect("N:MPD_Pre17 ", Final_MA) 
df<- as.data.frame(ef) 
tiff("Figure_all/Figure_Interaction/nice_graph_MA_94(two-way)_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#


ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, group = N, colour=N))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mechanical advantage (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks= Nbreaks, labels= Nlabels, name= "Nitrogen")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )

dev.off()

# Apical_development # F:SLA (*)
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(sqrtApical_development17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

### Remove 4 way and 3way
m <- lmer(sqrtApical_development17 ~ N*F*(SD*SLA17 + MPD_Pre17) 
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# F:SLA (*) 
Final_AD <-lmer(formula = sqrtApical_development17 ~ F + SLA17 + (1 | Block) + 
                 (1 | Comb) + F:SLA17, data = mydata2_Rmna, REML = F)


plot(allEffects(Final_AD))

### Fx SLA
ef <- effect("F:SLA17", Final_AD) 
df<- as.data.frame(ef)
tiff("Figure_all/Figure_Interaction/nice_graph_AD_94(two-way)_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Apical development (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

dev.off()

# Molar_plate_length ## It gives 4ways
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMolar_plate_length17 ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)## observation (87)

### Remove 4 way and 3way 
m <- lmer(logMolar_plate_length17 ~ N*F*(SD*SLA17 + MPD_Pre17)
          -N:SD:SLA17 -F:SD:SLA17 -N:F:SD -N:F:SLA17 -N:F:MPD_Pre17 -N:F:SD:SLA17+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# F:SLA (*), F:MPD (*)


Final_MPL <- lmer(formula = logMolar_plate_length17 ~ F + SLA17 + MPD_Pre17 + 
                  (1 | Block) + (1 | Comb) + F:SLA17 + F:MPD_Pre17, data = mydata2_Rmna, 
                REML = F)
plot(allEffects(Final_MPL))

### Fx SLA
ef <- effect("F:SLA17", Final_MPL) 
df<- as.data.frame(ef)

plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Molar plate length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

## F:MPD
ef <- effect("F:MPD_Pre17 ", Final_MPL) 
df<- as.data.frame(ef) 

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Molar plate length (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks= Fbreaks, labels= Flabels, name= "Fungicide ")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )


tiff("Figure_all/Figure_Interaction/nice_graph_MPL_94(two-way)_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=1, nrow=2, labels = c("(a)", "(b)"))
dev.off()


### Collembola mean functional traits 2017 and 2018 (103 plots) to check the data----
#05072022, 25072022, 26072022, 08082022 ## 09082020, 15082022, 27092022, 03102022
str(mydata)

# For mean 2017 and 2018
# Body_length
# logMandible_length
# logMandible_head_ratio
# logMechanical_advantage 
# Deployment_capacity 
# Apical_development 
# logMolar_plate_length 

## Check whether to use log data or unlog data--
hist(mydata$Deployment_capacity)
hist(log(mydata$Deployment_capacity+1))
hist(sqrt(mydata$Deployment_capacity))


# Choose the one with lower AIC and log transformation with log data
## Best transforation of y

Lm1 <- lmer(Molar_plate_length   ~ N*F*(SD*SLA + MPD_Pre) + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
Lm2 <- lmer(sqrtMolar_plate_length  ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F) 
Lm3 <- lmer(logMolar_plate_length   ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
anova(Lm1,Lm2,Lm3) ## Anova didn't work with scaled data 
# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)

# Body_length 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(Body_length ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F) 

## Remove 4-ways
m <- lmer(Body_length ~ N*F*(SD*SLA + MPD_Pre)
          -N:F:SD:SLA  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m)# N:F(*)
Final_BL <- lmer(formula = Body_length ~ N + F + (1 | Block) + (1 | Comb) + 
                   N:F, data = mydata2_Rmna, REML = F)
plot(allEffects(Final_BL))

#Nx F

ef <- effect("N:F ", Final_BL) 
df<- as.data.frame(ef)

#levels(df$N)[levels(df$N)=="0"] <- "no nitrogen"       #This is to change the 0 into "no fungicide"
#levels(df$N)[levels(df$N)=="1"] <- "nitrogen"      
levels(df$F)[levels(df$F)=="0"] <- "no fungicide"       #This is to change the 0 into "no fungicide"
levels(df$F)[levels(df$F)=="1"] <- "fungicide" 
str(df)

tiff("Figure_all/Figure_Interaction/nice_graph_BL_103_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper,x= F, group = N, colour=N))+
  geom_errorbar(aes(ymin= lower, ymax=upper, color=N),position = position_dodge(0.3),width=0.05,lwd=2)+
  geom_point(size=6, shape = 15, position = position_dodge(0.3))+
  labs(y = "Body length (µm)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        axis.title.x = element_blank(),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )
dev.off()


# Mandible_length 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMandible_length ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F) 
m <- lmer(logMandible_length ~ N*F*(SD*SLA + MPD_Pre)
          -N:F:SD:SLA + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F) 

summary(m)
stepba(m)# F:SLA(*), N:F:MPD(*)
Final_ML <- lmer(formula = logMandible_length ~ N + F + SLA + MPD_Pre + (1 | 
                  Block) + (1 | Comb) + N:F + N:MPD_Pre + F:SLA + F:MPD_Pre + 
                   N:F:MPD_Pre, data = mydata2_Rmna, REML = F)
plot(allEffects(Final_ML))

### Fx SLA
ef <- effect("F:SLA", Final_ML) 
df<- as.data.frame(ef)

plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm",lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mandible length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

## NxFxMPD

### Nx Fx MPD
#make nice graphs -
ef <- effect("N:F:MPD_Pre", Final_ML)  #get the estimates and lower and upper confidence interals
df <- as.data.frame(ef)  #make a dataframe out of it
plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre, group = N, colour=N))+
  facet_wrap(~df)+theme(strip.text.x=element_text(size=14, face = "bold",colour = "black"))+ 
  geom_smooth(method="lm", lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~F, labeller=labeller(F=c("0"="no fungicide","1"="with fungicide")))+
  labs(y = "Mandible length (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks= Nbreaks, labels= Nlabels, name= "Nitrogen")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen" )

tiff("Figure_all/Figure_Interaction/nice_graph_ML_103_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=1, nrow=2, labels = c("(a)", "(b)"))
dev.off()

# Mandible_head_ratio 
m <- lmer(logMandible_head_ratio ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
m <- lmer(logMandible_head_ratio ~ N*F*(SD*SLA + MPD_Pre) 
          -N:F:SD:SLA  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# no significant 



# Deployment_capacity
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(Deployment_capacity ~ N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
m <- lmer(Deployment_capacity ~ N*F*(SD*SLA + MPD_Pre)
          -N:F:SD:SLA  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# SD:SLA(*), N:SLA(*), F:SLA(*)
Final_DC <-lmer(formula = Deployment_capacity ~ N + F + SD + SLA + (1 | 
            Block) + (1 | Comb) + SD:SLA + N:SLA + F:SLA, data = mydata2_Rmna, 
                REML = F)

plot(allEffects(Final_DC))

## SDx SLA
ef <- effect("SD:SLA", Final_DC , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA))+ 
  geom_smooth(method="lm", lwd= 2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~SD, labeller = labeller(SD=c("1"="monoculture","4"="4 species", "8"="8 species", "20"="20 species")))+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))

### Fx SLA
ef <- effect("F:SLA", Final_DC) 
df<- as.data.frame(ef)

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm", lwd= 2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )


### Nx SLA
ef <- effect("N:SLA", Final_DC) 
df<- as.data.frame(ef)

plot3 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = N, colour=N))+ 
  geom_smooth(method="lm", lwd= 2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Deployment capacity (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks=Nbreaks, labels=Nlabels, name= "Nitrogen addition")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )


library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_DC_103_unscale.tiff", width = 35, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2,plot3, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=2, nrow=2, labels = c("(a)", "(b)","(c)"))
dev.off()

# Mechanical_advantage 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMechanical_advantage ~  N*F*(SD*SLA + MPD_Pre)  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)#N:MPD(**), N:F:SD:SLA (it gave four-way)
m <- lmer(logMechanical_advantage ~  N*F*(SD*SLA + MPD_Pre)
          -N:F:SD:SLA  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# N:MPD(*), F:SLA (*)
Final_MA <- lmer(formula = logMechanical_advantage ~ N + F + SLA + MPD_Pre + 
                   (1 | Block) + (1 | Comb) + N:MPD_Pre + F:SLA, data = mydata2_Rmna, 
                 REML = F)


plot(allEffects(Final_MA))

summary(Final_MA)
plot(allEffects(Final_MA)) 

## N:MPD
ef <- effect("N:MPD_Pre ", Final_MA) 
df<- as.data.frame(ef) 

MA_NMPD <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre, group = N, colour=N))+ 
  geom_smooth(method="lm", lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mechanical advantage (µm)")+
  labs (x="Functional diversity (MPD in SLA)")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Nvalues, breaks= Nbreaks, labels= Nlabels, name= "Nitrogen")   +          
  scale_fill_manual(values = Nvalues, breaks = Nbreaks, labels = Nlabels, name= "Nitrogen"  )


## Fx SLA
summary(Final_MA)
ef <- effect("F:SLA", Final_MA) 
df<- as.data.frame(ef)

MA_FSLA <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm", lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Mechanical advantage (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_MA_103_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(MA_NMPD,MA_FSLA, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=1, nrow=2, labels = c("(a)", "(b)"))
dev.off()


# Apical_development 
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(Apical_development ~  N*F*(SD*SLA + MPD_Pre) + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
m <- lmer(Apical_development ~  N*F*(SD*SLA + MPD_Pre)
          -N:F:SD:SLA  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# no significant 


# Molar_plate_length
## Showing boundary (singular) fit: see ?isSingular
m <- lmer(logMolar_plate_length ~ N*F*(SD*SLA + MPD_Pre)+ (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)
m <- lmer(logMolar_plate_length ~ N*F*(SD*SLA + MPD_Pre)
          -N:F:SD:SLA  + (1|Block)+ (1|Comb), mydatas2_Rmna, REML = F)

summary(m)
stepba(m)# F:SLA(*) 
Final_MPL <-lmer(formula = logMolar_plate_length ~ F + SLA + (1 | Block) + 
                   (1 | Comb) + F:SLA, data = mydata2_Rmna, REML = F)
plot(allEffects(Final_MPL))

### Fx SLA
ef <- effect("F:SLA", Final_MPL) 
df<- as.data.frame(ef)
tiff("Figure_all/Figure_Interaction/nice_graph_MPL_103_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = F, colour=F))+ 
  geom_smooth(method="lm", lwd= 2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Molar plate length (µm)")+
  labs (x="Community weighted mean SLA")+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        legend.text =element_text(size=14, face = "bold",colour = "black"),
        legend.title=element_text(size=14,face = "bold",colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background = element_blank(),
        axis.ticks= element_line(size = 1.2),
        axis.ticks.length=unit(.25, "cm"),
        panel.border = element_rect(color = "black",fill = NA, size = 1.2),
        axis.line = element_line(colour = "black", size = 1.2))+
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )

dev.off()