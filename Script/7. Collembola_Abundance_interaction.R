
rm(list=ls())
source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")
#source("O:/Biodiversity/for ThuZar (temporary)/Collembola/Collembola_brand_new/Script/3. Models_scripts.R")
library(ggplot2)
library(cowplot)


# Choose the one with lower AIC and log transformation with log data
## Best transforation of y
## 12082020, 23082022, 18102022
#log(Abundance+1)
#log(Abundance17+1)
#log(Abundance18+1)

Lm1 <- lmer(Abundance   ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtAbundance ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Abundance+1)  ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) 

Lm1 <- lmer(Abundance17   ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtAbundance17 ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Abundance17+1)  ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) 


Lm1 <- lmer(Abundance18   ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtAbundance18 ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Abundance18+1)  ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) 

# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)


## Mean Abundance----
#05072022, 06072022, 09082020, 23082022,18102022
str(mydata)
## Mean Abundance 2017 and 2018 
m <- lmer(log(Abundance+1) ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m) #N:MPD(*)
MAbund <- lmer(formula = log(Abundance + 1) ~ N + MPD_Pre + (1 | Block) + 
                 (1 | Comb) + N:MPD_Pre, data = mydata, REML = F)

summary(MAbund)
plot(allEffects(MAbund))

plot(allEffects(MAbund))
library(ggplot2)
library(cowplot)

## N:MPD
ef <- effect("N:MPD_Pre ", MAbund) 
df<- as.data.frame(ef) 
tiff("Figure_all/Figure_Interaction/nice_graph_Abundance_NxMPD_fullmodel_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
df
ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre, color = N))+ 
  geom_smooth(method="lm" ,lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Abundance of collembola")+
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

plot(allEffects(Final_m))

## Abundance 2017----
m <- lmer(log(Abundance17+1) ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# N:MPD(*)

Final_m <- lmer(formula = log(Abundance17 + 1) ~ N + MPD_Pre17 + (1 | Block) + 
                  (1 | Comb) + N:MPD_Pre17, data = mydata, REML = F)


plot(allEffects(Final_m))

## N:MPD
ef <- effect("N:MPD_Pre17 ", Final_m) 
df<- as.data.frame(ef) 
tiff("Figure_all/Figure_Interaction/nice_graph_Abundance17_NxMPD_fullmodel_unscale.tiff", width = 17.5, height = 11, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, color = N))+ 
  geom_smooth(method="lm" ,lwd=2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Abundance of collembola")+
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


# Abundance 2018----

# Showing singular fit
m <- lmer(log(Abundance18+1) ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# F (*)
Final_m <- lmer(formula = log(Abundance18 + 1) ~ F + (1 | Block) + (1 | 
             Comb), data = mydata, REML = F)

plot(allEffects(Final_m))

##F


coef(summary(Final_m))        #here you can see all estimates
ef <- effect("F", Final_m)	#select only the one for Fungicide
ef    #These are the estimates you want to plot
x <- as.data.frame(ef)  #now they are nicely in a dataframe
x

levels(x$F)[levels(x$F)=="0"] <- "no fungicide"       #This is to change the 0 into "no fungicide"
levels(x$F)[levels(x$F)=="1"] <- "fungicide"          
str(x)

tiff("Figure_all/Figure_Interaction/nice_graph_Abundance18_F_fullmodel_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

#There are many tutorials in the internet on how to use ggplot
ggplot(x, aes(y=fit, ymax=upper, ymin=lower, x=F, colour=F))+
  scale_colour_manual(values=c("#FF6666","#990000"))+
  geom_errorbar(aes(ymin= lower, ymax=upper, color=F),position = position_dodge(0.3),width=0.05, lwd=2)+
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


## 94 plots (2017) to test----
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

### 94 plots 
m <- lmer(log(Abundance17+1) ~ N*F*(SD*SLA17 + MPD_Pre17)  + (1|Block) + (1|Comb), mydatas2_Rmna, REML = F)
summary(m)
stepba(m) # SD:SLA(*), N:MPD(**)
Abund_94plots <- lmer(formula = log(Abundance17 + 1) ~ N + SD + SLA17 + MPD_Pre17 + 
                        (1 | Block) + (1 | Comb) + SD:SLA17 + N:MPD_Pre17, data = mydata2_Rmna, 
                      REML = F)

summary(Abund_94plots)
plot(allEffects(Abund_94plots)) 

## SDx SLA
ef <- effect("SD:SLA17", Abund_94plots , #get the estimates and lower and upper confidence interals
             xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
plot1 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA17))+ 
  geom_smooth(method="lm", lwd=2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~SD, labeller = labeller(SD=c("1"="monoculture","4"="4 species", "8"="8 species", "20"="20 species")))+
  labs(y = "Abundance of collembola")+
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


### NxMPD
ef <- effect("N:MPD_Pre17 ", Abund_94plots) 
df<- as.data.frame(ef) 

plot2 <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre17, group = N, colour=N))+ 
  geom_smooth(method="lm", lwd= 2)+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Abundance of collembola")+
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

#pdf("Figure/barplot.effects.Abundance_144.pdf",6,8)
tiff("Figure_all/Figure_Interaction/nice_graph_Abundance17_94plots_fullmodel_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(plot1,plot2, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=1, nrow=2, labels = c("(a)", "(b)"))
dev.off()



## All Main effects on Abundance ----
#15082022, 18102022

#log(Abundance+1)
#log(Abundance17+1)
#log(Abundance18+1)

Lm1 <- lmer(Abundance   ~ N + F + SD+ SLA + LDMC + MPD_Pre+ADF +Ca+CN_ratio+ Biomass + Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtAbundance ~ N + F + SD+ SLA + LDMC + MPD_Pre+ADF +Ca+CN_ratio+ Biomass + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Abundance+1)  ~ N + F + SD+ SLA + LDMC + MPD_Pre+ADF +Ca+CN_ratio+ Biomass + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3)

Lm1 <- lmer(Abundance17   ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtAbundance17 ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Abundance17+1)  ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) 

Lm1 <- lmer(Abundance18   ~ N + F + SD+ SLA18 + LDMC18 + MPD_Pre18+ADF18 +Ca18+CN_ratio18+ Biomass18 + Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtAbundance18 ~ N + F + SD+ SLA18 + LDMC18  + MPD_Pre18+ADF18 +Ca18+CN_ratio18+ Biomass18 + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Abundance18+1)  ~ N + F + SD+ SLA18 + LDMC18  + MPD_Pre18+ADF18 +Ca18+CN_ratio18+ Biomass18 + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) 

# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)

## Mean Abundance 2017 and 2018
## 18102022
m <- lmer(log(Abundance+1)  ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m) # N(*)
m <- lmer(formula = log(Abundance + 1) ~ N + (1 | Block) + (1 | Comb), 
          data = mydatas, REML = F)

##Can't make a plot



## Abundance 2017
m <- lmer(log(Abundance17+1)  ~ N + F + SD+ SLA17 + LDMC17 + MPD_Pre17+ADF17 +Ca17+CN_ratio17+ Biomass17 + Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m) # N(*), ADF(.)
Finalm <- lmer(formula = log(Abundance17 + 1) ~ N + (1 | Block) + (1 | 
             Comb), data = mydatas, REML = F)
## Can't make a plot

##Abundance 2018
m <- lmer(log(Abundance18+1)   ~ N + F + SD+ SLA18 + LDMC18  + MPD_Pre18+ADF18 +Ca18+CN_ratio18+ Biomass18 + Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m) ##LDMC(*), F(.)
Finalm <- lmer(formula = log(Abundance18 + 1) ~ LDMC18 + (1 | Block) + 
                 (1 | Comb), data = mydatas, REML = F)

