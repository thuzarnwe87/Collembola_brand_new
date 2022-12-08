

rm(list=ls())
source("N:/TZN_data/Collembola_brand_new/Script/3. Models_scripts.R")

### Feeding activity----
# Choose the one with lower AIC and log transformation with log data
## Best transforation of y
#12082020
#Feeding_rate
#logFeeding_rate18
#Feeding_rate22

Lm1 <- lmer(Feeding_rate  ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtFeeding_rate ~ N*F*(SD*SLA18 + MPD_Pre18) + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(logFeeding_rate  ~ N*F*(SD*SLA18 + MPD_Pre18) + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) ## 

Lm1 <- lmer(Feeding_rate18  ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtFeeding_rate18 ~ N*F*(SD*SLA18 + MPD_Pre18) + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Feeding_rate18+1)  ~ N*F*(SD*SLA18 + MPD_Pre18) + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) ###  models were not all fitted to the same size of dataset


Lm1 <- lmer(Feeding_rate22  ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtFeeding_rate22 ~ N*F*(SD*SLA18 + MPD_Pre18) + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(log(Feeding_rate22+1)  ~ N*F*(SD*SLA18 + MPD_Pre18) + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3)


# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)



## Mean Feeding rate (2018 and 2022)---
#05072022
str(mydata)
m <- lmer(Feeding_rate ~ N*F*(SD*SLA + MPD_Pre)  + (1|Block) + (1|Comb), mydatas, REML = F)
summary(m)
stepba(m)# F:MPD(**), N:SD:SLA (**)


FD <- lmer(formula = Feeding_rate ~ N + F + SD + SLA + MPD_Pre + (1 | 
          Block) + (1 | Comb) + SD:SLA + N:SD + N:SLA + F:MPD_Pre + 
             N:SD:SLA, data = mydata, REML = F)


summary(FD)

plot(allEffects(FD))
library(ggplot2)
library(cowplot)


#### Nx SDx SLA
ef <- effect("N:SD:SLA", FD , #get the estimates and lower and upper confidence interals
            xlevels=list(SD=c(1,4,8,20))) #tell r to give you values for certain levels of species diversity (for 1,4,8,20 species)
df <- as.data.frame(ef)  #make a dataframe out of it
df

FD_A <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=SLA, group = N, colour=N))+ 
  geom_smooth(method="lm", lwd= 2)+
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill= Ni, linetype=NA), alpha=0.3) +
  geom_ribbon(alpha=0.2, linetype=0)+
  facet_grid(.~SD, labeller = labeller(SD=c("1"="monoculture","4"="4 species", "8"="8 species", "20"="20 species")))+
  labs(y = "Feeding rate")+
  labs (x=expression("Community weighted mean SLA"))+
  theme(plot.margin = unit(c(1,1,1,0.25), "cm"),
        plot.title = element_text(size = 18,face = "bold",colour = "black",hjust =0.5),
        axis.text = element_text(size = 14, face = "bold",colour = "black"),
        axis.title = element_text(size = 16, face = "bold",colour = "black"),
        #axis.title.x = element_text(size = 16, face = "bold",colour = "black"),
        #axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
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

## Fx MPD

ef <- effect("F:MPD_Pre ", FD) 
df<- as.data.frame(ef)
df
FD_B <- ggplot(df, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre, group = F, colour=F))+ 
  geom_smooth(method="lm", lwd= 2)+
  #geom_ribbon(aes(ymin=lower, ymax=upper, fill= Ni, linetype=NA), alpha=0.3) +
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Feeding rate")+
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
  scale_colour_manual(values=Fvalues, breaks=Fbreaks, labels=Flabels, name= "Fungicide addition")   +          
  scale_fill_manual(values = Fvalues, breaks = Fbreaks, labels = Flabels, name= "Fungicide"  )


library(cowplot)
tiff("Figure_all/Figure_Interaction/nice_graph_Feeding_rate_fullmodel_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#
plot_grid(FD_A,FD_B, align = "h", axis = "b", 
          rel_widths = c(1, 1), ncol=1, nrow=2, labels = c("(a)", "(b)"))
dev.off()

# Feeding rate 2018
m <- lmer(sqrtFeeding_rate18 ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)## There is no data (SLA22, MPD22), yet.
summary(m)
stepba(m)# MPD (*)
Final_m <- lmer(formula = sqrtFeeding_rate18 ~ MPD_Pre18 + (1 | Block) + 
                  (1 | Comb), data = mydatas, REML = F)
plot(allEffects(Final_m))

### MPD
ef <- effect("MPD_Pre18", Final_m , xlevels=list(MPD_Pre18=seq(0,0.6667,0.1550)))
df_MPD <- as.data.frame(ef)  #make a dataframe out of it

tiff("Figure_all/Figure_Interaction/nice_graph_Feeding_rate2018_fullmodel_unscale.tiff", width = 17.5, height = 22, units = "cm", res = 600 ,pointsize=12,bg="transparent")#

ggplot(df_MPD, aes(y=fit, ymin=lower, ymax=upper, x=MPD_Pre18))+ 
  geom_smooth(method="lm", lwd=2, colour="darkgrey")+
  geom_ribbon(alpha=0.2, linetype=0)+
  labs(y = "Feeding rate")+
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
dev.off()

#2022
m <- lmer(sqrtFeeding_rate22 ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)## There is no data (SLA22, MPD22), yet.
m <- lmer(sqrtFeeding_rate22 ~ N*F*(SD*SLA18 + MPD_Pre18)  + (1|Block) + (1|Comb), mydatas, REML = F)## There is no data (SLA22, MPD22), yet.

summary(m)
stepba(m)# N:SD:SLA(***), changed as strong significant after changing zero as NA and model and using SLA18, MPD18
Final_m <- lmer(formula = sqrtFeeding_rate22 ~ N + SD + SLA18 + (1 | Block) + 
                  (1 | Comb) + SD:SLA18 + N:SD + N:SLA18 + N:SD:SLA18, data = mydatas, 
                REML = F)
plot(allEffects(Final_m))


### ### All main effects on feeding activity----
#05072022, 15082022
#Feeding_rate
#Feeding_rate18
#Feeding_rate22

Lm1 <- lmer(Feeding_rate  ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm2 <- lmer(sqrtFeeding_rate ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass  + (1|Block) + (1|Comb), mydatas, REML = F)
Lm3 <- lmer(logFeeding_rate  ~ N + F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass + (1|Block) + (1|Comb), mydatas, REML = F)
anova(Lm1,Lm2,Lm3) ## 


# a - independance of errors and within group errors
# grid.newpage()
grid.arrange(plot(Lm1, main = "no trans"),
             plot(Lm2, main = "sqrt trans"),
             plot(Lm3, main = "log trans"),
             ncol = 3)

str(mydata)
# Mean 2018 and 2022
m <- lmer(Feeding_rate ~ N+ F + SD+ SLA + LDMC+ MPD_Pre+ ADF+ Ca+ CN_ratio+ Biomass+ Rootbiomass+ (1|Block), mydatas,REML = F)
summary(m)
stepba(m)# Biomass (**) , Rootbiomass(.)
Final_m <- lmer(formula = Feeding_rate ~ Biomass + (1 | Block), data = mydatas, 
                REML = F)
plot(allEffects(Final_m))


## 2018
m <- lmer(Feeding_rate18 ~ N+ F + SD+ SLA18 + LDMC18+ MPD_Pre18+ ADF18+ Ca18+ CN_ratio18+ Biomass18+ Rootbiomass+ (1|Block), mydatas,REML = F) # Can't use 2017 data
summary(m)
stepba(m)# MPD(**), ADF(.)

Final_m <-  lmer(formula = Feeding_rate18 ~ MPD_Pre18 + (1 | Block), data = mydatas, 
                 REML = F)
plot(allEffects(Final_m))

#2022
m <- lmer(Feeding_rate22 ~ N+ F + SD+ SLA18 + LDMC18+ MPD_Pre18+ ADF18+ Ca18+ CN_ratio18+ Biomass18+ Rootbiomass+ (1|Block), mydatas,REML = F)
summary(m)
stepba(m)# ADF(*), Biomass(***)
Final_m <- lmer(formula = Feeding_rate22 ~ ADF18 + Biomass18 + (1 | Block), 
                data = mydatas, REML = F)




