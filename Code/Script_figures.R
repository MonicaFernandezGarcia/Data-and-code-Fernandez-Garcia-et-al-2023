
###The script below was written in  Rstudio, so it is  recommended to be run within RStudio.

rm(list=ls())#clear all

#Install libraries if need it:

  #install.packages(c("openxlsx","ggplot2","gridExtra","dplyr","tidyverse","rstatix","ggpubr","zoo"))

#Load libraries

  library(openxlsx)
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  library(tidyverse)
  library(rstatix)
  library(ggpubr)
  library(zoo)


#Chose your own directory: 
  
setwd("")

#FIGURE 1 ----------------------------------------------------------------------------

#NGRIP PLOT

NGRIP <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="NGRIP")

SI <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Stadial")

ngrip<-ggplot(NGRIP, aes(x=Age, y=O18)) +
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  geom_line(color="blue") +
  xlab("Age (cal BP)")+
  ylab(expression(paste(delta^18, 'O'* ' (\u2030)')))+
  theme_classic()


## Culture durantions plot

Cultures <- read.xlsx("Figure1.xlsx", rowNames=FALSE, colNames=TRUE, sheet="Culture")

cultures<-ggplot(Cultures, aes()) + 
  geom_rect(data = SI, aes(xmin = E , xmax = B, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("blue")) +
  geom_segment(aes(x=From, xend=To, y=Culture, yend=Culture), size=2) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  theme_classic() +
  xlab("Duration")


## MD04-2845 plot

MD042845 <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="MD04-2845")

MD04_2845<-ggplot(MD042845, aes(x=Date, y=Pollen, group=Type, colour=Type)) +
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  geom_area(fill="#336633", linetype = 0) +
  xlab("Age (cal BP)")+
  ylab("Arboreal Pollen (%)")+
  theme_classic()+
  theme(legend.position = "none")


## Other marine cores plot

OtherMarineCores <- read.xlsx("Figure1.xlsx", rowNames=FALSE, colNames=TRUE, sheet="Othermarine")

OtherMarineCores_Plot<-ggplot(OtherMarineCores, aes(x=Date, y=Pollen, group=Core, colour=Core)) +
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
   inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  geom_line() +
  scale_colour_manual(values=c("MD99-2331"="#000066","MD95-2039"="#CC0033","MD95-2042"= "#339999"))+
  xlab("Age (cal BP)")+
  ylab("Arboreal Pollen (%)")+
  theme_classic()+
  theme(legend.position = "none")


## Enol plot

        ### Temperature data from Enol was group in bins of 100 years:
        #binTemperaure<- as.data.frame(rollmean(Enol$Temperature, k=100))
        #binAge <- as.data.frame(rollmean(Enol$Age, k= 100))
        #write.xlsx(binTemperaure, "BinTempEnol.xlsx")
        #write.xlsx(binAge, "binAge.xlsx")
        ## These 100 yr bins are in the Figure1.xlsx file (binAge = Age.bins, binTemperature=Temperature.bins)

Enol <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Enol")

Enol_plot<-ggplot(Enol, aes(x=Age.bins, y=Temperature.bins)) +
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  geom_line() +
  xlab("Age (cal BP)")+
  ylab("Temperature (ºC)")+
  theme_classic()


## Bay of Biscay plot

Biscay <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Biscay")

Biscay_plot<-ggplot(Biscay, aes(x=Age, y=O18, group=Species, color=Species)) +
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  geom_line() +
  xlab("Age (cal BP)")+
  ylab(expression(paste(delta^18, 'O'* ' (\u2030)')))+
  theme_classic()+
  theme(legend.position="none")


Plot<-grid.arrange(cultures, ngrip, MD04_2845, OtherMarineCores_Plot, Enol_plot, Biscay_plot, ncol=1)

ggsave(file="Figure1-complete.svg", plot=Plot, width = 10, height = 15)


#FIGURE 3------------------------------------------------------------------------------
#FIGURE 3A - HABITAT WEIGHTING SMALL-MAMMALS PLOT--------------------------------------

Microlandscape <- read.xlsx("Figure3.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Micro")

Microlandscape$Shortname <- factor(Microlandscape$Shortname,levels=Microlandscape[order(Microlandscape$CRONORANGE),]$Shortname)

Woodland_micro <-ggplot(data=Microlandscape, aes(x=Shortname, y=WOODLAND, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Woodland (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


Openhumid_micro <-ggplot(data=Microlandscape, aes(x=Shortname, y=OPEN.HUMID, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Open humid (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


Opendry_micro <-ggplot(data=Microlandscape, aes(x=Shortname, y=OPEN.DRY, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Open dry (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


Rocky_micro <-ggplot(data=Microlandscape, aes(x=Shortname, y=ROCKY, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Rocky (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


Water_micro <-ggplot(data=Microlandscape, aes(x=Shortname, y=WATER, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Water (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


plot_f3_micro <- grid.arrange (Woodland_micro, Openhumid_micro, Opendry_micro, Rocky_micro, Water_micro, ncol=5)

ggsave(file="Figure3A.svg", plot=plot_f3_micro, width = 25, height = 6.5)


#FIGURE 3B - ARBOREAL POLLEN -----------------------------------------------------------

ArborealPollen <- read.xlsx("Figure3.xlsx", rowNames=FALSE,colNames=TRUE, sheet="APollen")

ArborealPollen$Shortname <- factor(ArborealPollen$Shortname, levels=ArborealPollen[order(ArborealPollen$CRONORANGE), ]$Shortname)

APollen <-ggplot(data=ArborealPollen, aes(x=Shortname, y=AP, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("AP (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


ggsave(file="Figure3B.svg", plot=APollen, width = 4.5, height = 6.5)

#FIGURE 3C - POLLEN - ECOLOGICAL CATEGORIES------------------------------------------

Eco_Cat <- read.xlsx("Figure3.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Pollencat")

Eco_Cat$Shortname <- factor(Eco_Cat$Shortname, levels=Eco_Cat[order(Eco_Cat$CRONORANGE),]$Shortname)


Conifers <-ggplot(data=Eco_Cat, aes(x=Shortname, y=Conifers, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Conifers (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


Mesophytes.Trees <-ggplot(data=Eco_Cat, aes(x=Shortname, y=Mesophytes.Trees, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Mesophytes Trees (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


Mediterranean <-ggplot(data=Eco_Cat, aes(x=Shortname, y=Mediterranean.forestschrub, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Mediterranean (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


Cold.grasses <-ggplot(data=Eco_Cat, aes(x=Shortname, y=Cold.grasses, fill= CULTURE)) +
  geom_area(aes(group=CULTURE, alpha=0.2))+
  geom_point(size=1.5)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("Cold grasses (%)")+
  theme(axis.title=element_text(size=8))+
  scale_y_continuous(limits=c(0,100))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


plot_f3_pollen <- grid.arrange (Conifers, Mesophytes.Trees, Mediterranean, Cold.grasses, ncol=4)

ggsave(file="Figure3C.svg", plot=plot_f3_pollen, width = 18, height = 6.5)



#FIGURE 4------------------------------------------------------------------------------
#FIGURE4A - MICRO CLIMATIC ESTIMATIONS (BM) -------------------------------------------

Dataset_Micro <- read.xlsx("Figure4.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Micro")
head(Dataset_Micro)

Dataset_Micro$Shortname <- factor(Dataset_Micro$Shortname, levels=Dataset_Micro[order(Dataset_Micro$CRONORANGE), ]$Shortname)

MAT_A <-ggplot(data=Dataset_Micro, aes(x=Shortname, y=RMAT, fill= CULTURE)) +
  geom_bar(stat = "identity",alpha = 0.6)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("MAT anomaly (ºC)")+
  theme(axis.title=element_text(size=8))+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


Dataset_Micro$Shortname <- factor(Dataset_Micro$Shortname, levels=Dataset_Micro[order(Dataset_Micro$CRONORANGE), ]$Shortname)

MAP_A <-ggplot(data=Dataset_Micro, aes(x=Shortname, y=RMAP, fill= CULTURE)) +
  geom_bar(stat = "identity", alpha = 0.6)+
  geom_hline(yintercept = 0, size=0.75)+
  ylab("MAP anomaly (mm)")+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(breaks=seq( -20, 5, 5))+
    theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))


plot_fig4_micro <- grid.arrange (MAT_A, MAP_A, ncol=2)

ggsave(file="Figure4A.svg", plot=plot_fig4_micro, width = 12, height = 7)


# FIGURE4B - POLLEN WA CLIMATIC ESTIMATIONS -------------------------------------------


Dataset_Pollen <- read.xlsx("Figure4.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Pollen")

Dataset_Pollen$Shortname <- factor(Dataset_Pollen$Shortname, levels=Dataset_Pollen[order(Dataset_Pollen$CRONORANGE), ]$Shortname)

MAT_A_pollen <-ggplot(data=Dataset_Pollen, aes(x=Shortname, y=RMAT, fill= CULTURE)) +
  geom_bar(stat = "identity", alpha = 0.6)+
  geom_hline(yintercept = 0, size=0.75)+
  xlab("Site and level") +
  ylab("MAT anomaly (ºC)")+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


Dataset_Pollen$Shortname <- factor(Dataset_Pollen$Shortname, levels=Dataset_Pollen[order(Dataset_Pollen$CRONORANGE), ]$Shortname)

MAP_A_pollen <-ggplot(data=Dataset_Pollen, aes(x=Shortname, y=RMAP, fill= CULTURE)) +
  geom_bar(stat = "identity", alpha = 0.6)+
  geom_hline(yintercept = 0, size=0.75)+
  xlab("Site and level") +
  ylab("MAP anomaly (mm)")+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=8),
        axis.line.y = element_blank())+
  scale_fill_manual(values=c("#3399CC","#3300FF","#CC9933","#993333","#99CC00" ))


plot_fig4_pollen <- grid.arrange ( MAT_A_pollen, MAP_A_pollen, ncol=2)

ggsave(file="Figure4B.svg", plot=plot_fig4_pollen, width = 10, height = 4.5)


#FIGURE 5-----------------------------------------------------------------------------
#FIGURE 5A - BOXPLOTS MICRO CLIMATE---------------------------------------------------

Figure_5A <- read.xlsx("Figure5.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Micro")

Figure_5A_MAT <- ggplot(Figure_5A, aes(x = factor(CULTURE), y = RMAT, fill = CULTURE)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.5, outlier.shape = NA) +
  scale_x_discrete(limits=c("MO (>45 ka)", "MO (45-42 ka)", "CH (40-38 ka)", "AU (35-30 ka)", "GRAV (31-25 ka)"))+
  geom_jitter(width = 0.1, height = 0, color = "black")+
  xlab("") + 
  ylab("Mean annual temperature anomaly (ºC)") + 
  labs(fill = "") + 
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.y=element_blank())


Figure_5A_MAP <- ggplot(Figure_5A, aes(x = factor(CULTURE), y = RMAP, fill = CULTURE)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.5, outlier.shape = NA) +
  scale_x_discrete(limits=c("MO (>45 ka)", "MO (45-42 ka)", "CH (40-38 ka)", "AU (35-30 ka)", "GRAV (31-25 ka)"))+
  geom_jitter(width = 0.1, height = 0, color = "black")+
  xlab("") + 
  ylab("Mean annual precipitation anomaly (mm)") + 
  labs(fill = "") + 
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF"))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.y=element_blank())


plot_fig5A <- grid.arrange(Figure_5A_MAT, Figure_5A_MAP, ncol=2)

ggsave(file="Figure5A.svg", plot=plot_fig5A, width = 10, height = 5)



# FIGURE 5B (POLLEN)-------------------------------------------------------------------

Figure_5B <- read.xlsx("Figure5.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Pollen")


Figure_5B_MAT <- ggplot(Figure_5B, aes(x = factor(CULTURE), y = RMAT, fill = CULTURE)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.5, outlier.shape = NA) +
  scale_x_discrete(limits=c("MO (45-42 ka)", "AU (35-30 ka)", "GRAV (31-25 ka)"))+
  geom_jitter(width = 0.1, height = 0, color = "black")+
  xlab("") + 
  ylab("Mean annual temperature anomaly (ºC)") + 
  labs(fill = "") + 
  scale_fill_manual(values=c("#993333", "#99CC00", "#3399CC"))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.y=element_blank())


Figure_5B_MAP <- ggplot(Figure_5B, aes(x = factor(CULTURE), y = RMAP, fill = CULTURE)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.5) + 
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.5, outlier.shape = NA) +
  scale_x_discrete(limits=c("MO (45-42 ka)", "AU (35-30 ka)", "GRAV (31-25 ka)"))+
  geom_jitter(width = 0.1, height = 0, color = "black")+
  xlab("") + 
  ylab("Mean annual precipitation anomaly (mm)") + 
  labs(fill = "") + 
  scale_fill_manual(values=c("#993333", "#99CC00", "#3399CC"))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.y=element_blank())


plot_fig5B <- grid.arrange(Figure_5B_MAT, Figure_5B_MAP, ncol=2)

ggsave(file="Figure5B.svg", plot=plot_fig5B, width = 8, height = 5)


# FIGURE 5C (ISOTOPES-------------------------------------------------------------------


Figure_5C <- read.xlsx("Figure5.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Isotopes")


Figure_5C_d13C <- ggplot(Figure_5C, aes(x = factor(CULTURE), y = d13C, fill = CULTURE)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.3) + 
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, color = "black")+
  scale_x_discrete(limits=c("MO (>45 ka)", "MO (45-42 ka)", "CH (40-38 ka)", "AU (35-30 ka)", "GRAV (31-25 ka)", "Sterile"))+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF", "white"))+
  facet_wrap( ~ Figure_5C$SPECIES) +
  xlab("") + 
  ylab(expression(paste(delta^{13}, "C (V-PDB \u2030)")))+
  labs(fill = "") + 
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.y=element_blank())


Figure_5C_d15N <- ggplot(Figure_5C, aes(x = factor(CULTURE), y = d15N, fill = CULTURE)) + 
  stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.9), width = 0.3) + 
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, color = "black")+
  scale_x_discrete(limits=c("MO (>45 ka)", "MO (45-42 ka)", "CH (40-38 ka)", "AU (35-30 ka)", "GRAV (31-25 ka)", "Sterile"))+
  scale_fill_manual(values=c("#993333", "#CC9933", "#99CC00", "#3399CC", "#3300FF", "white"))+
  xlab("") + 
  ylab(expression(paste(delta^{15},  "N (AIR \u2030)")))+
  facet_wrap( ~ Figure_5C$SPECIES) +
  labs(fill = "") + 
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.y=element_blank())


plot_fig5C <- grid.arrange(Figure_5C_d13C, Figure_5C_d15N, ncol=2)

ggsave(file="Figure5C.svg", plot=plot_fig5C, width = 17, height = 5)



#FIGURE 6--------------------------------------------------------------------------

Figure_6_ALL <- read.xlsx("Figure6.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Micro+Pollen")

NGRIP <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="NGRIP")

SI <- read.xlsx("Figure1.xlsx", rowNames=FALSE,colNames=TRUE, sheet="Stadial")

ngrip<-ggplot(NGRIP, aes(x=Age, y=O18)) +
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  scale_x_reverse( breaks = seq(25000, 55000, 5000)) +
  geom_line(color="blue") +
  xlab("Age (cal BP)")+
  ylab(expression(paste(delta^18, 'O'* ' (\u2030)')))+
  theme_classic()


Figure_6_MicroPollen_MAT<- ggplot(Figure_6_ALL, aes(x=Age, y=RMAT, group=CULTURE, color=CULTURE, label=NumberSite)) + 
  geom_point()+
  geom_errorbar(aes(ymin=`R-MAT-lwr`, ymax=`R-MAT-upr`), width=.2,
                position=position_dodge(0.05))+
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  ylab("MAT anomaly (ºC)")+
  xlab("Age (cal BP)")+
  scale_x_reverse(breaks = seq(25000, 55000, 5000)) +
  scale_y_continuous(breaks=seq( -20, 5, 5))+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values=c( "#CC9933", "#99CC00","#993333", "#3300FF", "#3399CC"))+
  geom_label()


Figure_6_MicroPollen_MAP<- ggplot(Figure_6_ALL, aes(x=Age, y=RMAP, group=CULTURE, color=CULTURE, label=NumberSite)) + 
  geom_point()+
  geom_errorbar(aes(ymin=`R-MAP-lwr`, ymax=`R-MAP-upr`), width=.2,
                position=position_dodge(0.05))+
  geom_rect(data = SI, aes(xmin = B , xmax = E, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("grey")) +
  ylab("MAP anomaly (mm)")+
  xlab("Age (cal BP)")+
  scale_x_reverse(breaks = seq(25000, 55000, 5000)) +
  scale_y_continuous(breaks=seq( -1500, 250, 250))+
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual(values=c( "#CC9933", "#99CC00","#993333", "#3300FF", "#3399CC"))+
  geom_label()


plot_fig6 <- grid.arrange(ngrip, Figure_6_MicroPollen_MAT, Figure_6_MicroPollen_MAP)

ggsave(file="Figure6.svg", plot=plot_fig6, width = 10, height = 8)



