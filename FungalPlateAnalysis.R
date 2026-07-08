library(tidyverse)
library(ggplot2)
library(lmerTest)
library(lme4)
library(emmeans)
library(performance)
library(see)

#Data prep
fungi.temp<-read.table(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/Fungi/Spreadsheets_for_analysis/FungalAnalysis.csv", header=TRUE, sep=",")
fungi.temp$Species<-as.factor(fungi.temp$Species)
fungi.temp$Area<-as.factor(fungi.temp$Area)
str(fungi.temp)

#Graphing
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/FungalPlateGraph.pdf", width=7, height=5)

ggplot(fungi.temp, aes(x=Species, y=Temp.ave, fill=Area))+ 
  geom_boxplot()+
  geom_point(pch=21, size=1.5,position=position_jitterdodge(jitter.width=0.5, dodge.width=0.75), alpha=1)+
  labs(x="Fungal Species",
       y="Average Temperature (°C)",
       fill="Area of Plate")+
  theme_minimal()+
  theme(axis.text.x=element_text(face="italic"))+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))
dev.off()

#Check model fit
fit_model <- lmer(Temp.ave ~ Area*Species + (1|Replicate), data=fungi.temp)
check_model(fit_model, check=c("linearity","homogeneity","normality","collinearity"))

#Analysis
anova(fit_model, type=3)
summary(fit_model)

anova_emm<-emmeans(fit_model, ~ Area*Species)
pairs(anova_emm)
