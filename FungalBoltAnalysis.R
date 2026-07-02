library(lmerTest)
library(lme4)
library(emmeans)
library(performance)
library(ggplot2)

#Data prep
bolts<-read.table(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/Fungi/Spreadsheets_for_analysis/FungalBoltAnalysis.csv", header=TRUE, sep=",")

bolts$Species<-as.factor(bolts$Species)
str(bolts)

#Model and checking assumptions
bolts_model<-lmer(Recorded~Species*Temp +(1|Diameter.cm), data = bolts)
check_model(bolts_model, check=c("linearity","homogeneity","normality","collinearity"))

#Analysis
anova(bolts_model, type=3)
summary(bolts_model)
bolts_emm<-emmeans(bolts_model, ~ Species*Temp)
pairs(bolts_emm)

#Graphing
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/FungalBoltsGraph.pdf", width=7, height=5)

ggplot(data=bolts, mapping=aes(x=Temp, y=Recorded, col=Species))+
  geom_point()+
  geom_smooth(se=FALSE)+
  labs(x="Oven Temperature (°C)",
       y="Temp. Inside Bolt (°C)")+
  theme_minimal()+
  theme(legend.text=element_text(face="italic"))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))+
  theme(axis.text=element_text(size=12))+theme(axis.title=element_text(size=13))
dev.off()



