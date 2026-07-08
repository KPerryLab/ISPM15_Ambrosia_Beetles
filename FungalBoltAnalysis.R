library(lmerTest)
library(lme4)
library(emmeans)
library(performance)
library(ggplot2)


#Data prep
bolts<-read.table(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/Fungi/Spreadsheets_for_analysis/FungalBoltAnalysis_RawData.csv", header=TRUE, sep=",")

bolts$Species<-as.factor(bolts$Species)
str(bolts)
boltsmodel<-lmer(Recorded ~ Diameter.cm + Temp*Species + (1+Temp*Species|Number), data=bolts)

#Model and checking assumptions
boltsmodel<-lmer(Recorded ~ Diameter.cm + Temp*Species + (1+Temp*Species|Number), data=bolts)
check_model(boltsmodel, check=c("linearity","homogeneity","normality","collinearity"))

#Analysis
anova(boltsmodel, type=3)
summary(boltsmodel)
bolts_emm<-emmeans(boltsmodel, ~Diameter.cm+Temp*Species)
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



