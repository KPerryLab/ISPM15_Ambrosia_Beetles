
#ULT crass
library(tidyverse)
library(car)
library(dplyr)
library(MASS)
library(lmtest)

#Uploading/subsetting
logreg_crass<-read.csv(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/Upper Lethal Limit Trials/Spreadsheets_for_analysis/GLMSurvival_RawData.csv", header=TRUE, sep=",")
str(logreg_crass)
summary(logreg_crass)
logreg_crass$Species<-as.factor(logreg_crass$Species)
logreg_crass$Host_species<-as.factor(logreg_crass$Host_species)
logreg_crass$Lifestage<-as.factor(logreg_crass$Lifestage)
str(logreg_crass)


#fit model
#Overall fit
logreg_crass_subset<-logreg_crass %>% 
  filter(Species =="X. crassiusculus")
fit_crass <- glm(Survival_24hr ~ Temp + Host_species + Lifestage,
                 data=logreg_crass_subset,family=binomial(link=logit))
summary(fit_crass)

#Models for individual graphs
#Adults 
logreg_crass_xaxis_subset<-logreg_crass_subset %>% 
  filter(Temp <"59")
logreg_crass_adult_subset<-logreg_crass_xaxis_subset %>% 
  filter(Lifestage=="Adult")
fit_crass_adult <- glm(Survival_24hr ~ Temp + Host_species,
                 data=logreg_crass_adult_subset,family=binomial(link=logit))
summary(fit_crass_adult)

#Larvae
logreg_crass_larvae_subset<-logreg_crass_xaxis_subset %>% 
  filter(Lifestage=="Larvae")
fit_crass_larvae <- glm(Survival_24hr ~ Temp + Host_species,
                       data=logreg_crass_larvae_subset,family=binomial(link=logit))
summary(fit_crass_larvae)

#Make Pretty + Save plot as clear PDF
#Adult
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/Graph_Crass_Adult.pdf", width=6.5, height=4.25)

ggplot(fit_crass_adult, aes(x=Temp, y=Survival_24hr, color=Host_species))+
  geom_jitter(height=0)+
  xlab("Temperature (C)")+
  ylab("Survival (Dead/Alive)")+
  labs(color="Host Species")+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  scale_color_manual(values=c("Beech"="red2","Red Maple"="royalblue2"))+
  geom_hline(data=data.frame(c(0.50)), aes(yintercept=c(0.50)), color="darkgray", linetype="dashed")+theme_classic()+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))

dev.off()

#Larvae
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/Graph_Crass_Larvae.pdf", width=6.5, height=4.25)

ggplot(fit_crass_larvae, aes(x=Temp, y=Survival_24hr, color=Host_species))+
  geom_jitter(height=0)+
  xlab("Temperature (C)")+
  ylab("Survival (Dead/Alive)")+
  labs(color="Host Species")+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  scale_color_manual(values=c("Beech"="red2","Red Maple"="royalblue2"))+
  geom_hline(data=data.frame(c(0.50)), aes(yintercept=c(0.50)), color="darkgray", linetype="dashed")+theme_classic()+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))

dev.off()

#Determine LD50, LD99
#Adult crass on beech
logreg_crass_adult_beech_subset<-logreg_crass_adult_subset %>% 
  filter(Host_species =="Beech")

counts_crass_adult_beech<-logreg_crass_adult_beech_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
    )

model_crass_adult_beech<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_crass_adult_beech)
ld50_crass_adult_beech <- dose.p(model_crass_adult_beech, p = 0.5)
ld50_crass_adult_beech
ld99_crass_adult_beech <- dose.p(model_crass_adult_beech, p = 0.99)
ld99_crass_adult_beech

#Adult crass on maple
logreg_crass_adult_maple_subset<-logreg_crass_adult_subset %>% 
  filter(Host_species =="Red Maple")

counts_crass_adult_maple<-logreg_crass_adult_maple_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_crass_adult_maple<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_crass_adult_maple)
ld50_crass_adult_maple <- dose.p(model_crass_adult_maple, p = 0.5)
ld50_crass_adult_maple
ld99_crass_adult_maple <- dose.p(model_crass_adult_maple, p = 0.99)
ld99_crass_adult_maple

#Larval crass on beech
logreg_crass_larvae_beech_subset<-logreg_crass_larvae_subset %>% 
  filter(Host_species =="Beech")

counts_crass_larvae_beech<-logreg_crass_larvae_beech_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_crass_larvae_beech<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_crass_larvae_beech)
ld50_crass_larvae_beech <- dose.p(model_crass_larvae_beech, p = 0.5)
ld50_crass_larvae_beech
ld99_crass_larvae_beech <- dose.p(model_crass_larvae_beech, p = 0.99)
ld99_crass_larvae_beech

#Larval crass on maple
logreg_crass_larvae_maple_subset<-logreg_crass_larvae_subset %>% 
  filter(Host_species =="Red Maple")

counts_crass_larvae_maple<-logreg_crass_larvae_maple_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_crass_larvae_maple<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_crass_larvae_maple)
ld50_crass_larvae_maple <- dose.p(model_crass_larvae_maple, p = 0.5)
ld50_crass_larvae_maple
ld99_crass_larvae_maple <- dose.p(model_crass_larvae_maple, p = 0.99)
ld99_crass_larvae_maple

###############################################################################
#ULT germanus

#Uploading/subsetting
logreg_germanus<-read.csv(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/Upper Lethal Limit Trials/Spreadsheets_for_analysis/GLMSurvival_RawData.csv", header=TRUE, sep=",")
str(logreg_germanus)
summary(logreg_germanus)
logreg_germanus$Species<-as.factor(logreg_germanus$Species)
logreg_germanus$Host_species<-as.factor(logreg_germanus$Host_species)
logreg_germanus$Lifestage<-as.factor(logreg_germanus$Lifestage)
str(logreg_germanus)


#fit model
#Overall fit
logreg_germanus_subset<-logreg_germanus %>% 
  filter(Species =="X. germanus")
fit_germanus <- glm(Survival_24hr ~ Temp + Host_species + Lifestage,
                 data=logreg_germanus_subset,family=binomial(link=logit))
summary(fit_germanus)

#Models for individual graphs
#Adults 
logreg_germanus_xaxis_subset<-logreg_germanus_subset %>% 
  filter(Temp <"59")
logreg_germanus_adult_subset<-logreg_germanus_xaxis_subset %>% 
  filter(Lifestage=="Adult")
fit_germanus_adult <- glm(Survival_24hr ~ Temp + Host_species,
                       data=logreg_germanus_adult_subset,family=binomial(link=logit))
summary(fit_germanus_adult)

#Larvae
logreg_germanus_larvae_subset<-logreg_germanus_xaxis_subset %>% 
  filter(Lifestage=="Larvae")
fit_germanus_larvae <- glm(Survival_24hr ~ Temp + Host_species,
                        data=logreg_germanus_larvae_subset,family=binomial(link=logit))
summary(fit_germanus_larvae)

#Make Pretty + Save plot as clear PDF
#Adult
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/Graph_Germanus_Adult.pdf", width=6.5, height=4.25)

ggplot(fit_germanus_adult, aes(x=Temp, y=Survival_24hr, color=Host_species))+
  geom_jitter(height=0)+
  xlab("Temperature (C)")+
  ylab("Survival (Dead/Alive)")+
  labs(color="Host Species")+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  scale_color_manual(values=c("Beech"="red2","Red Maple"="royalblue2"))+
  geom_hline(data=data.frame(c(0.50)), aes(yintercept=c(0.50)), color="darkgray", linetype="dashed")+theme_classic()+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))

dev.off()

#Larvae
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/Graph_Germanus_Larvae.pdf", width=6.5, height=4.25)

ggplot(fit_germanus_larvae, aes(x=Temp, y=Survival_24hr, color=Host_species))+
  geom_jitter(height=0)+
  xlab("Temperature (C)")+
  ylab("Survival (Dead/Alive)")+
  labs(color="Host Species")+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  scale_color_manual(values=c("Beech"="red2","Red Maple"="royalblue2"))+
  geom_hline(data=data.frame(c(0.50)), aes(yintercept=c(0.50)), color="darkgray", linetype="dashed")+theme_classic()+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))

dev.off()

#Determine LD50, LD99
#Adult germanus on beech
logreg_germanus_adult_beech_subset<-logreg_germanus_adult_subset %>% 
  filter(Host_species =="Beech")

counts_germanus_adult_beech<-logreg_germanus_adult_beech_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_germanus_adult_beech<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_germanus_adult_beech)
ld50_germanus_adult_beech <- dose.p(model_germanus_adult_beech, p = 0.5)
ld50_germanus_adult_beech
ld99_germanus_adult_beech <- dose.p(model_germanus_adult_beech, p = 0.99)
ld99_germanus_adult_beech

#Adult germanus on maple
logreg_germanus_adult_maple_subset<-logreg_germanus_adult_subset %>% 
  filter(Host_species =="Red Maple")

counts_germanus_adult_maple<-logreg_germanus_adult_maple_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_germanus_adult_maple<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_germanus_adult_maple)
ld50_germanus_adult_maple <- dose.p(model_germanus_adult_maple, p = 0.5)
ld50_germanus_adult_maple
ld99_germanus_adult_maple <- dose.p(model_germanus_adult_maple, p = 0.99)
ld99_germanus_adult_maple

#Larval germanus on beech
logreg_germanus_larvae_beech_subset<-logreg_germanus_larvae_subset %>% 
  filter(Host_species =="Beech")

counts_germanus_larvae_beech<-logreg_germanus_larvae_beech_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_germanus_larvae_beech<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_germanus_larvae_beech)
ld50_germanus_larvae_beech <- dose.p(model_germanus_larvae_beech, p = 0.5)
ld50_germanus_larvae_beech
ld99_germanus_larvae_beech <- dose.p(model_germanus_larvae_beech, p = 0.99)
ld99_germanus_larvae_beech

#Larval germanus on maple
logreg_germanus_larvae_maple_subset<-logreg_germanus_larvae_subset %>% 
  filter(Host_species =="Red Maple")

counts_germanus_larvae_maple<-logreg_germanus_larvae_maple_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_germanus_larvae_maple<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_germanus_larvae_maple)
ld50_germanus_larvae_maple <- dose.p(model_germanus_larvae_maple, p = 0.5)
ld50_germanus_larvae_maple
ld99_germanus_larvae_maple <- dose.p(model_germanus_larvae_maple, p = 0.99)
ld99_germanus_larvae_maple

################################################################################

#ULT maiche

#Uploading/subsetting
logreg_maiche<-read.csv(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/Upper Lethal Limit Trials/Spreadsheets_for_analysis/GLMSurvival_RawData.csv", header=TRUE, sep=",")
str(logreg_maiche)
summary(logreg_maiche)
logreg_maiche$Species<-as.factor(logreg_maiche$Species)
logreg_maiche$Host_species<-as.factor(logreg_maiche$Host_species)
logreg_maiche$Lifestage<-as.factor(logreg_maiche$Lifestage)
str(logreg_maiche)


#fit model
#Overall fit
logreg_maiche_subset<-logreg_maiche %>% 
  filter(Species =="A. maiche")
fit_maiche <- glm(Survival_24hr ~ Temp + Host_species + Lifestage,
                 data=logreg_maiche_subset,family=binomial(link=logit))
summary(fit_maiche)

#Models for individual graphs
#Adults 
logreg_maiche_xaxis_subset<-logreg_maiche_subset %>% 
  filter(Temp <"59")
logreg_maiche_adult_subset<-logreg_maiche_xaxis_subset %>% 
  filter(Lifestage=="Adult")
fit_maiche_adult <- glm(Survival_24hr ~ Temp + Host_species,
                       data=logreg_maiche_adult_subset,family=binomial(link=logit))
summary(fit_maiche_adult)

#Larvae
logreg_maiche_larvae_subset<-logreg_maiche_xaxis_subset %>% 
  filter(Lifestage=="Larvae")
fit_maiche_larvae <- glm(Survival_24hr ~ Temp + Host_species,
                        data=logreg_maiche_larvae_subset,family=binomial(link=logit))
summary(fit_maiche_larvae)

#Make Pretty + Save plot as clear PDF
#Adult
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/Graph_maiche_Adult.pdf", width=6.5, height=4.25)

ggplot(fit_maiche_adult, aes(x=Temp, y=Survival_24hr, color=Host_species))+
  geom_jitter(height=0)+
  xlab("Temperature (C)")+
  ylab("Survival (Dead/Alive)")+
  labs(color="Host Species")+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  scale_color_manual(values=c("Beech"="red2","Red Maple"="royalblue2"))+
  geom_hline(data=data.frame(c(0.50)), aes(yintercept=c(0.50)), color="darkgray", linetype="dashed")+theme_classic()+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))

dev.off()

#Larvae
pdf(file="C:/Users/jessi/OneDrive - The Ohio State University/Ambrosia 2024-2025/ISPM15_Ambrosia_Beetles/Graph_maiche_Larvae.pdf", width=6.5, height=4.25)

ggplot(fit_maiche_larvae, aes(x=Temp, y=Survival_24hr, color=Host_species))+
  geom_jitter(height=0)+
  xlab("Temperature (C)")+
  ylab("Survival (Dead/Alive)")+
  labs(color="Host Species")+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  scale_color_manual(values=c("Beech"="red2","Red Maple"="royalblue2"))+
  geom_hline(data=data.frame(c(0.50)), aes(yintercept=c(0.50)), color="darkgray", linetype="dashed")+theme_classic()+
  theme(axis.text=element_text(size=15))+theme(axis.title=element_text(size=14))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=13))

dev.off()

#Determine LD50, LD99
#Adult maiche on beech
logreg_maiche_adult_beech_subset<-logreg_maiche_adult_subset %>% 
  filter(Host_species =="Beech")

counts_maiche_adult_beech<-logreg_maiche_adult_beech_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_maiche_adult_beech<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_maiche_adult_beech)
ld50_maiche_adult_beech <- dose.p(model_maiche_adult_beech, p = 0.5)
ld50_maiche_adult_beech
ld99_maiche_adult_beech <- dose.p(model_maiche_adult_beech, p = 0.99)
ld99_maiche_adult_beech

#Adult maiche on maple
logreg_maiche_adult_maple_subset<-logreg_maiche_adult_subset %>% 
  filter(Host_species =="Red Maple")

counts_maiche_adult_maple<-logreg_maiche_adult_maple_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_maiche_adult_maple<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_maiche_adult_maple)
ld50_maiche_adult_maple <- dose.p(model_maiche_adult_maple, p = 0.5)
ld50_maiche_adult_maple
ld99_maiche_adult_maple <- dose.p(model_maiche_adult_maple, p = 0.99)
ld99_maiche_adult_maple

#Larval maiche on beech
logreg_maiche_larvae_beech_subset<-logreg_maiche_larvae_subset %>% 
  filter(Host_species =="Beech")

counts_maiche_larvae_beech<-logreg_maiche_larvae_beech_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_maiche_larvae_beech<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_maiche_larvae_beech)
ld50_maiche_larvae_beech <- dose.p(model_maiche_larvae_beech, p = 0.5)
ld50_maiche_larvae_beech
ld99_maiche_larvae_beech <- dose.p(model_maiche_larvae_beech, p = 0.99)
ld99_maiche_larvae_beech

#Larval maiche on maple
logreg_maiche_larvae_maple_subset<-logreg_maiche_larvae_subset %>% 
  filter(Host_species =="Red Maple")

counts_maiche_larvae_maple<-logreg_maiche_larvae_maple_subset %>%
  group_by(Temp) %>%
  summarize(
    Dead=sum(Survival_24hr=="0"),
    Alive=sum(Survival_24hr=="1")
  )

model_maiche_larvae_maple<- glm(cbind(Dead, Alive) ~ Temp, family = binomial(link = "logit"), data = counts_maiche_larvae_maple)
ld50_maiche_larvae_maple <- dose.p(model_maiche_larvae_maple, p = 0.5)
ld50_maiche_larvae_maple
ld99_maiche_larvae_maple <- dose.p(model_maiche_larvae_maple, p = 0.99)
ld99_maiche_larvae_maple

