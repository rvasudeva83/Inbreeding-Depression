################################################################
################# INBREEDING DEPRESSION ########################


rm(list=ls())
setwd
SL1.dat <- read.csv ("Balanced_Inbreeding_Sperm length by family.csv", fileEncoding = "Latin1", check.names = F)
SL1b.dat <- read.csv ("Balanced_Sperm Length Variance_Dec2023.csv", fileEncoding = "Latin1", check.names = F)

library (ggplot2)
library(Rmisc)     #To use summarySE function
library(tidyverse)
library(ggpubr)

str(SL1.dat)
SL1.dat$Treatment<-as.factor(SL1.dat$Treatment)
SL1.dat$Fam.ID<-as.factor(SL1.dat$Fam.ID)

str(SL1b.dat)
SL1b.dat$Gen<-as.factor(SL1b.dat$Gen)
SL1b.dat$Family.ID<-as.factor(SL1b.dat$Family.ID)

SL2 <- summarySE(SL1.dat, measurevar = "Sperm_length", groupvars = c("Treatment"))
SL2b <- summarySE(SL1b.dat, measurevar = "Sperm_length", groupvars = c("Gen", "Family.ID"))
SL2c <- summarySE(SL1b.dat, measurevar = "Sperm_length", groupvars = c("Gen"))

SL3<-subset(SL1.dat,Treatment=="G0")
SL3b<-subset(SL2b,Gen=="G0")
SL3c<-subset(SL2c,Gen=="G0")

# plot Figure2
str(SL1.dat)
P1<-ggplot(SL1.dat, aes(Treatment, Sperm_length)) +
  geom_point(aes(shape=Treatment, colour=Treatment, fill=Treatment),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Fam.ID), alpha=0.04, data=SL1.dat)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(SL3$Sperm_length), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Sperm length")+
  #theme(axis.title.x = element_blank())+ 
  ylim(70,100)+theme_light() +
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 82, ymax = 90, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

P1

# sperm length and standard deviation

str(SL3c)
P1b<-ggplot(SL2c, aes(Gen, Sperm_length), group=Gen) +
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen), size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_errorbar(aes(ymin=Sperm_length-sd, ymax=Sperm_length+sd), width=0.1,
                position=position_dodge(0.5), data=SL2c)+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Family.ID), alpha=0.04, data=SL2b)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(SL3b$Sperm_length), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Sperm length")+
  #theme(axis.title.x = element_blank())+ 
  ylim(68,94)+theme_light() +
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.60, xmax = 1.4, ymin = 80, ymax = 93.5, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

P1b


###### plot the variance ####### Figure2

library(rempsyc)

P1c<-nice_varplot(
  data = SL2b,
  variable = "Sperm_length",
  group = "Gen"
)+
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", colour="black", group = 1, size=1, alpha=0.4)+
  #geom_line(aes(group=Family), alpha=0.04, colour="black", data=MABo)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_classic2()+ ylim(70,100)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(SL3b$Sperm_length), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 82, ymax = 90, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Sperm length\nvariance")

P1c


##### basic variance table by Gen #####
#### between family/line variance #####

P1d<-nice_var(data = SL2b,
              variable = "Sperm_length",
              group = "Gen")

P1d


##### within family/line variance #####

P1e<-nice_var(data = SL2b,
              variable = "Sperm_length",
              group = "Family.ID")

P1e


##### adding sperm length, SD plot as an inset ######

library(cowplot)
SPC<-ggdraw()+
  draw_plot(P1)+
  draw_plot(P1c, x=0.42, y=0.575, width=0.55, height=0.4) 
SPC


########## Analysis sperm length and inbreeding ##########


attach(SL1.dat)
str(SL1.dat)

SL1.dat$Treatment<-as.factor(SL1.dat$Treatment)
SL1.dat$Fam.ID<-as.factor(SL1.dat$Fam.ID)

hist(SL1.dat$Sperm_length)

library(glmmTMB)
M1<-glmmTMB(Sperm_length~Treatment +(1|Fam.ID), data=SL1.dat)
summary(M1)

car::Anova(M1)


library(jtools)
library(broom)
library(ggstance)


plot_summs(M1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(M1)


#### check your model performance ####
library(performance)
library(see)
check_model(M1)

###### emmeans #######

library(emmeans)
emmeans(M1,"Treatment")
emmeans(M1, specs = pairwise ~ Treatment)


###############################################

###### variance analysis- sperm length #######

str(SL2b)

library(car)
leveneTest(Sperm_length~Family.ID, data=SL2b)

bartlett.test(Sperm_length~Gen, data=SL2b)

fligner.test(Sperm_length~Gen, data=SL2b)


# sperm length and standard deviation analysis
########## Analysis sperm length, SD and inbreeding ##########
##### within families #####
attach(SL2b)
str(SL2b)

hist(SL2b$Sperm_length)

library(glmmTMB)
M1b<-glmmTMB(sd ~ Gen + (1|Family.ID), data=SL2b)
summary(M1b)

car::Anova(M1b)

library(jtools)
library(broom)
library(ggstance)


plot_summs(M1b, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(M1b)


#### check your model performance ####
library(performance)
library(see)
check_model(M1b)

###### emmeans #######

library(emmeans)
emmeans(M1b,"Gen")
emmeans(M1b, specs = pairwise ~ Gen)


############################
##### between families #####
attach(SL2b)
str(SL2b)

hist(SL2b$Sperm_length)

library(glmmTMB)
M1w<-glmmTMB(sd~Family.ID +(1|Family.ID), data=SL2b)
summary(M1w)

car::Anova(M1w)

library(jtools)
library(broom)
library(ggstance)


plot_summs(M1w, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(M1w)


#### check your model performance ####
library(performance)
library(see)
check_model(M1w)

###### emmeans #######

library(emmeans)
emmeans(M1w,"Gen")
emmeans(M1w, specs = pairwise ~ Gen)


####################################################################################
############################TESTES and INBREEDING ##################################

rm(list=ls())
setwd
TV1.dat <- read.csv ("Balanced_Testes Volume March 2018.csv")
Tfol<- read.csv ("Balanced_Testes Follicle Variance.csv")
str(Tfol)

Tfol$Gen<-as.factor(Tfol$Gen)
Tfol$Family<-as.factor(Tfol$Family)

library(Rmisc)
library(ggplot2)
library(tidyverse)

TV2 <- summarySE (TV1.dat, measurevar = "Testes_vol", groupvars = c("ID"))
Tfol1b<- summarySE(Tfol, measurevar = "follicle.count", groupvars = c("Gen"))
Tfol1c<- summarySE(Tfol, measurevar = "follicle.count", groupvars = c("Family"))


TV3<-subset(TV1.dat,ID=="G0")

#palette selection#

#cbPalette<- c("#005AB5","#DC3220","#DC3220","#DC3220","#DC3220","#DC3220","#DC3220")

#line plot#

P3<-ggplot(TV1.dat, aes(ID, Testes_vol)) +
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin",
             size = 2.0, stroke=1.9, alpha=0.70) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Family), alpha=0.04, data=TV1.dat)+
  geom_point(size=0.5,alpha=0.04, stroke=1.5,fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(TV3$Testes_vol), 
             color = "black", linetype = "dashed", size = 1, alpha=0.5)+
  theme(legend.position="none")+ labs(x="Gen",y = "Testes \nvolume")+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 290, ymax = 635, alpha = .20, colour="grey")

P3

P3a<-ggplot(TV1.dat, aes(ID, Testes_vol))+
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Family), alpha=0.04, data=TV1.dat)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(TV3$Testes_vol), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Testes volume")+
  #theme(axis.title.x = element_blank())+ 
  ylim(0,850)+theme_light()+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 290, ymax = 635, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

P3a


TV2 <- summarySE (TV1.dat, measurevar = "Testes_vol", groupvars = c("ID"))
TV3b<-subset(TV2,ID=="G0")

P3b<-ggplot(TV2, aes(ID, Testes_vol))+
  geom_point(aes(shape=ID, colour=ID, fill=ID), size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_errorbar(aes(ymin=Testes_vol-sd, ymax=Testes_vol+sd), width=0.1,
                position=position_dodge(0.5), data=TV2)+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Family), alpha=0.04, data=TV1.dat)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(TV3$Testes_vol), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Testes volume")+
  #theme(axis.title.x = element_blank())+ 
  ylim(0,650)+theme_light()+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.60, xmax = 1.40, ymin = 290, ymax = 610, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

P3b


###### plot the variance #######

library(rempsyc)

P3c<-nice_varplot(
  data = TV1.dat,
  variable = "Testes_vol",
  group = "ID"
)+
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", colour="black", group = 1, size=1, alpha=0.4)+
  #geom_line(aes(group=Family), alpha=0.04, colour="black", data=MABo)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_classic()+ ylim(0,850)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(TV3b$Testes_vol), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 250, ymax = 650, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Testes volume\nvariance")

P3c


##### adding testes volume, SD plot as an inset ######

library(cowplot)
TVC<-ggdraw()+
  draw_plot(P3a)+
  draw_plot(P3c, x=0.42, y=0.58, width=0.55, height=0.4)
TVC



f1<-ggarrange(SPC, TVC, ncol = 2, nrow = 1, labels = c('A', 'B'),
              font.label = list(size = 18, color = "black", face = "bold", family = "Arial"))
f1


###### variance analysis- testes volume #######

# checks for between and within family variation
str(TV1.dat)
TV1.dat$ID<-as.factor(TV1.dat$ID)
TV1.dat$Family<-as.factor(TV1.dat$Family)

library(car)
leveneTest(Testes_vol~ID, data=TV1.dat)

bartlett.test(Testes_vol~ID, data=TV1.dat)

fligner.test(Testes_vol~ID, data=TV1.dat)


########## Analysis testes volume and inbreeding ##########
attach(TV1.dat)
str(TV1.dat)

TV1.dat$ID<-as.factor(TV1.dat$ID)
TV1.dat$Family<-as.factor(TV1.dat$Family) 

hist(TV1.dat$Testes_vol)

library(glmmTMB)
M2<-glmmTMB(Testes_vol~ID +(1|Family), data=TV1.dat)
summary(M2)

car::Anova(M2)

library(jtools)
library(broom)
library(ggstance)


plot_summs(M2, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(M2)


#### check your model performance ####
library(performance)
library(see)
check_model(M2)

###### emmeans #######

library(emmeans)
emmeans(M2,"ID")
emmeans(M2, specs = pairwise ~ ID)


# testes volume and standard deviation analysis
########## Analysis testes volume, SD and inbreeding ##########

##### within family ######

Tfol1b<- summarySE(Tfol, measurevar = "follicle.count", groupvars = c("Gen"))
Tfol1c<- summarySE(Tfol, measurevar = "follicle.count", groupvars = c("Family"))

attach(Tfol)
str(Tfol)

library(glmmTMB)
Tf0<-glmmTMB(TV.follicles.microns~Gen, data=Tfol)
summary(Tf0)

car::Anova(Tf0)

library(jtools)
library(broom)
library(ggstance)


plot_summs(Tf1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(Tf1)


#### check your model performance ####
library(performance)
library(see)
check_model(Tf1)

###### emmeans #######

library(emmeans)
emmeans(Tf1,"Family")
emmeans(Tf1, specs = pairwise ~ Family)


###### testes follicular variance #######

# checks for between family variation
str(Tfol)

library(car)
leveneTest(TV.follicles.microns~Family, data=Tfol)



##### check differences in testes volume variance #####
str(TV1.dat)
hist(TV1.dat$Testes_vol)

TV1.dat$ID<-as.factor(TV1.dat$ID)
TV1.dat$Family<-as.factor(TV1.dat$Family)

library(car)
leveneTest(Testes_vol~ID, data=TV1.dat)

bartlett.test(Testes_vol~ID, data=TV1.dat)

fligner.test(Testes_vol~ID, data=TV1.dat)


##### between family ######

Tfol1b<- summarySE(Tfol, measurevar = "follicle.count", groupvars = c("Gen"))
Tfol1c<- summarySE(Tfol, measurevar = "follicle.count", groupvars = c("Family"))

attach(Tfol)
str(Tfol)

library(glmmTMB)
Tf1<-glmmTMB(TV.follicles.microns~Family, data=Tfol)
summary(Tf1)

car::Anova(Tf1)

library(jtools)
library(broom)
library(ggstance)


plot_summs(Tf1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(Tf1)


#### check your model performance ####
library(performance)
library(see)
check_model(Tf1)

###### emmeans #######

library(emmeans)
emmeans(Tf1,"Family")
emmeans(Tf1, specs = pairwise ~ Family)


#############################################################################################
############################### INBREEDING & MALE BODY SIZE #################################


############### INBREEDING-REPRODUCTIVE OUTPUT-MALE&FEMALE BODY SIZE #################

rm(list=ls())


BS1.dat <- read.csv ("Inbreeding_Elytra.csv")
library (ggplot2)
library(Rmisc)     #To use summarySE function
library(tidyverse)
library(ggpubr)

str(BS1.dat)
attach(BS1.dat)

BS2 <- summarySE(BS1.dat, measurevar = "Elytra", groupvars = c("ID","Sex"))


BS3<-subset(BS1.dat,Sex=="Male")

#### censor families that weren't extant till the end ####

#(https://rforhr.com/filter.html)

BS3b <- BS3 %>% filter(!Family %in% c(3,4,10,15,25,26,28,29,30,36,37,41)) # extinct families censoring
BS4<-subset(BS3b,ID=="G0")

#

#line plot#

Q1<-ggplot(BS3b, aes(ID, Elytra)) +
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin",
             fun.y = "mean", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(aes(group=ID),stat = "summary_bin", fun.y = "mean", group = 1, size=1)+
  geom_line(aes(group=Family), alpha=0.04, data=BS3b)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(BS4$Elytra), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Male elytra length")+
  #theme(axis.title.x = element_blank())+ 
  ylim(2000,3200)+theme_light() +
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 2300, ymax = 2650, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

Q1

BS3c <- summarySE(BS3b, measurevar = "Elytra", groupvars = c("ID"))
BS4b<-subset(BS3c,ID=="G0")


Q1b<-ggplot(BS3c, aes(ID, Elytra)) +
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin",
             fun.y = "mean", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_errorbar(aes(ymin=Elytra-se, ymax=Elytra+se), width=0.1,
                position=position_dodge(0.5), data=BS3c)+
  geom_line(aes(group=ID),stat = "summary_bin", fun.y = "mean", group = 1, size=1)+
  geom_line(aes(group=Family), alpha=0.04, data=BS3b)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(BS4b$Elytra), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Male Elytra length")+
  #theme(axis.title.x = element_blank())+ 
  ylim(2100,2700)+theme_light() +
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 2300, ymax = 2650, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

Q1b


###### plot the variance #######

library(rempsyc)

Q1c<-nice_varplot(
  data = BS3b,
  variable = "Elytra",
  group = "ID"
)+
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", colour="black", group = 1, size=1, alpha=0.4)+
  #geom_line(aes(group=Family), alpha=0.04, colour="black", data=MABo)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_classic()+ ylim(2000,3200)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(BS4b$Elytra), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 2250, ymax = 2700, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Elytra length\nvariance")

Q1c



##### adding elytra length, SD plot as an inset ######

library(cowplot)
BS4c<-ggdraw()+
  draw_plot(Q1)+
  draw_plot(Q1c, x=0.45, y=0.565, width=0.5, height=0.42)
BS4c


###### variance analysis- elytra length (body size) #######

str(BS3b)
BS3b$ID<-as.factor(BS3b$ID)
BS3b$Family<-as.factor(BS3b$Family)

library(car)
leveneTest(Elytra~ID, data=BS3b)
leveneTest(Elytra~Family, data=BS3b)


bartlett.test(Elytra~ID, data=BS3b)

fligner.test(Elytra~ID, data=BS3b)


###### male elytra analysis and inbreeding ######
# 2024

attach(BS3b)
str(BS3b)

BS3b$Sex<-as.factor(BS3b$Sex)
BS3b$Family<-as.factor(BS3b$Family)
BS3b$ID<-as.factor(BS3b$ID)

hist(BS3b$Elytra)

library(glmmTMB)
M3<-glmmTMB(Elytra~ID +(1|Family), data=BS3b)
summary(M3)

car::Anova(M3)

library(jtools)
library(broom)
library(ggstance)


plot_summs(M3, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(M3)


#### check your model performance ####
library(performance)
library(see)
check_model(M3)

###### emmeans #######

library(emmeans)
emmeans(M3,"ID")
emmeans(M3, specs = pairwise ~ ID)


#######################################################################################################
######################### INBREEDING & REPRODUCTIVE OUTPUT/OFFSPRING COUNTS ###########################

######## reproductive output #########

RO <- read.csv ("Balanced_Offpsring productivity March 2018.csv")

RO2 <- read.csv ("Within and between family variance Reproductive output.csv")
str(RO2)
RO2$Family.ID<-as.factor(RO2$Family.ID)

#### censor families that weren't extant till the end #### (https://rforhr.com/filter.html)
RO <- RO %>% filter(!Family %in% c(10,37))

Summary<- summarySE (RO, measurevar = "Offspring", groupvars = c("ID"))

RO1<-subset(RO,ID=="G0")
str(RO)
attach(RO)


Q3<-ggplot(RO, aes(ID, Offspring)) +
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin", size = 4, stroke=2, alpha=0.95)+ #fun.y = "mean"
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(aes(group=ID),stat = "summary_bin", group = 1, size=1)+ #fun.y = "mean"
  geom_line(aes(group=Family), alpha=0.04, colour="black", data=RO)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(RO1$Offspring), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "7-day Reproductive Output")+
  theme_light()+
  ylim(0,290)+
  #theme(axis.title.x = element_blank())+ 
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 40, ymax = 165, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

Q3


Summary<- summarySE (RO, measurevar = "Offspring", groupvars = c("ID"))
RO1<-subset(RO,ID=="G0")
str(Summary)
Summary$ID<-as.factor(Summary$ID)

Q3b<-ggplot(Summary, aes(ID, Offspring)) +
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin",
             fun.y = "mean", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_errorbar(aes(ymin=Offspring-se, ymax=Offspring+se), width=0.1,
                position=position_dodge(0.5), data=Summary)+
  geom_line(aes(group=ID),stat = "summary_bin", fun.y = "mean", group = 1, size=1)+
  geom_line(aes(group=Family), alpha=0.04, data=RO)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(RO1$Offspring), 
             color = "black", linetype = "dashed", size = 1)+
  labs(x="Gen", y = "Male Elytra length")+
  #theme(axis.title.x = element_blank())+ 
  ylim(0,200)+theme_light() +
  theme (axis.text.y = element_text(colour = "black", face = "bold", size =21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size =21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size =21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.6, xmax = 1.4, ymin = 40, ymax = 165, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")

Q3b


###### plot the variance #######

library(rempsyc)

Q3c<-nice_varplot(
  data = RO,
  variable = "Offspring",
  group = "ID"
)+
  geom_point(aes(shape=ID, colour=ID, fill=ID),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", colour="black", group = 1, size=1, alpha=0.4)+
  #geom_line(aes(group=Family), alpha=0.04, colour="black", data=MABo)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_classic()+ ylim(0,290)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(RO1$Offspring), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size =21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size =21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size =21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 40, ymax = 170, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Reproductive output\nvariance")

Q3c


##### adding reproductive output, SD plot as an inset ######

library(cowplot)
Q3d<-ggdraw()+
  draw_plot(Q3)+
  draw_plot(Q3c, x=0.35, y=0.49, width=0.59, height=0.49)
Q3d


f2<-ggarrange(BS4c, Q3d, ncol = 2, nrow = 1, labels = c('A', 'B'),
              font.label = list(size = 18, color = "black", face = "bold", family = "Arial"))
f2


###### variance analysis- reproductive output #######
# between families

str(RO)
RO$ID<-as.factor(RO$ID)
RO$Family<-as.factor(RO$Family)

library(car)
leveneTest(Offspring~ID, data=RO)

#within family
RO2 <- read.csv ("Within and between family variance Reproductive output.csv")
str(RO2)
RO2$Family.ID<-as.factor(RO2$Family.ID)

leveneTest(Off.counts~Family.ID, data=RO2)

bartlett.test(Offspring~ID, data=RO)

fligner.test(Offspring~ID, data=RO)

ggarrange(Q1, Q3, ncol = 2, nrow = 1, labels = c('A', 'B'))
ggsave("Elyta & Repro. Output-Multiplot.png", width = 7, dpi = 1200)



######## Analysis Reproductive output and inbreeding ###########

str(RO)
RO$Family<-as.factor(RO$Family)
RO$ID<-as.factor(RO$ID)

attach(RO)
str(RO)

hist(RO$Offspring)

library(glmmTMB)
O1<-glmmTMB(Offspring~ID+ (1|Family), family=nbinom2, data=RO)
summary(O1)

car::Anova(O1)

library(jtools)
library(broom)
library(ggstance)


plot_summs(O1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(O1)


#### check your model performance ####
library(performance)
library(see)
check_model(O1)

###### emmeans #######

library(emmeans)
emmeans(O1,"ID")
emmeans(O1, specs = pairwise ~ ID)



##############################################################################################################
############################## INBREEDING & MAL, FEMALE LIFESPAN #############################################


#rm(list=ls())

#setwd("C:/Users/Ram/Dropbox/2016-R/Inbreeding-Variance")# if working on the TOSHIBA laptop
#LDS <- read.csv ("InbreedingSurvival_Sons_Daughters.csv") # SONS+DAUGHTERS

library(ggplot2)
library(tidyverse)
library(survival) # load survival library
library(ggpubr)
library(survminer)
library(rms)
library(multcomp)
library(Rmisc)


#### male lifespan survival plot #####

str(LS.m)

LS.m2<- summarySE (LS.m, measurevar = "Lifespan", groupvars = c("Gen"))
(74.8-36.4)/74.8*100
LS.m2b<- summarySE (LS.m, measurevar = "Lifespan", groupvars = c("Fam.ID","Gen","Status"))

fit1<- survfit(Surv(Lifespan,Status)~Gen, data = LS.m2b)
summary(fit1)
fit1b <- coxph(Surv(Lifespan,Status)~Gen, data = LS.m2b)
fit1b
summary(fit1b)

x<-ggsurvplot(fit1, data = LS.m2b,size=1,
              palette = c("#0000FF","#CC6666","#663300", 
                          "#993333", "#CCCC00", "#CCCC99",
                          "#999900"),
              legend.labs=c("0","1","2","3","4","5","6"),
              legend.title=c("Gen"),
              scales = "free_y", xlim=c(0,100),break.x.by=25,
              break.time.by = 5, conf.int.style = "step", conf.int=FALSE,
              panel.labs = list(ID = c("Male")),
              xlab = "Time (Weeks)",
              ylab="Survival Probability", legend = "right", ggtheme = theme_bw(),
              font.x = c(21, "bold", "black"), font.y = c(21, "bold", "black"),
              font.tickslab = c(21, "bold", "black"))
x$plot<-x$plot + theme(legend.text = element_text(size = 21, color = "black", face = "bold"))
x

ggforest(fit1b)



###### variance analysis- lifespan #######

str(LS.m2b)

library(car)
leveneTest(Lifespan~Fam.ID, data=LS.m)
leveneTest(Lifespan~Gen, data=LS.m2b)

bartlett.test(Offspring~ID, data=RO)

fligner.test(Offspring~ID, data=RO)



#########################################
##### female lifespan survival plot #####

attach (LS.f)
str(LS.f)

#LDS.f<- LDS %>% filter(ID=="Female")

LS.f2<- summarySE (LS.f, measurevar = "Lifespan", groupvars = c("Gen"))
(40-17)/40*100
(39.8-17.3)/39.8*100
LS.f2b<- summarySE (LS.f, measurevar = "Lifespan", groupvars = c("Gen","Fam.ID","Status"))

fit2b<- survfit(Surv(Lifespan,Status)~Gen, data = LS.f2b)
summary(fit2b)
fit2c <- coxph(Surv(Lifespan,Status)~Gen, data = LS.f2b)
fit2c
summary(fit2c)

fy<-ggsurvplot(fit2b, data = LS.f2b,size=1,
               palette = c("#0000FF","#CC6666","#663300", 
                           "#993333", "#CCCC00", "#CCCC99",
                           "#999900"),
               legend.labs=c("0","1","2","3","4","5","6"),
               legend.title=c("Gen"),
               scales = "free_y", xlim=c(0,75),break.x.by=25,
               break.time.by = 5, conf.int.style = "step", conf.int=FALSE,
               panel.labs = list(ID = c("Female")),
               xlab = "Time (Weeks)",
               ylab="Survival Probability", legend = "right", ggtheme = theme_bw(),
               font.x = c(21, "bold", "black"), font.y = c(21, "bold", "black"),
               font.tickslab = c(21, "bold", "black"))

fy

fy$plot<-fy$plot + theme(legend.text = element_text(size = 21, color = "black", face = "bold"))
fy

fx<-ggforest(fit2c)
fx

######## Lifespan line and points ##############
###### male lifespan ######

library(Rmisc)

#MABo <- read.csv ("InbredSurvivalSons.csv")

#head(MABo)

#attach (MABo)
#head(MABo)

#MABo1<- summarySE(MABo, measurevar = "Deaths", groupvars = c("Gen"))

#names(Inbr.dat)

MABo2<-subset(LS.m2, Gen=="G0") # subsetting 

Z1<-ggplot(LS.m2, aes(Gen, Lifespan)) +
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Fam.ID), alpha=0.04, colour="black", data=LS.m2b)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_bw()+ ylim(0,125)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(MABo2$Lifespan), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 29, ymax = 104, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Male Longevity (Weeks)")

Z1


###### plot the variance #######

library(rempsyc)

NPL<-nice_varplot(
  data = MABo,
  variable = "Deaths",
  group = "Gen"
)+
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", colour="black", group = 1, size=1, alpha=0.4)+
  #geom_line(aes(group=Family), alpha=0.04, colour="black", data=MABo)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_bw()+ ylim(0,120)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(MABo2$Deaths), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 16, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 16, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 16, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=16))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 29, ymax = 104, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Variance\nMale Longevity")

NPL


##### adding male lifespan, variance plot as an inset ######

library(cowplot)
LIF<-ggdraw()+
  draw_plot(Z1)+
  draw_plot(NPL, x=0.45, y=0.535, width=0.5, height=0.45)
LIF


###### variance analysis- lifespan #######

str(MABo)
MABo$Gen<-as.factor(MABo$Gen)

library(car)
leveneTest(Deaths~Gen, data=MABo)

bartlett.test(Deaths~Gen, data=MABo)

fligner.test(Deaths~Gen, data=MABo)


########## female lifespan continued ############

FABo2<-subset(LS.f2, Gen=="G0") # subsetting 

Z2<-ggplot(LS.f2, aes(Gen, Lifespan)) +
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen),stat = "summary_bin",
             fun.y = "mean", size = 2.0, stroke=1.9, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+theme_grey()+
  ylim(0,100)+
  geom_line(stat = "summary_bin", fun.y = "mean", group = 1, size=1)+
  geom_line(aes(group=Fam.ID), alpha=0.10, data=LS.f2b)+
  geom_point(size=0.5, alpha=0.10, stroke=1.5,fill="#D8D8D8")+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(FABo2$Lifespan), 
             color = "black", linetype = "dashed", size = 1, alpha=0.5)+
  theme(legend.position="none")+ labs(x="Gen",y = "Female Longevity\nWeeks")+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 18, ymax = 70, alpha = 0.20, colour="grey")

Z2

# female lifespan continued

library(Rmisc)

FABo <- read.csv ("InbredSurvivalDaughters.csv")

head(FABo)

attach (FABo)
str(FABo)

FABo$Gen<-as.factor(FABo$Gen)
FABo$Family<-as.factor(FABo$Family)

FABo1<- summarySE(FABo, measurevar = "Deaths", groupvars = c("Gen"))

#names(Inbr.dat)

FABo2<-subset(LS.f2, Gen=="G0") # subsetting

Z2b<-ggplot(LS.f2, aes(Gen, Lifespan)) +
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", group = 1, size=1)+
  geom_line(aes(group=Fam.ID), alpha=0.04, colour="black", data=LS.f2b)+
  geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_bw()+ ylim(0,75)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(FABo2$Lifespan), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size =21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size =21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size =21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 18, ymax = 72, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Female Lifespan (Weeks)")

Z2b


###### plot the variance #######

library(rempsyc)

FPL<-nice_varplot(
  data = FABo,
  variable = "Deaths",
  group = "Gen"
)+
  geom_point(aes(shape=Gen, colour=Gen, fill=Gen),stat = "summary_bin", size = 4, stroke=2, alpha=0.95) +
  scale_colour_manual(values=c("#648FFF","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#FE6100"))+
  scale_fill_manual(values=c("#648FFF","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8","#D8D8D8"))+
  scale_shape_manual(values=c(21,22,22,22,22,22,22))+
  geom_line(stat = "summary_bin", colour="black", group = 1, size=1, alpha=0.4)+
  #geom_line(aes(group=Family), alpha=0.04, colour="black", data=MABo)+
  #geom_point(size=0.5, alpha=0.04, stroke=1.5, fill="black")+
  theme_bw()+ ylim(0,90)+
  #scale_colour_manual(values = cbPalette)+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+
  geom_hline(yintercept = mean(FABo2$Deaths), 
             color = "black", linetype = "dashed", size = 1)+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 16, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 16, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 16, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=16))+
  annotate("rect", xmin = 0.80, xmax = 1.20, ymin = 18, ymax = 72, alpha = 0.02, colour="black", fill="lightblue")+
  theme(legend.position = "none")+ labs(x ="Gen", y = "Variance\nFemale Longevity")

FPL


##### adding female lifespan, variance plot as an inset ######

library(cowplot)
FIL<-ggdraw()+
  draw_plot(Z2b)+
  draw_plot(FPL, x=0.45, y=0.535, width=0.5, height=0.45)
FIL



ggarrange(x, Z1, nrow=1, ncol = 2)
ggsave("Male and Female Longevity-Multiplot.png", width = 7, dpi = 1200)



########################################################################################################
######################### INBREEDING & TRAIT HERITABILITY ##############################################

##############################HERITABILITY STUFF####################################
#################Experimental Inbreeding and heritability###########################
##############Correlations between gens for sperm length#####################


rm(list=ls())
setwd
SLC.dat <- read.csv ("Balanced_Corr_Gens_Sperm Length.csv")

library (ggplot2)
library(ggpmisc)
library(ggpubr)
library(Rmisc)     #To use summarySE function
library(tidyverse)

summary(SLC.dat)
head(SLC.dat)
tail(SLC.dat)

SLC2<-SLC.dat # renaming factors (R Graphics cookbook, pg. 246)
levels(SLC2$Corr_Gen)
Gen_names <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

SLC2$NN = factor(SLC2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A1<-ggplot(SLC2, aes(x = SL1, y = SL2)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25, size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  scale_x_continuous(limits = c(70,95), breaks = c(75, 85))+
  scale_alpha_continuous(range = c(0.2,0.3,0.4,0.5,0.6,0.7,1))+
  facet_grid(. ~ NN, labeller = as_labeller(Gen_names)) +
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="Sperm length (μm)", y="Sperm length (μm)")+ # Unicode micron symbol: μ
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(strip.text = element_text(colour = "black", face="italic", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 28, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 28, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")
A1

########Correlations between gens- offspring productivity (mean of 2 sib x sib pairs)##########

Off <- read.csv ("Balanced_Corr_Gens_Offspring Productivity.csv")

summary(Off)
head(Off)
tail(Off)

Off2<-Off # renaming factors (R Graphics cookbook, pg. 246)
levels(SLC2$Corr_Gen)
Gen_names2 <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

Off2$ON = factor(Off2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A2<-ggplot(Off2, aes(x = Ofs1, y = Ofs2)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25,size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  facet_grid(. ~ ON, labeller = as_labeller(Gen_names2))+
  scale_x_continuous(limits = c(0, 160), breaks = c(40, 160))+
  #xlim(0,160)+ scale.x.limits = c(0, 80, 160)+
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="7-day RO", y="7-day RO")+
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(strip.text = element_text(colour = "black", face="italic", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 28, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 28, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")
A2


####correlation betweeen gens- Testes volume ###############################

TV <- read.csv ("Balanced_Corr_Gens_Testes Volume.csv")

summary(TV)
head(TV)
tail(TV)

TV2<-TV # renaming factors (R Graphics cookbook, pg. 246)
levels(TV2$Corr_Gen)
Gen_names3 <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

TV2$Vol = factor(TV2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A3<-ggplot(TV2, aes(x = TV1, y = TV2)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25,size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  facet_grid(. ~ Vol, labeller = as_labeller(Gen_names3))+
  scale_x_continuous(limits = c(0, 800), breaks = c(200, 800))+
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="Testes volume (μm^3)", y="Testes volume (μm^3)")+
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(strip.text = element_text(colour = "black", face="italic", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 28, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 28, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")
A3


##### correlation betweeen gens- Male elytra length ######

ELY <- read.csv ("Balanced_Corr_Gens_Male Elytra Length.csv")

summary(ELY)
head(ELY)
tail(ELY)

ELY2<-ELY # renaming factors (R Graphics cookbook, pg. 246)
levels(ELY2$Corr_Gen)
Gen_names4 <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

ELY2$LEN = factor(TV2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A4<-ggplot(ELY2, aes(x = EL1, y = EL2)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25,size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  facet_grid(. ~ LEN, labeller = as_labeller(Gen_names4))+
  scale_x_continuous(limits = c(1900, 2900), breaks = c(1900, 2300))+
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="Male EL (μm)", y="Male EL (μm)")+
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(strip.text = element_text(colour = "black", face="italic", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 28, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 28, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")
A4

#Corr.dat$new = factor(Corr.dat$Corr_Gen, levels = c("G0_F1", "F1_F2", "F2_F3", "F3_F4", "F4_F5", "F5_F6"))

#ggplot(Corr.dat, aes(x = EL1, y = EL2, colour = Corr_Gen)) + 
#  geom_point(shape = 16) + facet_grid(. ~ new) + geom_smooth(method = "lm", aes(fill = Corr_Gen))+
#  theme_classic() + theme_bw() + theme(panel.grid = element_blank()) +
#  theme(legend.position="none") + stat_cor(method = "pearson",
#                                           size = 2,colour = "black", label.x = 2.4, label.y = 2.7)

############Female Elytra Length################
#####correlation betweeen gens- elytra length#####

FELY <- read.csv ("Balanced_Corr_Gens_Female Elytra Length.csv")

summary(FELY)
head(FELY)
tail(FELY)

FELY2<-FELY # renaming factors (R Graphics cookbook, pg. 246)
levels(FELY2$Corr_Gen)
Gen_names5 <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

FELY2$FLEN = factor(FELY2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A5<-ggplot(FELY2, aes(x = EL1, y = EL2)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25,size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  facet_grid(. ~ FLEN, labeller = as_labeller(Gen_names5))+
  scale_x_continuous(limits = c(1900, 2900), breaks = c(1900, 2300))+
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="Female EL (μm)", y="Female EL (μm)")+
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(legend.position="none")
A5

########Longevity (Extrinsic Mortality)###########
#####correlation betweeen gens- male longevity#####

MEL <- read.csv ("Balanced_Corr_Gens_Male Longevity.csv")

summary(MEL)
head(MEL)
tail(MEL)

MEL2<-MEL # renaming factors (R Graphics cookbook, pg. 246)
levels(MEL2$Corr_Gen)
Gen_names6 <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

MEL2$MELW = factor(MEL2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A6<-ggplot(MEL2, aes(x = Age1_weeks, y = Age2_weeks)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25, size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  facet_grid(. ~ MELW, labeller = as_labeller(Gen_names6))+
  scale_x_continuous(limits = c(0, 110), breaks = c(0, 75))+
  #scale_x_continuous(limits = c(0, 75))+
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="Male longevity (Weeks)", y="Male longevity (Weeks)")+
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(strip.text = element_text(colour = "black", face="italic", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 28, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 28, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")
A6

#####correlation betweeen gens- female longevity#####

FEL <- read.csv ("Balanced_Corr_Gens_Female Longevity.csv")

summary(FEL)
head(FEL)
tail(FEL)

FEL2<-FEL # renaming factors (R Graphics cookbook, pg. 246)
levels(FEL2$Corr_Gen)
Gen_names7 <- c(
  `G0_F1` = "0-1",
  `F1_F2` = "1-2",
  `F2_F3` = "2-3",
  `F3_F4` = "3-4",
  `F4_F5` = "4-5",
  `F5_F6` = "5-6"
)

FEL2$FELW = factor(FEL2$Corr_Gen, levels=c('G0_F1','F1_F2','F2_F3','F3_F4','F4_F5','F5_F6'))

A7<-ggplot(FEL2, aes(x = Age1_weeks, y = Age2_weeks)) + 
  geom_point(aes(shape=Corr_Gen, colour=Corr_Gen, fill=Corr_Gen), stroke=1.25, size=1) +#fill="white"
  scale_shape_manual(values=c(22,22,22,22,22,21))+
  facet_grid(. ~ FELW, labeller = as_labeller(Gen_names7))+
  scale_x_continuous(limits = c(0, 100), breaks = c(0, 75))+
  #scale_x_continuous(limits = c(0, 75))+
  geom_smooth(method = "lm",aes(colour = Corr_Gen))+
  theme_classic() + #theme(panel.grid = element_blank()) +
  labs(x="Female longevity (Weeks)", y="Female longevity (Weeks)")+
  scale_colour_manual(values=c("#FE6100","#FE6100","#FE6100","#FE6100","#FE6100","#648FFF"))+
  scale_fill_manual(values=c("#FFFFFF","#D8D8D8","#9D9EA0","#868686","#696969","#648FFF"))+
  theme(legend.position="none")
A7

ggarrange(A1, ncol = 1,align = c("v"))
ggsave("Sperm-Heritability.png", width = 7, dpi = 1200)

ggarrange(A2, ncol = 1,align = c("v"))
ggsave("RO-Heritability.png", width = 7, dpi = 1200)

ggarrange(A3, ncol = 1,align = c("v"))
ggsave("Testes-Heritability.png", width = 7, dpi = 1200)

ggarrange(A4, ncol = 1,align = c("v"))
ggsave("Male Elytra-Heritability.png", width = 7, dpi = 1200)

ggarrange(A5, ncol = 1,align = c("v"))
ggsave("Female Elytra-Heritability.png", width = 7, dpi = 1200)

ggarrange(A6, ncol = 1,align = c("v"))
ggsave("Male Longevity-Heritability.png", width = 7, dpi = 1200)

ggarrange(A7, ncol = 1,align = c("v"))
ggsave("Female Longevity-Heritability.png", width = 7, dpi = 1200)


library(ggpubr)
ggarrange(A1, A3, A4, A2, A6, ncol = 3, nrow = 2, labels = c('A', 'B', 'C', 'D', 'E'),
          font.label = list(size = 21, color = "black", face = "bold", family = "Arial"))


########################################################################################################
############################ COMPARING G0 & G7 TRAITS (NO INBREEDING) ##################################

############################################################
########## Comparing Gen 0 and Gen 7 Phenotypes ############

#male body size

rm(list=ls())
setwd("/Users/fbsrva/Library/CloudStorage/OneDrive-UniversityofLeeds/2016-R/Inbreeding-Variance")
BS07 <- read.csv ("G0_G7_Elytra_Male_Female comparisons.csv", fileEncoding = "Latin1", check.names = F)

library (ggplot2)
library(Rmisc)     #To use summarySE function
library(tidyverse)
library(ggpubr)

str(BS07)
BS07.m<- BS07 %>% filter(Sex=="Male")

str(BS07.m)
BS07$Sex<-as.factor(BS07$Sex)
BS07$ID<-as.factor(BS07$ID)

BS07b <- summarySE(BS07.m, measurevar = "Elytra", groupvars = c("ID"))
str(BS07b)

P1<-ggplot(BS07b, aes(ID, Elytra), colour=ID) +
  geom_point(aes(colour=ID, shape=ID), size=3, stroke=1.6, alpha=0.99) +
  geom_errorbar(aes(ymin=Elytra-se, ymax=Elytra+se, colour=ID), width=0.1)+
  #geom_line(aes(group = Family), size=0.75,alpha=0.1, data=BS07.m)+
  scale_colour_manual(values=c("blue", "#963E00"))+
  scale_shape_manual(values=c(21,22))+
  #geom_line(group = 1, size=1)+
  #facet_grid(. ~ Sex)+
  theme_bw()+
  geom_jitter(aes(colour=ID),
              position=position_jitterdodge(jitter.width = 0.5, dodge.width = 0.4),
              data=BS07,alpha=0.4)+
  scale_x_discrete(labels=c("G0", "G7"))+
  theme(strip.background =element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(strip.text = element_text(colour = "black", face="bold", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")+ labs(y = "Body size")

P1

###### variance analysis- elytra length (body size) #######

str(BS07.m)
BS07.m$ID<-as.factor(BS07.m$ID)

library(car)
leveneTest(Elytra~ID, data=BS07.m)

bartlett.test(Elytra~ID, data=BS07.m)

fligner.test(Elytra~ID, data=BS07.m)


########## Analysis male elytra across G0 & G7 ##########
attach(BS07.m)
str(BS07.m)

BS07.m$Sex<-as.factor(BS07.m$Sex)
BS07.m$Family<-as.factor(BS07.m$Family)
BS07.m$ID<-as.factor(BS07.m$ID)

hist(BS07.m$Elytra)

library(glmmTMB)
B1<-glmmTMB(Elytra~ID, data=BS07.m)
summary(B1)

car::Anova(B1)

library(jtools)
library(broom)
library(ggstance)


plot_summs(B1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(B1)


#### check your model performance ####
library(performance)
library(see)
check_model(B1)

###### emmeans #######

library(emmeans)
emmeans(B1,"ID")
emmeans(B1, specs = pairwise ~ ID)


# Sperm length comparisons (# Unicode micron symbol: μ)


setwd("/Users/fbsrva/Library/CloudStorage/OneDrive-UniversityofLeeds/2016-R/Inbreeding-Variance")
S07 <- read.csv ("G0_G7_SL comparisons.csv", fileEncoding = "Latin1", check.names = F)

library (ggplot2)
library(Rmisc)     #To use summarySE function
library(tidyverse)
library(ggpubr)

str(S07)
S07$Gen<-as.factor(S07$Gen)
S07$Family.ID<-as.factor(S07$Family.ID)

S07b <- summarySE(S07, measurevar = "Sperm_length", groupvars = c("Gen","Family.ID"))
S07c <- summarySE(S07, measurevar = "Sperm_length", groupvars = c("Gen"))
str(S07b)

P2<-ggplot(S07c, aes(Gen, Sperm_length), colour=Gen) +
  geom_point(aes(colour=Gen, shape=Gen), size=3, stroke=1.6, alpha=0.99) +
  geom_errorbar(aes(ymin=Sperm_length-se, ymax=Sperm_length+se, colour=Gen), width=0.1, alpha=0.9)+
  scale_colour_manual(values=c("blue", "#963E00"))+
  #geom_line(aes(group = Family.ID), size=0.75,alpha=0.1, data=S07b)+
  #geom_line(group = 1, size=1)+
  scale_shape_manual(values=c(21,22))+
  theme_bw()+
  geom_jitter(aes(colour=Gen),
              position=position_jitterdodge(jitter.width = 0.5, dodge.width = 0.4),
              data=S07b,alpha=0.4)+
  scale_x_discrete(labels=c("G0", "G7"))+
  theme(strip.background =element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(strip.text = element_text(colour = "black", face="bold", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")+ labs(y = "Sperm length")

P2

###### variance analysis- sperm length #######

str(S07b)

library(car)
leveneTest(Sperm_length~Gen, data=S07b)

bartlett.test(Sperm_length~Gen, data=S07b)

fligner.test(Sperm_length~Gen, data=S07b)


########## Analysis sperm length across G0 & G7 ##########
attach(S07)
str(S07)

S07$Sperm_ID<-as.factor(S07$Sperm_ID)


hist(S07$Sperm_length)

library(glmmTMB)
S1<-glmmTMB(Sperm_length~Gen, data=S07)
summary(S1)

car::Anova(S1)

library(jtools)
library(broom)
library(ggstance)


plot_summs(S1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(S1)


#### check your model performance ####
library(performance)
library(see)
check_model(S1)

###### emmeans #######

library(emmeans)
emmeans(S1,"Gen")
emmeans(S1, specs = pairwise ~ Gen)


# Testes follicle comparisons (# Unicode micron symbol: μ)

setwd("/Users/fbsrva/Library/CloudStorage/OneDrive-UniversityofLeeds/2016-R/Inbreeding-Variance")
TV07 <- read.csv ("G0_G7_TV comparisons.csv", fileEncoding = "Latin1", check.names = F)

str(TV07)
TV07$ID<-as.factor(TV07$ID)
TV07$Family<-as.factor(TV07$Family)

TV07b <- summarySE(TV07, measurevar = "Testes_vol_microns", groupvars = c("ID"))
str(TV07b)

P3<-ggplot(TV07b, aes(ID, Testes_vol_microns), colour=ID) +
  geom_point(aes(colour=ID, shape=ID), size=3, stroke=1.6, alpha=0.99) +
  geom_errorbar(aes(ymin=Testes_vol_microns-se, ymax=Testes_vol_microns+se, colour=ID), width=0.1, alpha=0.9)+
  scale_colour_manual(values=c("blue", "#963E00"))+
  #geom_line(aes(group = Family), size=0.75,alpha=0.1, data=TV07)+
  #geom_line(group = 1, size=1)+
  scale_shape_manual(values=c(21,22))+
  theme_bw()+
  geom_jitter(aes(colour=ID),
              position=position_jitterdodge(jitter.width = 0.5, dodge.width = 0.4),
              data=TV07,alpha=0.4)+
  scale_x_discrete(labels=c("G0", "G7"))+
  theme(strip.background =element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(strip.text = element_text(colour = "black", face="bold", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")+ labs(y = "Testes volume")

P3

###### variance analysis- testes volume #######

str(TV07)

library(car)
leveneTest(Testes_vol_microns~ID, data=TV07)

bartlett.test(Testes_vol_microns~ID, data=TV07)

fligner.test(Testes_vol_microns~ID, data=TV07)

########## Analysis testes size across G0 & G7 ##########
attach(TV07)
str(TV07)

hist(TV07$Testes_vol_microns)

library(glmmTMB)
T1<-glmmTMB(Testes_vol_microns~ID, data=TV07)
summary(T1)

car::Anova(T1)

library(jtools)
library(broom)
library(ggstance)


plot_summs(S1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(S1)


#### check your model performance ####
library(performance)
library(see)
check_model(S1)

###### emmeans #######

library(emmeans)
emmeans(S1,"Gen")
emmeans(S1, specs = pairwise ~ Gen)



# Reproductive output (offspring, 7-days)

setwd("/Users/fbsrva/Library/CloudStorage/OneDrive-UniversityofLeeds/2016-R/Inbreeding-Variance")
Fit07 <- read.csv ("G0_G7_RO comparisons.csv", fileEncoding = "Latin1", check.names = F)

str(Fit07)
Fit07$ID<-as.factor(Fit07$ID)
Fit07$Family<-as.factor(Fit07$Family)

Fit07b <- summarySE(Fit07, measurevar = "Offspring", groupvars = c("ID"))
str(Fit07b)

P4<-ggplot(Fit07b, aes(ID, Offspring), colour=ID) +
  geom_point(aes(colour=ID, shape = ID), size=3, stroke=1.6, alpha=0.99) +
  geom_errorbar(aes(ymin=Offspring-se, ymax=Offspring+se, colour=ID), width=0.1, alpha=0.9)+
  scale_colour_manual(values=c("blue", "#963E00"))+
  #geom_line(aes(group = Family), size=0.75,alpha=0.2, data=Fit07)+
  #geom_line(group = 1, size=1)+
  scale_shape_manual(values=c(21,22))+
  theme_bw()+
  geom_jitter(aes(colour=ID),
              position=position_jitterdodge(jitter.width = 0.5, dodge.width = 0.4),
              data=Fit07,alpha=0.4)+
  scale_x_discrete(labels=c("G0", "G7"))+
  theme(strip.background =element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(strip.text = element_text(colour = "black", face="bold", size=21))+
  theme (axis.text.y = element_text(colour = "black", face = "bold", size = 21, family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", size = 21, family = "Times", face = "bold"),
        axis.title.y = element_text(colour= "black", size = 21, family = "Times", face = "bold"))+
  theme(axis.text.x = element_text(family="Times", face="bold", colour="black", size=21))+
  theme(legend.position="none")+ labs(y = "7-day Reproductive output")

P4

ggarrange(P2, P3, P1, P4, ncol = 2, nrow = 2, labels = c('A', 'B', 'C', 'D'),
          font.label = list(size = 21, color = "black", face = "bold", family = "Arial"))

###### variance analysis- reproductive output #######

str(Fit07)

library(car)
leveneTest(Offspring~ID, data=Fit07)

bartlett.test(Offspring~ID, data=Fit07)

fligner.test(Offspring~ID, data=Fit07)


########## Analysis reproductive output across G0 & G7 ##########
attach(Fit07)
str(Fit07)

hist(Fit07$Offspring)

library(glmmTMB)
F1<-glmmTMB(Offspring~ID, family=nbinom1(link="log"), data=Fit07)
summary(F1)

car::Anova(F1)

library(jtools)
library(broom)
library(ggstance)


plot_summs(F1, scale = TRUE, exp = FALSE)

library(ggstats)
ggcoef_model(F1)


#### check your model performance ####
library(performance)
library(see)
check_model(F1)

###### emmeans #######

library(emmeans)
emmeans(F1,"ID")
emmeans(F1, specs = pairwise ~ ID)



#######################################################################################################
############################### EXTINCTIONS THROUGH INBREEDING ########################################


##################Extinctions across inbreeding generations#######################

rm(list=ls())
setwd("/Users/fbsrva/Library/CloudStorage/OneDrive-UniversityofLeeds/2016-R/Inbreeding-Variance")
#rm(list=ls())
#setwd("C:/Users/hjr14asu/Dropbox/2016-R/Inbreeding-Variance")
#setwd("C:/Users/User/Dropbox/2016-R/Inbreeding-Variance")
CV.dat <- read.csv ("Extinctions.csv")

library (ggplot2)
library(Rmisc)     #To use summarySE function
library(lattice)
library(raster)

head(CV.dat)

ggplot(CV.dat, aes(x = Gen, y = Extinctions, fill = Gen)) +
  geom_bar(stat = "identity", width = 0.5) + ylim(0,8)+
  theme_bw() + theme(legend.position="none") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.text.x=element_text(colour="black", size = 16, family = "Times", face = "bold"),
        axis.text.y=element_text(colour="black", size = 16, face = "bold", family = "Times"))+
  theme(axis.title.x = element_text(colour= "black", face = "bold", size = 16, family = "Times"),
        axis.title.y = element_text(colour= "black", face = "bold", size = 16, family = "Times"))+
  scale_x_discrete(labels=c("0", "1", "2", "3", "4", "5", "6"))+ ylab("Extinct families")+
  xlab("Experimental generations")+
  scale_fill_brewer(palette = "Oranges")+
  coord_flip()



####### Package Citation ########

sessionInfo()
R.Version()
RStudio.Version()
citation()

citation("ggplot2")
citation("car")
citation("MASS")
citation("Rmisc")
citation("glmmTMB")
citation("tidyverse")
citation("emmeans")
citation("survminer")
citation("survival")
citation("dplyr")
citation("ggstats")
citation("performance")
citation("rempsyc")
citation("see")
citation("cowplot")

