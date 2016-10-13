library(lme4)
library(lmerTest)
wholeENG = read.csv("/Users/chongzhang/Onedrive/Data-Analyses-R-Python-mySQL/csv files/ENG.csv")
summary(wholeENG)
head(wholeENG)

lR78 = lmer (log_R78 ~ dprimeT * log_R34 * RC1 * RC2 + (1+log_R34|Participant)+(1+log_R34|Item), wholeENG)
summary(lR78)

SS = subset(wholeENG, RCtype == "SS")
SO = subset(wholeENG, RCtype == "SO")
OS = subset(wholeENG, RCtype == "OS")
OO = subset(wholeENG, RCtype == "OO")

SS_SO = subset(wholeENG, RCtype == "SO" | RCtype == "SS")
OS_OO = subset(wholeENG, RCtype == "OS" | RCtype == "OO")
SS_OS = subset(wholeENG, RCtype == "OS" | RCtype == "SS")
SO_OO = subset(wholeENG, RCtype == "SO" | RCtype == "OO")
SO_OS = subset(wholeENG, RCtype == "SO" | RCtype == "OS")
SS_OO = subset(wholeENG, RCtype == "SS" | RCtype == "OO")


summary(SS) #log_R78: mean = 3.150
summary(SO) #log_R78: mean = 3.133
summary(OS) #log_R78: mean = 3.154
summary(OO) #log_R78: mean = 3.120 
#OO >> SO >> SS >> OS

#---------------------------------------------------------------------------------------------------------------------#
#SS vs. SO 
summary(lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SS_SO)) #not sig.t = 0.679
summary(lmer(log_R78 ~ RCtype + (1+log_R34|Participant)+(1+log_R34|Item), SS_SO)) #t = 0.701
summary(lmer(log_R78 ~ RCtype + (1|Participant)+(1|Item), SS_SO)) #t = 1.227
#OO vs. OS 
summary(lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), OS_OO)) #sig. t = 1.998
summary(lmer(log_R78 ~ RCtype + (1+log_R34|Participant)+(1+log_R34|Item), OS_OO)) # t = 2.05
summary(lmer(log_R78 ~ RCtype + (1|Participant)+(1|Item), OS_OO)) # t = 2.419
#SS vs. SO 
summary(lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SS_OS)) #not sig. t = 0.409
summary(lmer(log_R78 ~ RCtype + (1+log_R34|Participant)+(1+log_R34|Item), SS_OS)) #t = 0.343
summary(lmer(log_R78 ~ RCtype + (1|Participant)+(1|Item), SS_OS)) # t = -0.265
#OO vs. SO 
summary(lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SO_OO)) #not sig. t = 1.337
summary(lmer(log_R78 ~ RCtype + (1+log_R34|Participant)+(1+log_R34|Item), SO_OO)) # t = 1.315
summary(lmer(log_R78 ~ RCtype + (1|Participant)+(1|Item), SO_OO)) # t = 0.932
#SO vs. OS #not sig. t = -0.006
summary(lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SO_OS))
#SS vs. OO #sig. t = 2.239
summary(lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SS_OO))
#-----------------------------------------------------------------------------------#
#SS vs. SO 
summary(lmer(log_R78 ~ dprimeT * RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SS_SO))
summary(lmer(log_R78 ~ log_R34 * dprimeT * RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SS_SO)) # SS vs. SO not sig.
#OO vs. OS 
summary(lmer(log_R78 ~ log_R34 * dprimeT * RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), OS_OO)) #couldn't converge
summary(lmer(log_R78 ~ log_R34 * dprimeT * RCtype + (1+log_R34|Participant)+(1+log_R34|Item), OS_OO))
summary(lmer(log_R78 ~ log_R34 * dprimeT * RCtype + (1|Participant)+(1|Item), OS_OO))
summary(lmer(log_R78 ~ dprimeT * RCtype + (1|Participant)+(1|Item), OS_OO))
summary(lmer(log_R78 ~ RCtype  + (1|Participant)+(1|Item), OS_OO))

#SS vs. SO 
summary(lmer(log_R78 ~ log_R34 * dprimeT * RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), SS_OS)) #not sig. t = 0.409