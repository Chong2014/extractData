library(lme4)
whole = read.csv("/Users/chongzhang/Desktop/ENG.csv")
summary(whole)
head(whole)
SS = subset(whole, Rctype == "SS")
SS
SO = subset(whole, Rctype == "SO")
OS = subset(whole, Rctype == "OS")
OO = subset(whole, Rctype == "OO")

#ARC_EQUI_INANIM_NP2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP2RT
#l20 = lmer(PRE_TAR ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)

lR3 = lmer (log_R3 ~ Rctype*Matching + (1|Participant)+(1|Item), whole)
lR3

S_V_in_RC1 = subset(whole, RC1 == "S")$log_R3
O_V_in_RC1 = subset(whole, RC1 == "O")$log_R4
t.test(S_V_in_RC1, O_V_in_RC1)
