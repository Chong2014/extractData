library(lme4.0)
x = read.csv("/Users/chongzhang/OneDrive/0815R.csv")
summary(x)
head(x)
xMain = subset(x, MainFill == "MAIN")
#lmer(DV ~A*B*C + (1+A*B*C|item)+(1+A*B*C|participant))
l1 = lmer(PassiveMarkerRT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l1
l2 = lmer(Region1RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l2
l3 = lmer(Region2RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l3
l4 = lmer(de1RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l4
l5 = lmer(demRT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l5
l6 = lmer(NP1RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l6
l7 = lmer(de2RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l7
source("http://becker.phonologist.org/650/mer_utils.R")
kappa(l7) #34.35737
xMain$c.de2RT = xMain$de2RT - mean(xMain$de2RT)
xMain$c.de2RT
l7c.testing = lmer(c.de2RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l7c.testing
kappa(l7c.testing) #34.35737
l8 = lmer(NP2RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l8
l9 = lmer(EndRT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l9
l10 = lmer(AnswerRT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l10
l11 = lmer(LOCAL_ATT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l11
l12 = lmer(PRE_TAR ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l12
#Fully-crossed models:
l4f = lmer(de1RT ~ Demonstrative*Plausibility*Rctype + (1+Demonstrative*Plausibility*Rctype|Subjects)+(1+Demonstrative*Plausibility*Rctype|Items), xMain)
l4f
l6f = lmer(NP1RT ~ Demonstrative*Plausibility*Rctype + (1+Demonstrative*Plausibility*Rctype|Subjects)+(1+Demonstrative*Plausibility*Rctype|Items), xMain)
l6f
l7f = lmer(de2RT ~ Demonstrative*Plausibility*Rctype + (1+Demonstrative*Plausibility*Rctype|Subjects)+(1+Demonstrative*Plausibility*Rctype|Items), xMain)
l7f
l10f = lmer(AnswerRT ~ Demonstrative*Plausibility*Rctype + (1+Demonstrative*Plausibility*Rctype|Subjects)+(1+Demonstrative*Plausibility*Rctype|Items), xMain)
l10f

#Center factor levels
xMain$Demonstrative
xMain$c.Demonstrative = (xMain$Demonstrative == "DEM")-.5
xMain$c.Demonstrative

#ifelse(1:10==5, "oh no", "yay")
xMain$c.Plausibility = ifelse(xMain$Plausibility == "BIASLOW", -1/2, ifelse(xMain$Plausibility == "EQUI", 0, 1/2))
xMain$c.Plausibility

xMain$c.Rctype = (xMain$Rctype == "ACTIVE")-.5
xMain$c.Rctype
l1c = lmer(PassiveMarkerRT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l1c 
kappa.mer(l1c) #1.000127
max(vif.mer(l1c)) #1
l2c = lmer(Region1RT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l2c
l3c = lmer(Region2RT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l3c
l4c = lmer(de1RT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l4c
l5c = lmer(demRT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l5c
l6c = lmer(NP1RT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l6c
l7c = lmer(de2RT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l7c
l8c = lmer(NP2RT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l8c
l9c = lmer(EndRT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l9c
l10c = lmer(AnswerRT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l10c

l11c = lmer(LOCAL_ATT ~ c.Demonstrative*c.Plausibility*c.Rctype + (1|Subjects)+(1|Items), xMain)
l11c

NP1RT_BHA = subset(x, Rctype =="1" & Plausibility == "3")$NP1RT
mean(NP1RT_BHA) #387.3021
NP1RT_BLA = subset(x, Rctype =="1" & Plausibility == "1")$NP1RT
mean(NP1RT_BLA) #405.1319

NP1RT_BHP = subset(x, Rctype =="0" & Plausibility == "3")$NP1RT
mean(NP1RT_BHP) #409.3247
NP1RT_BLP = subset(x, Rctype =="0" & Plausibility == "1")$NP1RT
mean(NP1RT_BLP) #377.5139

NP1RT_BH = subset(x, Plausibility == "BIASHIGH")$NP1RT
mean(NP1RT_BH) 
NP1RT_BL = subset(x, Plausibility == "BIASLOW")$NP1RT
mean(NP1RT_BL) 

NP1RT_EQUI = subset(x, Plausibility == "EQUI")$NP1RT
NP1RT_EQUI
mean(NP1RT_EQUI)

LA_BH_A = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "ACTIVE")$LOCAL_ATT
LA_BH_P = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "PASSIVE")$LOCAL_ATT
LA_BL_A = subset(xMain, Plausibility == "BIASLOW" & Rctype == "ACTIVE")$LOCAL_ATT
LA_BL_P = subset(xMain, Plausibility == "BIASLOW" & Rctype == "PASSIVE")$LOCAL_ATT
LA_EQUI_A = subset(xMain, Plausibility == "EQUI" & Rctype == "ACTIVE")$LOCAL_ATT
LA_EQUI_P = subset(xMain, Plausibility == "EQUI" & Rctype == "PASSIVE")$LOCAL_ATT
t.test(LA_BH_A,LA_BH_P)
t.test(LA_BL_A,LA_BL_P)
tt.test(LA_EQUI_A,LA_EQUI_P)

RT_BL_A = subset(xMain, Plausibility == "BIASLOW" & Rctype == "ACTIVE")$NP1RT
RT_BL_P = subset(xMain, Plausibility == "BIASLOW" & Rctype == "PASSIVE")$NP1RT
RT_BH_A = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "ACTIVE")$NP1RT
RT_BH_P = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "PASSIVE")$NP1RT
RT_EQUI_A = subset(xMain, Plausibility == "EQUI" & Rctype == "ACTIVE")$NP1RT
RT_EQUI_P = subset(xMain, Plausibility == "EQUI" & Rctype == "PASSIVE")$NP1RT
RT_BH_P_D = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "PASSIVE" & Demonstrative == "DEM")$NP1RT
RT_BH_P_D
RT_BH_A_D = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "ACTIVE" & Demonstrative == "DEM")$NP1RT
RT_EQUI_P_D = subset(xMain, Plausibility == "EQUI" & Rctype == "PASSIVE" & Demonstrative == "DEM")$NP1RT
t.test(RT_BL_A, RT_BL_P)
t.test(RT_BH_A, RT_BH_P)
t.test(RT_EQUI_A, RT_EQUI_P)
t.test(RT_BL_P, RT_BH_P)
t.test(RT_BL_A,RT_BH_A)
t.test(RT_BL_P,RT_BH_)
t.test(RT_BH_P_D,RT_BH_A_D)
t.test(RT_BH_P_D,RT_EQUI_P_D)


RT_BH_A_Dem = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "ACTIVE"& Demonstrative == "DEM")$NP1RT
RT_BH_P_Dem = subset(xMain, Plausibility == "BIASHIGH" & Rctype == "PASSIVE"& Demonstrative == "DEM")$NP1RT
t.test(RT_BH_A_Dem,RT_BH_P_Dem)


RT_A_D = subset(xMain, Demonstrative == "DEM" & Rctype == "ACTIVE")$de2RT
RT_A_ND = subset(xMain, Demonstrative == "NODEM" & Rctype == "ACTIVE")$de2RT
RT_P_D = subset(xMain, Demonstrative == "DEM" & Rctype == "PASSIVE")$de2RT
RT_P_ND = subset(xMain, Demonstrative == "NODEM" & Rctype == "PASSIVE")$de2RT
t.test(RT_A_D,RT_A_ND)
t.test(RT_A_D,RT_P_D)
t.test(RT_P_D,RT_P_ND)
t.test(RT_P_ND,RT_A_ND)

RT_BL_D = subset(xMain, Demonstrative == "DEM" & Plausibility == "BIASLOW")$de2RT
RT_BL_ND = subset(xMain, Demonstrative == "NODEM" & Plausibility == "BIASLOW")$de2RT
RT_BH_D = subset(xMain, Demonstrative == "DEM" & Plausibility == "BIASHIGH")$de2RT
RT_BH_ND = subset(xMain, Demonstrative == "NODEM" & Plausibility == "BIASHIGH")$de2RT
RT_EQUI_D = subset(xMain, Demonstrative == "DEM" & Plausibility == "EQUI")$de2RT
RT_EQUI_ND = subset(xMain, Demonstrative == "NODEM" & Plausibility == "EQUI")$de2RT
#t.test (RT_BL_D,RT_BH_D)
#t.test (RT_BL_D,RT_EQUI_D)
#t.test (RT_BH_D,RT_EQUI_D)
#t.test (RT_BL_ND,RT_BH_ND) marginally 1.8
t.test (RT_BL_ND,RT_EQUI_ND)
#t.test (RT_BH_ND,RT_EQUI_ND)
t.test (RT_BL_D,RT_BL_ND)
#t.test (RT_BH_D,RT_BH_ND)
#t.test (RT_EQUI_D,RT_EQUI_ND)

l15 = lmer(RCvn ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l15

l16 = lmer(de1RT ~ Demonstrative*Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l16

RT_RCvn_A_B = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS")$RCvn
RT_RCvn_P_B = subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS")$RCvn
RT_RCvn_A_EQUI = subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI")$RCvn
RT_RCvn_P_EQUI = subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI")$RCvn
t.test(RT_RCvn_A_B, RT_RCvn_P_B)
t.test(RT_RCvn_A_B, RT_RCvn_A_EQUI)
t.test(RT_RCvn_A_B, RT_RCvn_P_EQUI)
t.test(RT_RCvn_P_B, RT_RCvn_A_EQUI)
t.test(RT_RCvn_P_B, RT_RCvn_P_EQUI)
t.test(RT_RCvn_A_EQUI, RT_RCvn_P_EQUI)

RT_de1_A_B = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS")$de1RT
RT_de1_P_B = subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS")$de1RT
RT_de1_A_EQUI = subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI")$de1RT
RT_de1_P_EQUI = subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI")$de1RT
t.test(RT_de1_A_B, RT_de1_P_B)
t.test(RT_de1_A_B, RT_de1_A_EQUI)
t.test(RT_de1_A_B, RT_de1_P_EQUI)
t.test(RT_de1_P_B, RT_de1_A_EQUI)
t.test(RT_de1_P_B, RT_de1_P_EQUI)
t.test(RT_de1_A_EQUI, RT_de1_P_EQUI)

#########################################

ARC_EQUI_INANIM_NP1=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP1RT
PRC_EQUI_INANIM_NP1=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP1RT
ARC_EQUI_ANIM_NP1=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "animate")$NP1RT
PRC_EQUI_ANIM_NP1=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "animate")$NP1RT

ARC_BIAS_INANIM_NP1 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$NP1RT
PRC_BIAS_INANIM_NP1 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$NP1RT
ARC_BIAS_ANIM_NP1 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "animate")$NP1RT
PRC_BIAS_ANIM_NP1 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "animate")$NP1RT

t.test (ARC_EQUI_INANIM_NP1, PRC_EQUI_INANIM_NP1)
t.test (ARC_EQUI_INANIM_NP1, ARC_EQUI_ANIM_NP1)
t.test (ARC_EQUI_INANIM_NP1, PRC_EQUI_ANIM_NP1)
t.test (ARC_EQUI_INANIM_NP1, ARC_BIAS_INANIM_NP1)
t.test (ARC_EQUI_INANIM_NP1, PRC_BIAS_INANIM_NP1)
t.test (ARC_EQUI_INANIM_NP1, ARC_BIAS_ANIM_NP1)
t.test (ARC_EQUI_INANIM_NP1, PRC_BIAS_ANIM_NP1)
t.test (PRC_EQUI_INANIM_NP1, ARC_EQUI_ANIM_NP1)
t.test (PRC_EQUI_INANIM_NP1, PRC_EQUI_ANIM_NP1)
t.test (PRC_EQUI_INANIM_NP1, ARC_BIAS_INANIM_NP1)
t.test (PRC_EQUI_INANIM_NP1, PRC_BIAS_INANIM_NP1)
t.test (PRC_EQUI_INANIM_NP1, ARC_BIAS_ANIM_NP1)
t.test (PRC_EQUI_INANIM_NP1, PRC_BIAS_ANIM_NP1)
t.test (ARC_EQUI_ANIM_NP1, PRC_EQUI_ANIM_NP1)
t.test (ARC_EQUI_ANIM_NP1, ARC_BIAS_INANIM_NP1)
t.test (ARC_EQUI_ANIM_NP1, PRC_BIAS_INANIM_NP1)
t.test (ARC_EQUI_ANIM_NP1, ARC_BIAS_ANIM_NP1)
t.test (ARC_EQUI_ANIM_NP1, PRC_BIAS_ANIM_NP1)
t.test(ARC_BIAS_INANIM_NP1, PRC_BIAS_INANIM_NP1)
t.test(ARC_BIAS_INANIM_NP1, ARC_BIAS_ANIM_NP1)
t.test(ARC_BIAS_INANIM_NP1, PRC_BIAS_ANIM_NP1)
t.test(PRC_BIAS_INANIM_NP1, ARC_BIAS_ANIM_NP1)
t.test(PRC_BIAS_INANIM_NP1, PRC_BIAS_ANIM_NP1)
t.test(ARC_BIAS_ANIM_NP1, PRC_BIAS_ANIM_NP1)

ARC_EQUI_INANIM_de1=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$de1RT
PRC_EQUI_INANIM_de1=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$de1RT
ARC_EQUI_ANIM_de1=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "animate")$de1RT
PRC_EQUI_ANIM_de1=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "animate")$de1RT

ARC_BIAS_INANIM_de1 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$de1RT
PRC_BIAS_INANIM_de1 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$de1RT
ARC_BIAS_ANIM_de1 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "animate")$de1RT
PRC_BIAS_ANIM_de1 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "animate")$de1RT

t.test (ARC_EQUI_INANIM_de1, PRC_EQUI_INANIM_de1)
t.test (ARC_EQUI_INANIM_de1, ARC_EQUI_ANIM_de1)
t.test (ARC_EQUI_INANIM_de1, PRC_EQUI_ANIM_de1)
t.test (ARC_EQUI_INANIM_de1, ARC_BIAS_INANIM_de1)
t.test (ARC_EQUI_INANIM_de1, PRC_BIAS_INANIM_de1)
t.test (ARC_EQUI_INANIM_de1, ARC_BIAS_ANIM_de1)
t.test (ARC_EQUI_INANIM_de1, PRC_BIAS_ANIM_de1)
t.test (PRC_EQUI_INANIM_de1, ARC_EQUI_ANIM_de1)
t.test (PRC_EQUI_INANIM_de1, PRC_EQUI_ANIM_de1)
t.test (PRC_EQUI_INANIM_de1, ARC_BIAS_INANIM_de1)
t.test (PRC_EQUI_INANIM_de1, PRC_BIAS_INANIM_de1)
t.test (PRC_EQUI_INANIM_de1, ARC_BIAS_ANIM_de1)
t.test (PRC_EQUI_INANIM_de1, PRC_BIAS_ANIM_de1)
t.test (ARC_EQUI_ANIM_de1, PRC_EQUI_ANIM_de1)
t.test (ARC_EQUI_ANIM_de1, ARC_BIAS_INANIM_de1)
t.test (ARC_EQUI_ANIM_de1, PRC_BIAS_INANIM_de1)
t.test (ARC_EQUI_ANIM_de1, ARC_BIAS_ANIM_de1)
t.test (ARC_EQUI_ANIM_de1, PRC_BIAS_ANIM_de1)
t.test(ARC_BIAS_INANIM_de1, PRC_BIAS_INANIM_de1)
t.test(ARC_BIAS_INANIM_de1, ARC_BIAS_ANIM_de1)
t.test(ARC_BIAS_INANIM_de1, PRC_BIAS_ANIM_de1)
t.test(PRC_BIAS_INANIM_de1, ARC_BIAS_ANIM_de1)
t.test(PRC_BIAS_INANIM_de1, PRC_BIAS_ANIM_de1)
t.test(ARC_BIAS_ANIM_de1, PRC_BIAS_ANIM_de1)

ARC_EQUI_INANIM_de2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$de2RT
PRC_EQUI_INANIM_de2=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$de2RT
ARC_EQUI_ANIM_de2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "animate")$de2RT
PRC_EQUI_ANIM_de2=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "animate")$de2RT

ARC_BIAS_INANIM_de2 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$de2RT
PRC_BIAS_INANIM_de2 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$de2RT
ARC_BIAS_ANIM_de2 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "animate")$de2RT
PRC_BIAS_ANIM_de2 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "animate")$de2RT

t.test (ARC_EQUI_INANIM_de2, PRC_EQUI_INANIM_de2)
t.test (ARC_EQUI_INANIM_de2, ARC_EQUI_ANIM_de2)
t.test (ARC_EQUI_INANIM_de2, PRC_EQUI_ANIM_de2)
t.test (ARC_EQUI_INANIM_de2, ARC_BIAS_INANIM_de2)
t.test (ARC_EQUI_INANIM_de2, PRC_BIAS_INANIM_de2)
t.test (ARC_EQUI_INANIM_de2, ARC_BIAS_ANIM_de2)
t.test (ARC_EQUI_INANIM_de2, PRC_BIAS_ANIM_de2)
t.test (PRC_EQUI_INANIM_de2, ARC_EQUI_ANIM_de2)
t.test (PRC_EQUI_INANIM_de2, PRC_EQUI_ANIM_de2)
t.test (PRC_EQUI_INANIM_de2, ARC_BIAS_INANIM_de2)
t.test (PRC_EQUI_INANIM_de2, PRC_BIAS_INANIM_de2)
t.test (PRC_EQUI_INANIM_de2, ARC_BIAS_ANIM_de2)
t.test (PRC_EQUI_INANIM_de2, PRC_BIAS_ANIM_de2)
t.test (ARC_EQUI_ANIM_de2, PRC_EQUI_ANIM_de2)
t.test (ARC_EQUI_ANIM_de2, ARC_BIAS_INANIM_de2)
t.test (ARC_EQUI_ANIM_de2, PRC_BIAS_INANIM_de2)
t.test (ARC_EQUI_ANIM_de2, ARC_BIAS_ANIM_de2)
t.test (ARC_EQUI_ANIM_de2, PRC_BIAS_ANIM_de2)
t.test(ARC_BIAS_INANIM_de2, PRC_BIAS_INANIM_de2)
t.test(ARC_BIAS_INANIM_de2, ARC_BIAS_ANIM_de2)
t.test(ARC_BIAS_INANIM_de2, PRC_BIAS_ANIM_de2)
t.test(PRC_BIAS_INANIM_de2, ARC_BIAS_ANIM_de2)
t.test(PRC_BIAS_INANIM_de2, PRC_BIAS_ANIM_de2)
t.test(ARC_BIAS_ANIM_de2, PRC_BIAS_ANIM_de2)

ARC_EQUI_INANIM_NP2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP2RT
PRC_EQUI_INANIM_NP2=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP2RT
ARC_EQUI_ANIM_NP2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "animate")$NP2RT
PRC_EQUI_ANIM_NP2=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "animate")$NP2RT

ARC_BIAS_INANIM_NP2 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$NP2RT
PRC_BIAS_INANIM_NP2 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$NP2RT
ARC_BIAS_ANIM_NP2 = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "animate")$NP2RT
PRC_BIAS_ANIM_NP2 =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "animate")$NP2RT

t.test (ARC_EQUI_INANIM_NP2, PRC_EQUI_INANIM_NP2)
t.test (ARC_EQUI_INANIM_NP2, ARC_EQUI_ANIM_NP2)
t.test (ARC_EQUI_INANIM_NP2, PRC_EQUI_ANIM_NP2)
t.test (ARC_EQUI_INANIM_NP2, ARC_BIAS_INANIM_NP2)
t.test (ARC_EQUI_INANIM_NP2, PRC_BIAS_INANIM_NP2)
t.test (ARC_EQUI_INANIM_NP2, ARC_BIAS_ANIM_NP2)
t.test (ARC_EQUI_INANIM_NP2, PRC_BIAS_ANIM_NP2)
t.test (PRC_EQUI_INANIM_NP2, ARC_EQUI_ANIM_NP2)
t.test (PRC_EQUI_INANIM_NP2, PRC_EQUI_ANIM_NP2)
t.test (PRC_EQUI_INANIM_NP2, ARC_BIAS_INANIM_NP2)
t.test (PRC_EQUI_INANIM_NP2, PRC_BIAS_INANIM_NP2)
t.test (PRC_EQUI_INANIM_NP2, ARC_BIAS_ANIM_NP2)
t.test (PRC_EQUI_INANIM_NP2, PRC_BIAS_ANIM_NP2)
t.test (ARC_EQUI_ANIM_NP2, PRC_EQUI_ANIM_NP2)
t.test (ARC_EQUI_ANIM_NP2, ARC_BIAS_INANIM_NP2)
t.test (ARC_EQUI_ANIM_NP2, PRC_BIAS_INANIM_NP2)
t.test (ARC_EQUI_ANIM_NP2, ARC_BIAS_ANIM_NP2)
t.test (ARC_EQUI_ANIM_NP2, PRC_BIAS_ANIM_NP2)
t.test(ARC_BIAS_INANIM_NP2, PRC_BIAS_INANIM_NP2)
t.test(ARC_BIAS_INANIM_NP2, ARC_BIAS_ANIM_NP2)
t.test(ARC_BIAS_INANIM_NP2, PRC_BIAS_ANIM_NP2)
t.test(PRC_BIAS_INANIM_NP2, ARC_BIAS_ANIM_NP2)
t.test(PRC_BIAS_INANIM_NP2, PRC_BIAS_ANIM_NP2)
t.test(ARC_BIAS_ANIM_NP2, PRC_BIAS_ANIM_NP2)

ARC_EQUI_INANIM_End=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$EndRT
PRC_EQUI_INANIM_End=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$EndRT
ARC_EQUI_ANIM_End=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "animate")$EndRT
PRC_EQUI_ANIM_End=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "animate")$EndRT

ARC_BIAS_INANIM_End = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$EndRT
PRC_BIAS_INANIM_End =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$EndRT
ARC_BIAS_ANIM_End = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "animate")$EndRT
PRC_BIAS_ANIM_End =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "animate")$EndRT

t.test (ARC_EQUI_INANIM_End, PRC_EQUI_INANIM_End)
t.test (ARC_EQUI_INANIM_End, ARC_EQUI_ANIM_End)
t.test (ARC_EQUI_INANIM_End, PRC_EQUI_ANIM_End)
t.test (ARC_EQUI_INANIM_End, ARC_BIAS_INANIM_End)
t.test (ARC_EQUI_INANIM_End, PRC_BIAS_INANIM_End)
t.test (ARC_EQUI_INANIM_End, ARC_BIAS_ANIM_End)
t.test (ARC_EQUI_INANIM_End, PRC_BIAS_ANIM_End)
t.test (PRC_EQUI_INANIM_End, ARC_EQUI_ANIM_End)
t.test (PRC_EQUI_INANIM_End, PRC_EQUI_ANIM_End)
t.test (PRC_EQUI_INANIM_End, ARC_BIAS_INANIM_End)
t.test (PRC_EQUI_INANIM_End, PRC_BIAS_INANIM_End)
t.test (PRC_EQUI_INANIM_End, ARC_BIAS_ANIM_End)
t.test (PRC_EQUI_INANIM_End, PRC_BIAS_ANIM_End)
t.test (ARC_EQUI_ANIM_End, PRC_EQUI_ANIM_End)
t.test (ARC_EQUI_ANIM_End, ARC_BIAS_INANIM_End)
t.test (ARC_EQUI_ANIM_End, PRC_BIAS_INANIM_End)
t.test (ARC_EQUI_ANIM_End, ARC_BIAS_ANIM_End)
t.test (ARC_EQUI_ANIM_End, PRC_BIAS_ANIM_End)
t.test(ARC_BIAS_INANIM_End, PRC_BIAS_INANIM_End)
t.test(ARC_BIAS_INANIM_End, ARC_BIAS_ANIM_End)
t.test(ARC_BIAS_INANIM_End, PRC_BIAS_ANIM_End)
t.test(PRC_BIAS_INANIM_End, ARC_BIAS_ANIM_End)
t.test(PRC_BIAS_INANIM_End, PRC_BIAS_ANIM_End)
t.test(ARC_BIAS_ANIM_End, PRC_BIAS_ANIM_End)


ARC_EQUI_INANIM_PRE_TAR=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$PRE_TAR
PRC_EQUI_INANIM_PRE_TAR=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$PRE_TAR
ARC_EQUI_ANIM_PRE_TAR=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "animate")$PRE_TAR
PRC_EQUI_ANIM_PRE_TAR=subset(xMain, Rctype == "PASSIVE" & Plausibility == "EQUI" & Animacy == "animate")$PRE_TAR

ARC_BIAS_INANIM_PRE_TAR = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$PRE_TAR
PRC_BIAS_INANIM_PRE_TAR =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "inanimate")$PRE_TAR
ARC_BIAS_ANIM_PRE_TAR = subset(xMain, Rctype == "ACTIVE" & Plausibility == "BIAS" & Animacy == "animate")$PRE_TAR
PRC_BIAS_ANIM_PRE_TAR =subset(xMain, Rctype == "PASSIVE" & Plausibility == "BIAS" & Animacy == "animate")$PRE_TAR

t.test (ARC_EQUI_INANIM_PRE_TAR, PRC_EQUI_INANIM_PRE_TAR)
t.test (ARC_EQUI_INANIM_PRE_TAR, ARC_EQUI_ANIM_PRE_TAR)
t.test (ARC_EQUI_INANIM_PRE_TAR, PRC_EQUI_ANIM_PRE_TAR)
t.test (ARC_EQUI_INANIM_PRE_TAR, ARC_BIAS_INANIM_PRE_TAR)
t.test (ARC_EQUI_INANIM_PRE_TAR, PRC_BIAS_INANIM_PRE_TAR)
t.test (ARC_EQUI_INANIM_PRE_TAR, ARC_BIAS_ANIM_PRE_TAR)
t.test (ARC_EQUI_INANIM_PRE_TAR, PRC_BIAS_ANIM_PRE_TAR)
t.test (PRC_EQUI_INANIM_PRE_TAR, ARC_EQUI_ANIM_PRE_TAR)
t.test (PRC_EQUI_INANIM_PRE_TAR, PRC_EQUI_ANIM_PRE_TAR)
t.test (PRC_EQUI_INANIM_PRE_TAR, ARC_BIAS_INANIM_PRE_TAR)
t.test (PRC_EQUI_INANIM_PRE_TAR, PRC_BIAS_INANIM_PRE_TAR)
t.test (PRC_EQUI_INANIM_PRE_TAR, ARC_BIAS_ANIM_PRE_TAR)
t.test (PRC_EQUI_INANIM_PRE_TAR, PRC_BIAS_ANIM_PRE_TAR)
t.test (ARC_EQUI_ANIM_PRE_TAR, PRC_EQUI_ANIM_PRE_TAR)
t.test (ARC_EQUI_ANIM_PRE_TAR, ARC_BIAS_INANIM_PRE_TAR)
t.test (ARC_EQUI_ANIM_PRE_TAR, PRC_BIAS_INANIM_PRE_TAR)
t.test (ARC_EQUI_ANIM_PRE_TAR, ARC_BIAS_ANIM_PRE_TAR)
t.test (ARC_EQUI_ANIM_PRE_TAR, PRC_BIAS_ANIM_PRE_TAR)
t.test(ARC_BIAS_INANIM_PRE_TAR, PRC_BIAS_INANIM_PRE_TAR)
t.test(ARC_BIAS_INANIM_PRE_TAR, ARC_BIAS_ANIM_PRE_TAR)
t.test(ARC_BIAS_INANIM_PRE_TAR, PRC_BIAS_ANIM_PRE_TAR)
t.test(PRC_BIAS_INANIM_PRE_TAR, ARC_BIAS_ANIM_PRE_TAR)
t.test(PRC_BIAS_INANIM_PRE_TAR, PRC_BIAS_ANIM_PRE_TAR)
t.test(ARC_BIAS_ANIM_PRE_TAR, PRC_BIAS_ANIM_PRE_TAR)

l12 = lmer(PRE_TAR ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l12
l6 = lmer(NP1RT ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l6
l7 = lmer(de2RT ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l7
l8 = lmer(NP2RT ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l8
l9 = lmer(EndRT ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l9
l10 = lmer(LOCAL_ATT ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l10
LOCAL_ATT

l11 = lmer(NP1de2 ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l11
l12 = lmer(de2NP2 ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l12
l13 = lmer(NP1de2NP2 ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l13

xMain = subset(x, MainFill == "MAIN")
xMainDem = subset(x, MainFill == "MAIN" & Demonstrative == "DEM")
xMainNoDem = subset(x, MainFill == "MAIN" & Demonstrative == "DEM")

l20 = lmer(PRE_TAR ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l20
l21 = lmer(NP1RT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l21
l22 = lmer(de2RT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l22
l23 = lmer(NP2RT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l23
l24 = lmer(EndRT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l24
l25 = lmer(LOCAL_ATT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l25
l26 = lmer(NP1de2 ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l26
l27 = lmer(de2NP2 ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l27
l28 = lmer(NP1de2NP2 ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)
l28

l30 = lmer(PRE_TAR ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l30
l31 = lmer(NP1RT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l31
l32 = lmer(de2RT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l32
l33 = lmer(NP2RT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l33
l34 = lmer(EndRT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l34
l35 = lmer(LOCAL_ATT ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l35
l36 = lmer(NP1de2 ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l36
l37 = lmer(de2NP2 ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l37
l38 = lmer(NP1de2NP2 ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainNoDem)
l38

xMainBias = subset(x, MainFill == "MAIN" & Plausibility == "BIAS")
xMainEQUI = subset(x, MainFill == "MAIN" & Plausibility == "EQUI")
l40 = lmer(PRE_TAR ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l40
l41 = lmer(NP1RT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l41
l42 = lmer(de2RT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l42
l43 = lmer(NP2RT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l43
l44 = lmer(EndRT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l44
l45 = lmer(LOCAL_ATT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l45
l46 = lmer(NP1de2 ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l46
l47 = lmer(de2NP2 ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l47
l48 = lmer(NP1de2NP2 ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainBias)
l48

l50 = lmer(PRE_TAR ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l50
l51 = lmer(NP1RT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l51
l52 = lmer(de2RT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l52
l53 = lmer(NP2RT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l53
l54 = lmer(EndRT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l54
l55 = lmer(LOCAL_ATT ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l55
l56 = lmer(NP1de2 ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l56
l57 = lmer(de2NP2 ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l57
l58 = lmer(NP1de2NP2 ~ Demonstrative*Rctype*Animacy + (1|Subjects)+(1|Items), xMainEQUI)
l58

l59 = lmer(RCwithDe ~ Plausibility*Rctype + (1|Subjects)+(1|Items), xMain)
l59

l60 = lmer(LOCAL_ATT ~ Demonstrative*Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMain)
l60

EQUI_ANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Animacy == "animate")$LOCAL_ATT
EQUI_INANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Animacy == "inanimate")$LOCAL_ATT
BIAS_ANI_JUDGEMENT =subset(xMain, Plausibility == "BIAS" & Animacy == "animate")$LOCAL_ATT
BIAS_INANI_JUDGEMENT =subset(xMain, Plausibility == "BIAS" & Animacy == "inanimate")$LOCAL_ATT
t.test(EQUI_ANI_JUDGEMENT, EQUI_INANI_JUDGEMENT)  
t.test(EQUI_ANI_JUDGEMENT, BIAS_ANI_JUDGEMENT)  
t.test(EQUI_ANI_JUDGEMENT, BIAS_INANI_JUDGEMENT)  
t.test(EQUI_INANI_JUDGEMENT, BIAS_ANI_JUDGEMENT) 
t.test(EQUI_INANI_JUDGEMENT, BIAS_INANI_JUDGEMENT) 
t.test(BIAS_ANI_JUDGEMENT, BIAS_INANI_JUDGEMENT)

ACTIVE_ANI_JUDGEMENT = subset(xMain, Rctype == "ACTIVE" & Animacy == "animate")$LOCAL_ATT
ACTIVE_INANI_JUDGEMENT = subset(xMain, Rctype == "ACTIVE" & Animacy == "inanimate")$LOCAL_ATT
PASSIVE_ANI_JUDGEMENT =subset(xMain, Rctype == "PASSIVE" & Animacy == "animate")$LOCAL_ATT
PASSIVE_INANI_JUDGEMENT =subset(xMain, Rctype == "PASSIVE" & Animacy == "inanimate")$LOCAL_ATT
t.test(ACTIVE_ANI_JUDGEMENT, ACTIVE_INANI_JUDGEMENT)  
t.test(ACTIVE_ANI_JUDGEMENT, PASSIVE_ANI_JUDGEMENT)  
t.test(ACTIVE_ANI_JUDGEMENT, PASSIVE_INANI_JUDGEMENT)  
t.test(ACTIVE_INANI_JUDGEMENT, PASSIVE_ANI_JUDGEMENT) 
t.test(ACTIVE_INANI_JUDGEMENT, PASSIVE_INANI_JUDGEMENT) 
t.test(PASSIVE_ANI_JUDGEMENT, PASSIVE_INANI_JUDGEMENT)

ACTIVE_BIAS_DEM_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "DEM" & Rctype == "ACTIVE")$LOCAL_ATT
ACTIVE_BIAS_NODEM_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "NODEM" & Rctype == "ACTIVE")$LOCAL_ATT
PASSIVE_BIAS_DEM_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "DEM" & Rctype == "PASSIVE")$LOCAL_ATT
PASSIVE_BIAS_NODEM_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "NODEM" & Rctype == "PASSIVE")$LOCAL_ATT
ACTIVE_EQUI_DEM_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "DEM" & Rctype == "ACTIVE")$LOCAL_ATT
ACTIVE_EQUI_NODEM_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "NODEM" & Rctype == "ACTIVE")$LOCAL_ATT
PASSIVE_EQUI_DEM_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "DEM" & Rctype == "PASSIVE")$LOCAL_ATT
PASSIVE_EQUI_NODEM_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "NODEM" & Rctype == "PASSIVE")$LOCAL_ATT
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, ACTIVE_BIAS_NODEM_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, PASSIVE_BIAS_DEM_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, PASSIVE_BIAS_NODEM_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, ACTIVE_EQUI_DEM_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, ACTIVE_EQUI_NODEM_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, PASSIVE_EQUI_DEM_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)

t.test(ACTIVE_BIAS_NODEM_JUDGEMENT, PASSIVE_BIAS_DEM_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_JUDGEMENT, PASSIVE_BIAS_NODEM_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_JUDGEMENT, ACTIVE_EQUI_DEM_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_JUDGEMENT, ACTIVE_EQUI_NODEM_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_JUDGEMENT, PASSIVE_EQUI_DEM_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)

t.test(PASSIVE_BIAS_DEM_JUDGEMENT, PASSIVE_BIAS_NODEM_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_JUDGEMENT, ACTIVE_EQUI_DEM_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_JUDGEMENT, ACTIVE_EQUI_NODEM_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_JUDGEMENT, PASSIVE_EQUI_DEM_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)

t.test(PASSIVE_BIAS_NODEM_JUDGEMENT, ACTIVE_EQUI_DEM_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_JUDGEMENT, ACTIVE_EQUI_NODEM_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_JUDGEMENT, PASSIVE_EQUI_DEM_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)

t.test(ACTIVE_EQUI_DEM_JUDGEMENT, ACTIVE_EQUI_NODEM_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_JUDGEMENT, PASSIVE_EQUI_DEM_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)

t.test(ACTIVE_EQUI_NODEM_JUDGEMENT, PASSIVE_EQUI_DEM_JUDGEMENT)
t.test(ACTIVE_EQUI_NODEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)

t.test(PASSIVE_EQUI_DEM_JUDGEMENT, PASSIVE_EQUI_NODEM_JUDGEMENT)


ACTIVE_BIAS_DEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "DEM" & Rctype == "ACTIVE" & Animacy == "animate")$LOCAL_ATT
ACTIVE_BIAS_DEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "DEM" & Rctype == "ACTIVE" & Animacy == "inanimate")$LOCAL_ATT
PASSIVE_BIAS_DEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "DEM" & Rctype == "PASSIVE" & Animacy == "animate")$LOCAL_ATT
PASSIVE_BIAS_DEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "DEM" & Rctype == "PASSIVE" & Animacy == "inanimate")$LOCAL_ATT
ACTIVE_BIAS_NODEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "NODEM" & Rctype == "ACTIVE" & Animacy == "animate")$LOCAL_ATT
ACTIVE_BIAS_NODEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "NODEM" & Rctype == "ACTIVE" & Animacy == "inanimate")$LOCAL_ATT
PASSIVE_BIAS_NODEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "NODEM" & Rctype == "PASSIVE" & Animacy == "animate")$LOCAL_ATT
PASSIVE_BIAS_NODEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "BIAS" & Demonstrative == "NODEM" & Rctype == "PASSIVE" & Animacy == "inanimate")$LOCAL_ATT
ACTIVE_EQUI_DEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "DEM" & Rctype == "ACTIVE" & Animacy == "animate")$LOCAL_ATT
ACTIVE_EQUI_DEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "DEM" & Rctype == "ACTIVE" & Animacy == "inanimate")$LOCAL_ATT
PASSIVE_EQUI_DEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "DEM" & Rctype == "PASSIVE" & Animacy == "animate")$LOCAL_ATT
PASSIVE_EQUI_DEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "DEM" & Rctype == "PASSIVE" & Animacy == "inanimate")$LOCAL_ATT
ACTIVE_EQUI_NODEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "NODEM" & Rctype == "ACTIVE" & Animacy == "animate")$LOCAL_ATT
ACTIVE_EQUI_NODEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "NODEM" & Rctype == "ACTIVE" & Animacy == "inanimate")$LOCAL_ATT
PASSIVE_EQUI_NODEM_ANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "NODEM" & Rctype == "PASSIVE" & Animacy == "animate")$LOCAL_ATT
PASSIVE_EQUI_NODEM_INANI_JUDGEMENT = subset(xMain, Plausibility == "EQUI" & Demonstrative == "NODEM" & Rctype == "PASSIVE" & Animacy == "inanimate")$LOCAL_ATT

t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_BIAS_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, ACTIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_BIAS_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(ACTIVE_EQUI_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_ANI_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_EQUI_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_EQUI_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_DEM_INANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_EQUI_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_INANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(PASSIVE_EQUI_DEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(ACTIVE_EQUI_NODEM_ANI_JUDGEMENT, ACTIVE_EQUI_NODEM_INANI_JUDGEMENT)
t.test(ACTIVE_EQUI_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_EQUI_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(ACTIVE_EQUI_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_ANI_JUDGEMENT)
t.test(ACTIVE_EQUI_NODEM_INANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

t.test(PASSIVE_EQUI_NODEM_ANI_JUDGEMENT, PASSIVE_EQUI_NODEM_INANI_JUDGEMENT)

####
t.test(PASSIVE_BIAS_DEM_INANI_JUDGEMENT, PASSIVE_BIAS_NODEM_INANI_JUDGEMENT)
####

EQUI_LOCAL_ATT = subset(xMain,Plausibility == "EQUI")$LOCAL_ATT
t.test(EQUI_LOCAL_ATT,mu=0.5135)
length(EQUI_LOCAL_ATT)
EQUI_NonLOCAL_ATT = subset(xMain,LOCAL_ATT == "0")$LOCAL_ATT
length(EQUI_NonLOCAL_ATT)

t.test(EQUI_LOCAL_ATT,EQUI_NonLOCAL_ATT)

######0817######
library(lme4.0)
x = read.csv("/Users/chongzhang/OneDrive/QualifyPaper/QP2/mrel.csv")
summary(x)
head(x)
l60 = lmer(ANIM_ATT ~ DEM*PLAUS*Rctype*ANIM_ORDER + (1|Subjects)+(1|Items), x)
l60
EQUI = subset(x, PLAUS == "EQUI")$ANIM_ATT
EQUI
t.test(EQUI, 1-EQUI)
BIAS= subset(x, PLAUS == "BIAS")$ANIM_ATT
t.test(BIAS, 1-BIAS)
EQUI_ANI = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI")$ANIM_ATT
EQUI_INANI = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "IA")$ANIM_ATT
BIAS_ANI = subset(x, PLAUS == "BIAS" & ANIM_ORDER == "AI")$ANIM_ATT
BIAS_INANI = subset(x, PLAUS == "BIAS" & ANIM_ORDER == "IA")$ANIM_ATT
t.test(EQUI_ANI, EQUI_INANI) #not 
t.test(BIAS_ANI, BIAS_INANI) #yes

ACTIVE_EQUI_ANI = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE")$ANIM_ATT
PASSIVE_EQUI_ANI = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "PASSIVE")$ANIM_ATT
t.test(ACTIVE_EQUI_ANI, PASSIVE_EQUI_ANI) #not

ACTIVE_EQUI_INANI = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "IA" & Rctype == "ACTIVE")$ANIM_ATT
PASSIVE_EQUI_INANI = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "IA" & Rctype == "PASSIVE")$ANIM_ATT
t.test(ACTIVE_EQUI_INANI, PASSIVE_EQUI_INANI) #not

ACTIVE_EQUI= subset(x, PLAUS == "EQUI"  & Rctype == "ACTIVE")$ANIM_ATT
PASSIVE_EQUI = subset(x, PLAUS == "EQUI"  & Rctype == "PASSIVE")$ANIM_ATT
t.test(ACTIVE_EQUI, PASSIVE_EQUI) #not

ANI_EQUI= subset(x, PLAUS == "EQUI"  & ANIM_ORDER == "AI")$ANIM_ATT
INANI_EQUI = subset(x, PLAUS == "EQUI"  & ANIM_ORDER == "IA")$ANIM_ATT
t.test(ANI_EQUI, INANI_EQUI) #not
#

ACTIVE_BIAS= subset(x, PLAUS == "BIAS"  & Rctype == "ACTIVE")$ANIM_ATT
PASSIVE_BIAS = subset(x, PLAUS == "BIAS"  & Rctype == "PASSIVE")$ANIM_ATT
t.test(ACTIVE_BIAS, PASSIVE_BIAS) #yes

ANI_BIAS= subset(x, PLAUS == "BIAS"  & ANIM_ORDER == "AI")$ANIM_ATT
INAI_BIAS = subset(x, PLAUS == "BIAS"  & ANIM_ORDER == "IA")$ANIM_ATT
t.test(ANI_BIAS, INAI_BIAS)#significant: main effects of local attachment preference

ACTIVE_BIAS_ANI = subset(x, PLAUS == "BIAS" & ANIM_ORDER == "AI" & Rctype == "ACTIVE")$ANIM_ATT
PASSIVE_BIAS_ANI = subset(x, PLAUS == "BIAS" & ANIM_ORDER == "AI" & Rctype == "PASSIVE")$ANIM_ATT
t.test(ACTIVE_BIAS_ANI, PASSIVE_BIAS_ANI) #yes

ACTIVE_BIAS_INANI = subset(x, PLAUS == "BIAS" & ANIM_ORDER == "IA" & Rctype == "ACTIVE")$ANIM_ATT
PASSIVE_BIAS_INANI = subset(x, PLAUS == "BIAS" & ANIM_ORDER == "IA" & Rctype == "PASSIVE")$ANIM_ATT
t.test(ACTIVE_BIAS_INANI, PASSIVE_BIAS_INANI) #yes
#active RC with a subject, more likely to integrate an animate thing. than passive RC.

ACTIVE_EQUI_ANI_DEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
ACTIVE_EQUI_ANI_NODEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
t.test(ACTIVE_EQUI_ANI_DEM, ACTIVE_EQUI_ANI_NODEM)#not
#the presence/absence of DEM does not make a difference.

ACTIVE_EQUI_ANI_NODEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
PASSIVE_EQUI_ANI_NODEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
t.test(ACTIVE_EQUI_ANI_NODEM, PASSIVE_EQUI_ANI_NODEM)#not

PASSIVE_EQUI_ANI_DEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "DEM")$ANIM_ATT
PASSIVE_EQUI_ANI_NODEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
t.test(PASSIVE_EQUI_ANI_DEM, PASSIVE_EQUI_ANI_NODEM)#not

ACTIVE_EQUI_ANI_DEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
ACTIVE_EQUI_ANI_NODEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
t.test(ACTIVE_EQUI_ANI_DEM, ACTIVE_EQUI_ANI_NODEM)#not

ACTIVE_EQUI_ANI_DEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
PASSIVE_EQUI_ANI_DEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "DEM")$ANIM_ATT
t.test(ACTIVE_EQUI_ANI_DEM, PASSIVE_EQUI_ANI_DEM)#yes
#only in ANI condition, passive is a strong cue.

EQUI = subset(x, PLAUS == "EQUI")
BIAS = subset(x, PLAUS == "BIAS")
l61 = lmer(ANIM_ATT ~ DEM*Rctype*ANIM_ORDER + (1|Subjects)+(1|Items), EQUI)
l61
l62 = lmer(ANIM_ATT ~ DEM*Rctype*ANIM_ORDER + (1|Subjects)+(1|Items), BIAS)
l62

ACTIVE_ANI_NODEM = subset(EQUI, ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
ACTIVE_ANI_DEM = subset(EQUI, ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
PASSIVE_ANI_NODEM = subset(EQUI, ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
PASSIVE_ANI_DEM = subset(EQUI, ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "DEM")$ANIM_ATT
ACTIVE_INANI_NODEM = subset(EQUI, ANIM_ORDER == "IA" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
ACTIVE_INANI_DEM = subset(EQUI, ANIM_ORDER == "IA" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
PASSIVE_INANI_NODEM = subset(EQUI, ANIM_ORDER == "IA" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
PASSIVE_INANI_DEM = subset(EQUI, ANIM_ORDER == "IA" & Rctype == "PASSIVE" & DEM == "DEM")$ANIM_ATT

ACTIVE_ANI_NODEM = subset(BIAS, ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
ACTIVE_ANI_DEM = subset(BIAS, ANIM_ORDER == "AI" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
PASSIVE_ANI_NODEM = subset(BIAS, ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
PASSIVE_ANI_DEM = subset(BIAS, ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "DEM")$ANIM_ATT
ACTIVE_INANI_NODEM = subset(BIAS, ANIM_ORDER == "IA" & Rctype == "ACTIVE" & DEM == "NODEM")$ANIM_ATT
ACTIVE_INANI_DEM = subset(BIAS, ANIM_ORDER == "IA" & Rctype == "ACTIVE" & DEM == "DEM")$ANIM_ATT
PASSIVE_INANI_NODEM = subset(BIAS, ANIM_ORDER == "IA" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
PASSIVE_INANI_DEM = subset(BIAS, ANIM_ORDER == "IA" & Rctype == "PASSIVE" & DEM == "DEM")$ANIM_ATT


PASSIVE_ANI_NODEM = subset(x, PLAUS == "EQUI" & ANIM_ORDER == "AI" & Rctype == "PASSIVE" & DEM == "NODEM")$ANIM_ATT
t.test(ACTIVE_EQUI_ANI_NODEM, PASSIVE_EQUI_ANI_NODEM)


ANI = subset(x, ANIM_ORDER == "AI")
INANI = subset(x, ANIM_ORDER == "IA")
l63 = lmer(ANIM_ATT ~ DEM*Rctype*PLAUS + (1|Subjects)+(1|Items), ANI)
l63
l64 = lmer(ANIM_ATT ~ DEM*Rctype*PLAUS + (1|Subjects)+(1|Items), INANI)
l64

ANI_EQUI = subset(x, ANIM_ORDER == "AI"&PLAUS == "EQUI")
ANI_BIAS = subset(x, ANIM_ORDER == "AI"&PLAUS == "BIAS")
INANI_EQUI = subset(x, ANIM_ORDER == "IA"&PLAUS == "EQUI")
INANI_BIAS = subset(x, ANIM_ORDER == "IA"&PLAUS == "BIAS")
l65 = lmer(ANIM_ATT ~ DEM*Rctype + (1|Subjects)+(1|Items), ANI_EQUI)
l65
l66 = lmer(ANIM_ATT ~ DEM*Rctype + (1|Subjects)+(1|Items), ANI_BIAS)
l66
l67 = lmer(ANIM_ATT ~ DEM*Rctype + (1|Subjects)+(1|Items), INANI_EQUI)
l67
l68 = lmer(ANIM_ATT ~ DEM*Rctype + (1|Subjects)+(1|Items), INANI_BIAS)
l68

ACTIVE_BIAS_DEM= subset(x, PLAUS == "BIAS"  & Rctype == "ACTIVE"&DEM == "DEM")$ANIM_ATT
ACTIVE_BIAS_NODEM = subset(x, PLAUS == "BIAS"  & Rctype == "ACTIVE"&DEM == "NODEM")$ANIM_ATT
t.test(ACTIVE_BIAS_DEM, ACTIVE_BIAS_NODEM) #no
PASSIVE_BIAS_DEM= subset(x, PLAUS == "BIAS"  & Rctype == "PASSIVE"&DEM == "DEM")$ANIM_ATT
PASSIVE_BIAS_NODEM = subset(x, PLAUS == "BIAS"  & Rctype == "PASSIVE"&DEM == "NODEM")$ANIM_ATT
t.test(PASSIVE_BIAS_DEM, PASSIVE_BIAS_NODEM) #not
ACTIVE_BIAS_NODEM= subset(x, PLAUS == "BIAS"  & Rctype == "ACTIVE"&DEM == "NODEM")$ANIM_ATT
PASSIVE_BIAS_NODEM = subset(x, PLAUS == "BIAS"  & Rctype == "PASSIVE"&DEM == "NODEM")$ANIM_ATT
t.test(ACTIVE_BIAS_NODEM, PASSIVE_BIAS_NODEM) #yes


