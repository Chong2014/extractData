library(lme4)
wholeENG = read.csv("/Users/chongzhang/Desktop/ENG.csv")
summary(wholeENG)
head(wholeENG)
SS = subset(wholeENG, RCtype == "SS")
SS
SO = subset(wholeENG, RCtype == "SO")
OS = subset(wholeENG, RCtype == "OS")
OO = subset(wholeENG, RCtype == "OO")

#ARC_EQUI_INANIM_NP2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP2RT
#l20 = lmer(PRE_TAR ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)

lR3 = lmer (log_R3 ~ RCtype*Matching + (1|Participant)+(1|Item), wholeENG)
lR3

#V1 in RC1, S vs. O
S_V_in_RC1 = subset(wholeENG, RC1 == "S")$log_R3
O_V_in_RC1 = subset(wholeENG, RC1 == "O")$log_R4
t.test(S_V_in_RC1, O_V_in_RC1) #significant

#V2 in RC2, S vs. O
S_V_in_RC2 = subset(wholeENG, RC2 == "S")$log_R7
O_V_in_RC2 = subset(wholeENG, RC2 == "O")$log_R8
t.test(S_V_in_RC2, O_V_in_RC2) #significant

#V1 in RC1
SS_V_in_RC1 = subset(wholeENG, RC1 == "S" & RC2 == "S")$log_R3
SO_V_in_RC1 = subset(wholeENG, RC1 == "S" & RC2 == "O")$log_R4
OS_V_in_RC1 = subset(wholeENG, RC1 == "O" & RC2 == "S")$log_R3
OO_V_in_RC1 = subset(wholeENG, RC1 == "O" & RC2 == "O")$log_R4
t.test(SS_V_in_RC1, SO_V_in_RC1) #significant
t.test(OS_V_in_RC1, OO_V_in_RC1) #significant
t.test(SO_V_in_RC1, OS_V_in_RC1) #significant
t.test(SS_V_in_RC1, OS_V_in_RC1) #significant
t.test(SO_V_in_RC1, OO_V_in_RC1) #significant

#V2 in RC2
SS_V_in_RC2 = subset(wholeENG, RC1 == "S" & RC2 == "S")$log_R7
SO_V_in_RC2 = subset(wholeENG, RC1 == "S" & RC2 == "O")$log_R8
OS_V_in_RC2 = subset(wholeENG, RC1 == "O" & RC2 == "S")$log_R7
OO_V_in_RC2 = subset(wholeENG, RC1 == "O" & RC2 == "O")$log_R8
t.test(SS_V_in_RC2, SO_V_in_RC2) #significant
t.test(OS_V_in_RC2, OO_V_in_RC2) #not significant
t.test(SO_V_in_RC2, OS_V_in_RC2) #not significant
t.test(SS_V_in_RC2, OS_V_in_RC2) #not significant
t.test(SO_V_in_RC2, OO_V_in_RC2) #not significant


#Edge of RC2
SS_edge_RC2 = subset(wholeENG, RC1 == "S" & RC2 == "S")$log_R6
SO_edge_RC2 = subset(wholeENG, RC1 == "S" & RC2 == "O")$log_R6
OS_edge_RC2 = subset(wholeENG, RC1 == "O" & RC2 == "S")$log_R6
OO_edge_RC2 = subset(wholeENG, RC1 == "O" & RC2 == "O")$log_R6
t.test(SS_edge_RC2, SO_edge_RC2) #not significant
t.test(OS_edge_RC2, OO_edge_RC2) #not significant
t.test(SO_edge_RC2, OS_edge_RC2) #not significant
t.test(SS_edge_RC2, OO_edge_RC2) #not significant

#Main Verb
SS_main_verb = subset(wholeENG, RC1 == "S" & RC2 == "S")$log_R10
SO_main_verb = subset(wholeENG, RC1 == "S" & RC2 == "O")$log_R10
OS_main_verb = subset(wholeENG, RC1 == "O" & RC2 == "S")$log_R10
OO_main_verb = subset(wholeENG, RC1 == "O" & RC2 == "O")$log_R10
t.test(SS_main_verb, SO_main_verb) #not significant
t.test(SO_main_verb, OS_main_verb) #significant
t.test(OS_main_verb, OO_main_verb) #not significant
t.test(SS_main_verb, OO_main_verb) #not significant
t.test(SS_main_verb, OS_main_verb) #not significant
t.test(SO_main_verb, OO_main_verb) #not significant
