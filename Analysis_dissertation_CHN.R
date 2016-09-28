library(lme4)
whole = read.csv("/Users/chongzhang/Desktop/CHN.csv")
summary(whole)
head(whole)
SS = subset(whole, RCtype == "SS")
SS
SO = subset(whole, RCtype == "SO")
OS = subset(whole, RCtype == "OS")
OO = subset(whole, RCtype == "OO")

#ARC_EQUI_INANIM_NP2=subset(xMain, Rctype == "ACTIVE" & Plausibility == "EQUI" & Animacy == "inanimate")$NP2RT
#l20 = lmer(PRE_TAR ~ Plausibility*Rctype*Animacy + (1|Subjects)+(1|Items), xMainDem)

lR11 = lmer (log_R11 ~ RCtype*Matching + (1|Participant)+(1|Item), whole)
summary(lR11)

#Verb in RC1, SS+SO compared with OS+OO
S_V_in_RC1 = subset(whole, RC1 == "S")$log_R3
O_V_in_RC1 = subset(whole, RC1 == "O")$log_R4
t.test(S_V_in_RC1, O_V_in_RC1) #not significant
#Verb in RC1, SS compared with OO
SS_V_in_RC1 = subset(whole, RC1 == "S" & RC2 == "S")$log_R3
OO_V_in_RC1 = subset(whole, RC1 == "O" & RC2 == "O")$log_R4
t.test(SS_V_in_RC1, OO_V_in_RC1) #not significant
#Verb in RC1, SO compared with OS
SO_V_in_RC1 = subset(whole, RC1 == "S" & RC2 == "O")$log_R3
OS_V_in_RC1 = subset(whole, RC1 == "O" & RC2 == "S")$log_R4
t.test(SO_V_in_RC1, OS_V_in_RC1) #not significant
#Verb in RC1, SS compared with OS
SS_V_in_RC1 = subset(whole, RC1 == "S" & RC2 == "S")$log_R3
OS_V_in_RC1 = subset(whole, RC1 == "O" & RC2 == "S")$log_R4
t.test(SS_V_in_RC1, OS_V_in_RC1) #not significant
#Verb in RC1, SO compared with OO
SO_V_in_RC1 = subset(whole, RC1 == "S" & RC2 == "O")$log_R3
OO_V_in_RC1 = subset(whole, RC1 == "O" & RC2 == "O")$log_R4
t.test(SO_V_in_RC1, OO_V_in_RC1) #not significant

#-------------------------------------------------------------#
#Verb in RC2, SS+OS compared with SO+OO
S_V_in_RC2 = subset(whole, RC2 == "S")$log_R8
O_V_in_RC2 = subset(whole, RC2 == "O")$log_R9
t.test(S_V_in_RC2, O_V_in_RC2) #significant
#Verb in RC2, SS compared with OO
SS_V_in_RC2 = subset(whole, RC1 == "S" & RC2 == "S")$log_R8
OO_V_in_RC2 = subset(whole, RC1 == "O" & RC2 == "O")$log_R9
t.test(SS_V_in_RC2, OO_V_in_RC2) #significant
#Verb in RC2, SO compared with OS
SO_V_in_RC2 = subset(whole, RC1 == "S" & RC2 == "O")$log_R8
OS_V_in_RC2 = subset(whole, RC1 == "O" & RC2 == "S")$log_R9
t.test(SO_V_in_RC2, OS_V_in_RC2) #significant, but smaller
#Verb in RC2,  OS compared with OO
OS_V_in_RC2 = subset(whole, RC1 == "O" & RC2 == "S")$log_R8
OO_V_in_RC2 = subset(whole, RC1 == "O" & RC2 == "O")$log_R9
t.test(OS_V_in_RC2, OO_V_in_RC2) #not significant
#Verb in RC2,  SS compared with SO
SS_V_in_RC2 = subset(whole, RC1 == "S" & RC2 == "S")$log_R8
SO_V_in_RC2 = subset(whole, RC1 == "S" & RC2 == "O")$log_R9
t.test(SS_V_in_RC2, SO_V_in_RC2) #significant
#Verb in RC2,  SO compared with OO
SO_V_in_RC2 = subset(whole, RC1 == "S" & RC2 == "O")$log_R8
OO_V_in_RC2 = subset(whole, RC1 == "O" & RC2 == "O")$log_R9
t.test(SO_V_in_RC2, OO_V_in_RC2) #significant
#------important------
#Verb in RC1: nothing is significant
#Verb in RC2: everything is significant, except for OS vs. OO.
#------important------

#Edge of RC1, S compared with O
S_edge_RC1_PP = subset(whole, RC1 == "S")$log_R2
O_edge_RC1_PP = subset(whole, RC1 == "O")$log_R2
t.test(S_edge_RC1_PP, O_edge_RC1_PP) #not significant
#Edge of RC1, S compared with O
S_edge_RC1_noPP = subset(whole, RC1 == "S")$log_R3
O_edge_RC1_noPP = subset(whole, RC1 == "O")$log_R3
t.test(S_edge_RC1_noPP, O_edge_RC1_noPP) #significant
#Edge of RC1, SS compared with OS
SS_edge_RC1_noPP = subset(whole, RC1 == "S" & RC2 == "S")$log_R3
OS_edge_RC1_noPP = subset(whole, RC1 == "O" & RC2 == "S")$log_R3
t.test(SS_edge_RC1_noPP, OS_edge_RC1_noPP) #significant
#Edge of RC1, SS compared with OO
SS_edge_RC1_noPP = subset(whole, RC1 == "S" & RC2 == "S")$log_R3
OO_edge_RC1_noPP = subset(whole, RC1 == "O" & RC2 == "O")$log_R3
t.test(SS_edge_RC1_noPP, OO_edge_RC1_noPP) #significant
#Edge of RC1, SO compared with OS
SO_edge_RC1_noPP = subset(whole, RC1 == "S" & RC2 == "O")$log_R3
OS_edge_RC1_noPP = subset(whole, RC1 == "O" & RC2 == "S")$log_R3
t.test(SO_edge_RC1_noPP, OS_edge_RC1_noPP) #small significant
#Edge of RC1, SO compared with OO
SO_edge_RC1_noPP = subset(whole, RC1 == "S" & RC2 == "O")$log_R3
OO_edge_RC1_noPP = subset(whole, RC1 == "O" & RC2 == "O")$log_R3
t.test(SO_edge_RC1_noPP, OO_edge_RC1_noPP) #small significant

#Edge of RC2, S compared with O
S_edge_RC2_PP = subset(whole, RC2 == "S")$log_R7
O_edge_RC2_PP = subset(whole, RC2 == "O")$log_R7
t.test(S_edge_RC2_PP, O_edge_RC2_PP) #not significant
#Edge of RC2, S compared with O
S_edge_RC2_noPP = subset(whole, RC2 == "S")$log_R8
O_edge_RC2_noPP = subset(whole, RC2 == "O")$log_R8
t.test(S_edge_RC2_noPP, O_edge_RC2_noPP) #significant
#Edge of RC2, SS compared with SO
SS_edge_RC2_noPP = subset(whole, RC1 == "S" & RC2 == "S")$log_R8
SO_edge_RC2_noPP = subset(whole, RC1 == "S" & RC2 == "O")$log_R8
t.test(SS_edge_RC2_noPP, SO_edge_RC2_noPP)  #significant
#Edge of RC2, SS compared with OO
SS_edge_RC2_noPP = subset(whole, RC1 == "S" & RC2 == "S")$log_R8
OO_edge_RC2_noPP = subset(whole, RC1 == "O" & RC2 == "O")$log_R8
t.test(SS_edge_RC2_noPP, OO_edge_RC2_noPP)  #significant
#Edge of RC2, SO compared with OS
SO_edge_RC2_noPP = subset(whole, RC1 == "S" & RC2 == "O")$log_R8
OS_edge_RC2_noPP = subset(whole, RC1 == "O" & RC2 == "S")$log_R8
t.test(SO_edge_RC2_noPP, OS_edge_RC2_noPP)  #significant
#Edge of RC2, OS compared with OO
OS_edge_RC2_noPP = subset(whole, RC1 == "O" & RC2 == "S")$log_R8
OO_edge_RC2_noPP = subset(whole, RC1 == "O" & RC2 == "O")$log_R8
t.test(OS_edge_RC2_noPP, OO_edge_RC2_noPP)  #significant
#Edge of RC2, SS compared with OS
SS_edge_RC2_noPP = subset(whole, RC1 == "S" & RC2 == "S")$log_R8
OS_edge_RC2_noPP = subset(whole, RC1 == "O" & RC2 == "S")$log_R8
t.test(SS_edge_RC2_noPP, OS_edge_RC2_noPP)  #small significant
#------important------
#Edge of RC1: S is faster than O.
#Edge of RC2: S is faster than O.
#------important------

#head NP
SS_head = subset(whole, RC1 == "S" & RC2 == "S")$log_R11
SO_head = subset(whole, RC1 == "S" & RC2 == "O")$log_R11
OS_head = subset(whole, RC1 == "O" & RC2 == "S")$log_R11
OO_head = subset(whole, RC1 == "O" & RC2 == "O")$log_R11
S_head_RC1 = subset(whole, RC1 == "S")$log_R11
O_head_RC1 = subset(whole, RC1 == "O")$log_R11
S_head_RC2 = subset(whole, RC2 == "S")$log_R11
O_head_RC2 = subset(whole, RC2 == "O")$log_R11
#-------important--------
t.test(SS_head, SO_head) #significant
t.test(SS_head, OS_head) #significant
t.test(SS_head, OO_head) #not significant
t.test(SO_head, OS_head) #not significant
t.test(SO_head, OO_head) #significant
t.test(OS_head, OO_head) #marginally significant
#it looks like parallelism wins 
#-------important--------

#difference in heads when first RC is S or O, regardless of the second RC.
t.test(S_head_RC1, O_head_RC1) #not significant
#difference in heads when second RC is S or O, regardless of the first RC.
t.test(S_head_RC2, O_head_RC2) #not significant
#this makes sense coz we need the combination of two RCs.
