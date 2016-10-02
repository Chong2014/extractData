library(lme4)
wholeCHN_CORR_over70 = read.csv("/Users/chongzhang/Onedrive/Data-Analyses-R-Python-mySQL/csv files/CHN_CORR_over70.csv")
summary(wholeCHN_CORR_over70)

lR34 = lmer (log_R34 ~ RC1 * RC2 + (1|Participant)+(1|Item), wholeCHN_CORR_over70)
summary(lR34)

RC1_S = subset(wholeCHN_CORR_over70, RC1 == "S")$log_R34
RC1_O = subset(wholeCHN_CORR_over70, RC1 == "O")$log_R34
t.test(RC1_S, RC1_O)# t = 4.0657, df = 1504.9, p-value = 5.036e-05

#RC2 on four levels:
SS_R89_rt = subset(wholeCHN_CORR_over70, RC1 == "S" & RC2 == "S")$log_R89
SO_R89_rt = subset(wholeCHN_CORR_over70, RC1 == "S" & RC2 == "O")$log_R89
OS_R89_rt = subset(wholeCHN_CORR_over70, RC1 == "O" & RC2 == "S")$log_R89
OO_R89_rt = subset(wholeCHN_CORR_over70, RC1 == "O" & RC2 == "O")$log_R89
t.test(SS_R89_rt, SO_R89_rt) #sig. t = -3.531, df = 777.91, p-value = 0.0004384; 3.166057  3.229264
t.test(OS_R89_rt, OO_R89_rt) #not sig. ;3.245063  3.216498 
#SS *>> OO >> SO >> OS
t.test(OS_R89_rt, SO_R89_rt) #not sig.
t.test(SS_R89_rt, OO_R89_rt) #sig. t = -2.7894, df = 777.93, p-value = 0.00541
t.test(SO_R89_rt, OO_R89_rt) #not sig.
 
#compare SS+OS and OO+SO in RC2:
lR89 = lmer (log_R89 ~ RC1 * RC2 + (1|Participant)+(1|Item), wholeCHN_CORR_over70)
summary(lR89)

lR89_onRC1 = lmer (log_R89 ~ log_R34 * RC1 * RC2 + (1+log_R34|Participant)+(1+log_R34|Item), wholeCHN_CORR_over70)
summary(lR89_onRC1)

RC2_S = subset(wholeCHN_CORR_over70, RC2 == "S")$log_R89
RC2_O = subset(wholeCHN_CORR_over70, RC2 == "O")$log_R89
t.test(RC2_S, RC2_O) #not sig. t = -1.2931, df = 1542.9, p-value = 0.1962 

#RC1+RC2, i.e. R3489
lR3489 = lmer (log_R3478 ~ RC1 * RC2 + (1|Participant)+(1|Item), wholeCHN_CORR_over70)
summary(lR3489)
RCs_SS = subset(wholeCHN_CORR_over70, RC1 == "S" & RC2 == "S")$log_R3489
RCs_SO = subset(wholeCHN_CORR_over70, RC1 == "S" & RC2 == "O")$log_R3489
RCs_OS = subset(wholeCHN_CORR_over70, RC1 == "O" & RC2 == "S")$log_R3489
RCs_OO = subset(wholeCHN_CORR_over70, RC1 == "O" & RC2 == "O")$log_R3489
t.test(RCs_SS, RCs_SO) #sig. t = -2.309, df = 773.29, p-value = 0.02121; 3.490907  3.525842 
t.test(RCs_OS, RCs_OO) #not sig.;3.514294  3.491944 
#SS >> OO >> OS >> SO
t.test(RCs_SS, RCs_OO) #not sig.
t.test(RCs_OS, RCs_SO) #not sig.
t.test(RCs_SS, RCs_OS) #not sig.
t.test(RCs_SO, RCs_OO) #sig. t = 2.3459, df = 777.91, p-value = 0.01923

#Verb in RC1:
RC1_verb_S = subset(wholeENG_CORR_over70, RC1 == "S")$log_R3
RC1_verb_O = subset(wholeENG_CORR_over70, RC1 == "O")$log_R4
t.test(RC1_verb_S, RC1_verb_O) #t = -17.941, df = 1257.4, p-value < 2.2e-16
RC1_verb_SS = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R3
RC1_verb_SO = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R3
RC1_verb_OS = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R4
RC1_verb_OO = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R4
t.test(RC1_verb_SS, RC1_verb_SO) #not sig.
t.test(RC1_verb_OS, RC1_verb_OO) #not sig.
t.test(RC1_verb_SO, RC1_verb_OO) #t = -11.609, df = 643.49, p-value < 2.2e-16

#Verb in RC2:
RC2_verb_S = subset(wholeENG_CORR_over70, RC2 == "S")$log_R7
RC2_verb_O = subset(wholeENG_CORR_over70, RC2 == "O")$log_R8
t.test(RC2_verb_S, RC2_verb_O) #t = 3.6663, df = 1406.1, p-value = 0.0002552
RC2_verb_SS = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R7
RC2_verb_SO = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R8
RC2_verb_OS = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R7
RC2_verb_OO = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R8
t.test(RC2_verb_SS, RC2_verb_SO) #t = 3.0541, df = 706.83, p-value = 0.002342
t.test(RC2_verb_OS, RC2_verb_OO) #t = 2.1477, df = 697.42, p-value = 0.03208
t.test(RC2_verb_SO, RC2_verb_OO) #not sig
t.test(RC2_verb_SO, RC2_verb_OS) #not sig
t.test(RC2_verb_SS, RC2_verb_OS) #marginal t = 1.8623, df = 716.66, p-value = 0.06297

#edge of RC1
RC1_edge_S = subset(wholeENG_CORR_over70, RC1 == "S")$log_R3
RC1_edge_O = subset(wholeENG_CORR_over70, RC1 == "O")$log_R3
t.test(RC1_edge_S, RC1_edge_O) #t = -3.9891, df = 1409.7, p-value = 6.973e-05
RC1_edge_SS = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R3
RC1_edge_SO = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R3
RC1_edge_OS = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R3
RC1_edge_OO = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R3
t.test(RC1_edge_SS, RC1_edge_SO) #not sig.
t.test(RC1_edge_OS, RC1_edge_OO) #not sig.
t.test(RC1_edge_SO, RC1_edge_OO) #not sig.
#edge of RC2
RC2_edge_S = subset(wholeENG_CORR_over70, RC2 == "S")$log_R7
RC2_edge_O = subset(wholeENG_CORR_over70, RC2 == "O")$log_R7
t.test(RC2_edge_S, RC2_edge_O) #not significant, t = -0.31733, df = 1426.1, p-value = 0.751
RC2_edge_SS = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R7
RC2_edge_SO = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R7
RC2_edge_OS = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R7
RC2_edge_OO = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R7
t.test(RC2_edge_SS, RC2_edge_SO) #not sig.
t.test(RC2_edge_OS, RC2_edge_OO) #not sig.
t.test(RC2_edge_SS, RC2_edge_OS) #not sig.

#main verb
main_verb_SS = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R10
main_verb_SO = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R10
main_verb_OS = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R10
main_verb_OO = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R10
t.test(main_verb_SS, main_verb_SO) #not sig.
t.test(main_verb_OS, main_verb_OO) #not sig.
#OS >> SS >> OO >> SO
t.test(main_verb_OS, main_verb_SO) #t = -2.1665, df = 674.05, p-value = 0.03062


