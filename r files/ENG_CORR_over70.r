library(lme4)
wholeENG_CORR_over70 = read.csv("/Users/chongzhang/Desktop/ENG_CORR_over70.csv")
summary(wholeENG_CORR_over70)

lR34 = lmer (log_R34 ~ RC1 * RC2 + (1|Participant)+(1|Item), wholeENG_CORR_over70)
summary(lR34)

RC1_S = subset(wholeENG_CORR_over70, RC1 == "S")$log_R34
RC1_O = subset(wholeENG_CORR_over70, RC1 == "O")$log_R34
t.test(RC1_S, RC1_O)# sig. t = -7.1187, df = 1433.1, p-value = 1.718e-12

SS_R78_rt = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R78
SO_R78_rt = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R78
OS_R78_rt = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R78
OO_R78_rt = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R78
t.test(SS_R78_rt, SO_R78_rt) #not sig.
t.test(OS_R78_rt, OO_R78_rt) #sig. t = 2.3917, df = 704.91, p-value = 0.01703
#OO >> SO >> SS >> OS
t.test(SS_R78_rt, OO_R78_rt) #sig. t = 2.3511, df = 717.53, p-value = 0.01899
t.test(SO_R78_rt, OO_R78_rt) #not sig
# OO *>> SS
t.test(SO_R78_rt, OS_R78_rt) #not sig
t.test(SS_R78_rt, OS_R78_rt) #not sig

#compare SS+OS and OO+SO in RC2:
lR78 = lmer (log_R78 ~ RC1 * RC2 + (1|Participant)+(1|Item), wholeENG_CORR_over70)
summary(lR78)

lR78_onRC1 = lmer (log_R78 ~ log_R34 * RC1 * RC2 + (1+log_R34|Participant)+(1+log_R34|Item), wholeENG_CORR_over70)
summary(lR78_onRC1)

RC2_S = subset(wholeENG_CORR_over70, RC2 == "S")$log_R78
RC2_O = subset(wholeENG_CORR_over70, RC2 == "O")$log_R78
t.test(RC2_S, RC2_O) #t = 2.3727, df = 1427.5, p-value = 0.01779; 3.198157  3.165546 

#RC1+RC2, i.e. R3478
lR3478 = lmer (log_R3478 ~ RC1 * RC2 + (1|Participant)+(1|Item), wholeENG_CORR_over70)
summary(lR3478)
RCs_SS = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "S")$log_R3478
RCs_SO = subset(wholeENG_CORR_over70, RC1 == "S" & RC2 == "O")$log_R3478
RCs_OS = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "S")$log_R3478
RCs_OO = subset(wholeENG_CORR_over70, RC1 == "O" & RC2 == "O")$log_R3478
t.test(RCs_SS, RCs_SO) #not sig.
t.test(RCs_OS, RCs_OO) #not sig.
t.test(RCs_SS, RCs_OO) #t = -2.7344, df = 712.88, p-value = 0.006405

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

lR78 = lmer (log_R78 ~ log_R34 * RC1 * RC2 + (1+log_R34|Participant)+(1+log_R34|Item), wholeENG_CORR_over70)
summary(lR78)

lR78withoutR34 = lmer (log_R78 ~ RC1 * RC2 + (1+log_R34|Participant)+(1+log_R34|Item), wholeENG_CORR_over70)
summary(lR78withoutR34)

