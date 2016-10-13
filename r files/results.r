#English
#Region 7 and Region 8 (RC2)
#---------------------------------------------------------------------------------------------------------#
summary(SS) #log_R78: mean = 3.150
summary(SO) #log_R78: mean = 3.133
summary(OS) #log_R78: mean = 3.154
summary(OO) #log_R78: mean = 3.120 
#OO >> SO >> SS >> OS
# OO
m.1 = lmer(log_R78 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.1)
"""Fixed effects:
Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)   3.14345    0.01780  48.05000 176.636   <2e-16 ***
RCtypeOS      0.02444    0.01457 151.15000   1.678   0.0954 .  
RCtypeSO      0.01891    0.01418 176.76000   1.333   0.1842    
RCtypeSS      0.02978    0.01416 174.54000   2.102   0.0369 * """

#relevel: 0S
wholeENG$RCtypeReleveled = relevel(wholeENG$RCtype, "OS")
m.2 = lmer(log_R78 ~ RCtypeReleveled + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.2)
"""Fixed effects:
Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)         3.167895   0.017856  48.240000 177.417   <2e-16 ***
RCtypeReleveledOO  -0.024444   0.014567 151.160000  -1.678   0.0954 .  
RCtypeReleveledSO  -0.005536   0.014298 173.080000  -0.387   0.6991    
RCtypeReleveledSS   0.005336   0.014282 173.060000   0.374   0.7092 """

#relevel: SO
wholeENG$RCtypeReleveled = relevel(wholeENG$RCtype, "SO")
m.3 = lmer(log_R78 ~ RCtypeReleveled + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.3)
"""Fixed effects:
  Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)         3.162358   0.017768  47.960000 177.982   <2e-16 ***
  RCtypeReleveledOO  -0.018908   0.014183 176.770000  -1.333    0.184    
RCtypeReleveledOS   0.005536   0.014298 173.080000   0.387    0.699    
RCtypeReleveledSS   0.010872   0.013688 185.960000   0.794    0.428 """

#relevel: SS
wholeENG$RCtypeReleveled = relevel(wholeENG$RCtype, "SS")
m.4 = lmer(log_R78 ~ RCtypeReleveled + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.4)
"""
Fixed effects:
                    Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)         3.173230   0.017767  48.380000 178.603   <2e-16 ***
RCtypeReleveledOO  -0.029780   0.014164 174.540000  -2.102   0.0369 *  
RCtypeReleveledOS  -0.005336   0.014282 173.060000  -0.374   0.7092    
RCtypeReleveledSO  -0.010872   0.013688 185.960000  -0.794   0.4281 """
#---------------------------------------------------------------------------------------------------------#

#Verb in RC2
#OO
m.1 = lmer(log_R8 ~ RCtype + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.1)
"""Fixed effects:
             Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)   2.75757    0.01576  32.11000 174.930  < 2e-16 ***
RCtypeOS      0.06847    0.01648 154.02000   4.156 5.35e-05 ***
RCtypeSO      0.01177    0.01622 170.08000   0.725   0.4692    
RCtypeSS      0.04141    0.01620 169.11000   2.555   0.0115 * """

#relevel: OS
wholeENG$RCtypeReleveled = relevel(wholeENG$RCtype, "OS")
m.2 = lmer(log_R7 ~ RCtypeReleveled + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.2)
"""
#relevel: SO
wholeENG$RCtypeReleveled = relevel(wholeENG$RCtype, "SO")
m.3 = lmer(log_R8 ~ RCtypeReleveled + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.3)
#relevel: SS
wholeENG$RCtypeReleveled = relevel(wholeENG$RCtype, "SS")
m.4 = lmer(log_R7 ~ RCtypeReleveled + (1+log_R34+dprimeT|Participant)+(1+log_R34+dprimeT|Item), wholeENG)
summary(m.4)
