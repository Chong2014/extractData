library(lme4)
library(lmerTest)
wholeENG = read.csv("/Users/chongzhang/Onedrive/Data-Analyses-R-Python-mySQL/csv files/ENG.csv")
wholeENG_CORR = read.csv("/Users/chongzhang/Onedrive/Data-Analyses-R-Python-mySQL/csv files/ENG_CORR.csv")
summary(wholeENG)
summary(wholeENG_CORR)



#------------------------------------------------------------------------------------------------------#
m_Region1 = lmer(log_R1 ~ RC1fac * RC2fac + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG)
summary(m_Region1) #nothing is significant
m_Region1_CORR = lmer(log_R1 ~ RC1fac * RC2fac + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG_CORR)
summary(m_Region1_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region2 = lmer(log_R2 ~ RC1fac * RC2fac + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG)
summary(m_Region2) #nothing is significant
m_Region2_CORR = lmer(log_R2 ~ RC1fac * RC2fac + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG_CORR)
summary(m_Region2_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region3 = lmer(log_R3 ~ RC1fac * RC2fac + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG)
summary(m_Region3) # RC1fac        -0.061187   0.011870 75.620000  -5.155 1.97e-06 ***
m_Region3_nonfac = lmer(log_R3 ~ RC1 * RC2 + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG)
summary(m_Region3_nonfac) # RC1S        -0.04320    0.01679 75.62000  -2.573    0.012 *   
m_Region3_CORR = lmer(log_R3 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region3_CORR) # RC1fac        -0.049952   0.012915 75.140000  -3.868 0.000232 ***
#------------------------------------------------------------------------------------------------------#
m_Region4 = lmer(log_R4 ~ RC1fac * RC2fac + (1*dprimeT|Participant)+(1*dprimeT|Item), wholeENG)
summary(m_Region4) # RC1fac        -1.171e-01  1.419e-02  2.103e+03  -8.253 2.22e-16 ***
m_Region4_CORR = lmer(log_R4 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region4_CORR) # RC1fac          -0.11595    0.01676 1534.50000  -6.916 6.77e-12 ***
#------------------------------------------------------------------------------------------------------#
m_Region5 = lmer(log_R5 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG)
summary(m_Region5) #nothing is significant
m_Region5_CORR = lmer(log_R5 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region5_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region6 = lmer(log_R6 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG)
summary(m_Region6) #nothing is significant
m_Region6_CORR = lmer(log_R6 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region6_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region7 = lmer(log_R7 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG)
summary(m_Region7) # RC1fac         0.02612    0.01172 74.33000   2.228   0.0289 *  
m_Region7_CORR = lmer(log_R7 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region7_CORR) # RC1fac         0.03719    0.01384 73.49000   2.688   0.0089 ** 
#------------------------------------------------------------------------------------------------------#
m_Region8 = lmer(log_R8 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG)
summary(m_Region8) # RC2fac         0.05345    0.01106 73.27000   4.831 7.25e-06 ***
                   # RC1fac:RC2fac -0.04532    0.02213 73.27000  -2.048   0.0441 *  
m_Region8_CORR = lmer(log_R8 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region8_CORR) # RC2fac        -0.04825    0.01304 75.67000  -3.701 0.000405 ***
#------------------------------------------------------------------------------------------------------#
m_Region9 = lmer(log_R9 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG)
summary(m_Region9) # RC2fac         0.043866   0.009771 72.610000   4.489 2.63e-05 ***
                   # RC1fac:RC2fac -0.041879   0.019543 72.610000  -2.143   0.0355 * 
m_Region9_CORR = lmer(log_R9 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region9_CORR) # RC2fac         0.05253    0.01143 72.10000   4.596 1.79e-05 ***
#------------------------------------------------------------------------------------------------------#
m_Region10 = lmer(log_R10 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG)
summary(m_Region10) # RC1fac        -0.015505   0.007164 74.120000  -2.164   0.0337 *  
m_Region10_CORR = lmer(log_R10 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeENG_CORR)
summary(m_Region10_CORR) # RC1fac        -1.662e-02  8.376e-03  1.538e+03  -1.984   0.0474 *  
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
