library(lme4)
library(lmerTest)
wholeCHN = read.csv("/Users/chongzhang/Onedrive/Data-Analyses-R-Python-mySQL/csv files/CHN.csv")
wholeCHN_CORR = read.csv("/Users/chongzhang/Onedrive/Data-Analyses-R-Python-mySQL/csv files/CHN_CORR.csv")
summary(wholeCHN)
summary(wholeCHN_CORR)

#------------------------------------------------------------------------------------------------------#
m_Region1 = lmer(log_R1 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region1) #nothing is significant
m_Region1_CORR = lmer(log_R1 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region1_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region2 = lmer(log_R2 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region2) #nothing is significant
m_Region2_CORR = lmer(log_R2 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region2_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region3 = lmer(log_R3 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region3) #RC1fac         0.044251   0.012267 74.500000   3.607 0.000557 ***
m_Region3_CORR = lmer(log_R3 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region3_CORR) #RC1fac        -0.101044   0.012626 76.900000  -8.003 1.01e-11 ***
#------------------------------------------------------------------------------------------------------#
m_Region4 = lmer(log_R4 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region4) # RC1fac        -0.10052    0.01122 74.23000  -8.961 1.92e-13 ***
m_Region4_CORR = lmer(log_R4 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region4_CORR) # RC1fac        -0.101044   0.012626 76.900000  -8.003 1.01e-11 ***
#------------------------------------------------------------------------------------------------------#
m_Region5 = lmer(log_R5 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region5) #RC1fac        -0.0311805  0.0108910 75.5800000  -2.863  0.00543 ** 
m_Region5_CORR = lmer(log_R5 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region5_CORR) #RC1fac        -3.010e-02  1.142e-02  1.669e+03  -2.636  0.00847 ** 
#------------------------------------------------------------------------------------------------------#
m_Region6 = lmer(log_R6 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region6) #nothing is significant
m_Region6_CORR = lmer(log_R6 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region6_CORR) #nothing is significant
#------------------------------------------------------------------------------------------------------#
m_Region7 = lmer(log_R7 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region7) # RC1fac         0.030048   0.013127 71.080000   2.289   0.0251 *  
m_Region7_CORR = lmer(log_R7 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region7_CORR) # RC1fac         0.040163   0.013966 71.890000   2.876   0.0053 ** 
#------------------------------------------------------------------------------------------------------#
m_Region8 = lmer(log_R8 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region8) # RC1fac         0.02948    0.01153 74.66000   2.557   0.0126 *  
  `                # RC2fac         0.09079    0.01153 74.66000   7.876 2.11e-11 *** 
m_Region8_CORR = lmer(log_R8 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region8_CORR) # RC1fac           0.03406    0.01257 1667.50000   2.709  0.00681 ** 
                        # RC2fac           0.07395    0.01255 1666.60000   5.892 4.61e-09 ***
#------------------------------------------------------------------------------------------------------#
m_Region9 = lmer(log_R9 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region9) # RC2fac          -0.05180    0.01178 2103.00000  -4.397 1.15e-05 ***
                   # RC1fac:RC2fac   -0.09666    0.02356 2103.00000  -4.103 4.24e-05 ***
m_Region9_CORR = lmer(log_R9 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region9_CORR) # RC2fac          -0.04965    0.01338 1667.10000  -3.710 0.000214 ***
                        # RC1fac:RC2fac   -0.11517    0.02679 1667.80000  -4.298 1.82e-05 ***
#------------------------------------------------------------------------------------------------------#
m_Region10 = lmer(log_R10 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region10) # RC2fac           0.03397    0.01084 2103.00000   3.134  0.00175 ** 
m_Region10_CORR = lmer(log_R10 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region10_CORR) # RC2fac           0.04300    0.01225 1667.60000   3.510  0.00046 ***  
#------------------------------------------------------------------------------------------------------#
m_Region11 = lmer(log_R11 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region11) # RC1fac:RC2fac -0.157533   0.030765 73.610000  -5.121 2.36e-06 ***
m_Region11_CORR = lmer(log_R11 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region11_CORR) # RC1fac:RC2fac -0.177627   0.033757 73.040000  -5.262 1.37e-06 *** 
#------------------------------------------------------------------------------------------------------#
m_Region12 = lmer(log_R12 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN)
summary(m_Region12) # RC2fac         2.421e-02  8.537e-03  2.103e+03   2.836  0.00461 ** 
                    # RC1fac:RC2fac -4.031e-02  1.707e-02  2.103e+03  -2.361  0.01832 *  
m_Region12_CORR = lmer(log_R12 ~ RC1fac * RC2fac + (1*log_R4*dprimeT|Participant)+(1*log_R4*dprimeT|Item), wholeCHN_CORR)
summary(m_Region12_CORR) # RC2fac         1.927e-02  9.092e-03  1.666e+03   2.119  0.03420 *  
                         # RC1fac:RC2fac -5.385e-02  1.820e-02  1.667e+03  -2.958  0.00314 **  
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
