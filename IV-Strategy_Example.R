
# The 2SLS Estimator and Instrumental Variables (IV) Strategy Code Chunks

# 2SLS: a generalised approach to IV estimation when there are one or more
# endogenous variables and at least as many excluded instrument variables (order condition)
# It uses a combination of all exogenous variables (included and
# excluded) that has the highest correlation with the endogenous variable as the IV

# The requirements 
# The IVs not in the population/original model
# IVs uncorrelated with error
# IVs correlated with Average Price
# AP is endogenous due to simultaneity bias (water use and price jointly determined)
# Or OVB???

#Let's take a look at the following model
IV.M3<-ivreg(log(WaterUse)~log(AP)+log(Income)+log(Rain)+
               log(MidTemp) |. -log(AP)+Rtier1+Rtier2+
               Rtier3,data=PWDAD21.p)

summary(IV.M3)

#             Estimate    Std. Error  t value   Pr(>|t|)    
#(Intercept)   -2.3161929  0.0118086   -196.14   <2e-16 ***
#log(RAP)      -0.2507641  0.0029854   -84.00   <2e-16 ***
#log(Income)    0.2936317  0.0009100    322.66   <2e-16 ***
#log(Rain )    -0.0273397  0.0002963   -92.26   <2e-16 ***
#log(MidTemp)   0.9144000  0.0014008    652.75   <2e-16 ***


# When we have multiple IVs for an endogenous variable, we can estimate an integrated
# IV and use it in a 2SLS.
# So all the tire prices are used here

# Manually estimate the first and second stage of a 2SLS
lm.1<-lm(log(RAP)~log(Income)+log(Rain)+log(MidTemp)+
           Rtier1+Rtier2+Rtier3,data=PWDAD21.p)
summary(lm.1)

#                Estimate   Std. Error  t value Pr(>|t|)    
#  (Intercept)   -9.317e-01  1.107e-03 -841.843  < 2e-16 ***
#  log(Income)    1.736e-02  7.205e-05  240.960  < 2e-16 ***
#  log(Rain)     -7.468e-04  2.422e-05  -30.836  < 2e-16 ***
#  log(MidTemp)   7.393e-02  1.163e-04  636.004  < 2e-16 ***
#  Rtier1        -1.234e+01  6.105e-02 -202.169  < 2e-16 ***
#  Rtier2         9.742e+00  4.553e-02  213.987  < 2e-16 ***
#  Rtier3        -7.168e-04  9.141e-05  -7.842  4.45e-15 ***

lm.2<-lm(log(WaterUse)~fitted(lm.1)+log(Income2)+log(Rain)+log(MidTemp),
         data=PWDAD21.p)
summary(lm.2)

mtable(IV.M3,lm.2, summary.stats = F)

# So, we get identical coefficient estimates as in the canned 2SLS estimation

#=======================================
#                  IV.M3      lm.2     
#---------------------------------------
#  (Intercept)    -2.316***  -2.316***  
#                  (0.012)    (0.012)    
#log(RAP)         -0.251***             
#                 (0.003)               
#log(Income)      0.294***    0.294***  
#                 (0.001)    (0.001)    
#log(Rain)       -0.027***   -0.027***  
#                (0.000)     (0.000)    
#log(MidTemp)    0.914***     0.914***  
#                (0.001)     (0.001)    
#fitted(lm.1)                -0.251***  
#                            (0.003)    
#=======================================
#  Significance: 
# *** = p < 0.001;   
# ** = p < 0.01;   
# * = p < 0.05  
#

# We also obtained identical standard errors as in the canned 2SLS estimation
#This may not be the case for smaller samples
#normally 2SLS is less efficient as IV/2SLS has larger standard error than OLS
#due to multicollinearity
#That is the price needs to be paid to get consistent estimate

# Let's check whether we have weak IVs in this case

linearHypothesis(lm.1, c("Rtier1", "Rtier2","Rtier3")) 

# Res.Df     RSS     Df   Sum of Sq       F       Pr(>F)    
#1 18703998  158315                                   
#2 18703995  80364   3     77951        6047486    < 2.2e-16 ***

# So, we have a strong evidence that all instruments  are valid 
# as the joint F statistic is > 10 i.e. 6047486

# Perform an endogeneity test of the AP variable
end.1<-lm(log(WaterUse)~log(RAP)+log(Income)+log(Rain)+
            log(MidTemp)+resid(lm.1),data=PWDAD21.p)
summary(end.1)

#We reject the null that AP is not endogenous, hence, 2SLS is a better estimator
#(The results say there is strong evidence for endogeneity, p value =0.000 
#the coefficients(estimates for the fitted value)



# Exogeneity Test 
# The intuition is if all instruments are exogenous, then the 2SLS residual should be
# uncorrelated with all included and excluded exogenous variables, and any linear
# combination of them.
# and if the intsruments are exogenous they are uncorrelated with the structural error (Ui)

# Perform an over-identification (exogeneity) test
# what is the number of over-identification restrictions q?

# number of IVs - number of endogenous variables = 3-1 = 2

# what is the sample size n?

# n=18704002 

# what is the 95% critical value of a chi2 distribution with 1 degree of freedom?
qchisq(.95, 2) # number of over-identification restrictions q=2

# 5.99

# what is the test statistic?

exo<-lm(resid(IVAP.M3)~log(Income2)+log(Rain)+log(MidTemp)+
          Rtier1+Rtier2+Rtier3,data=PWDAD21.p)
summary(exo) 

#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    1.464e-01  1.407e-02  10.406  < 2e-16 ***
#log(Income)    7.828e-04  9.161e-04   0.855    0.393    
#log(Rain)      1.723e-03  3.080e-04   5.596  2.2e-08 ***
#log(MidTemp)   1.017e-03  1.478e-03   0.688    0.492    
#Rtier1         1.320e+01  7.763e-01  16.999  < 2e-16 ***
#Rtier2        -1.001e+01  5.789e-01 -17.289  < 2e-16 ***
#Rtier3         2.190e-02  1.162e-03  18.844  < 2e-16 ***

# R2=0.0001457 this indicates that the intsruments are uncorrelated with the error
(18703995*0.0001457)

#2725.172

# Given a small value of R2, we fail to reject the null that 
# the instruments are all exogenous
# So Tier1, Tier2 and Tier3 are all strictly exogenous
# They also are in practice, as the prices are set by
# the Independent Pricing and Regulatory tribunal

