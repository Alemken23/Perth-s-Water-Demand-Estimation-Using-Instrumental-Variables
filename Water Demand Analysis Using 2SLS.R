

#Load libraries ----------------------------------------------------------------

library(plm) #Panel data analysis library
library(AER) #Applied Econometric analysis inducing IVreg
library(foreign)
library(lmtest)# For hetoroskedasticity analysis
library(carData)# Companion to applied regression 
library(broom)
library(lattice)
library(MASS)
library(memisc)
library(tseries)   # For timeseries analysis
library(gapminder)

library(panelr)
library(stargazer)
library(ggpubr)
library(sandwich)
library(gmm)
library(systemfit)
library(rstatix)

library(tidyverse)
library(dyn)
library(broom)
library(magrittr)

# Data preparation -------------------------------------------------------------
## Remove top and bottom 1% of water use

quantile(PWDAD21.5$WaterUse, c(.001,.999))

PWDAD21.6<- subset(PWDAD21.5, PWDAD21.5$WaterUse > quantile(PWDAD21.5$WaterUse, c(.001)))   

PWDAD21.7<- subset(PWDAD21.6, PWDAD21.6$WaterUse< quantile(PWDAD21.6$WaterUse, c(.999)))
summary(PWDAD21.7$WaterUse)
dim(PWDAD21.7)
length(unique(PWDAD21.7[,1]))

##########################
## removing top and bottom 1% of GRV

quantile(PWDAD21.7$Latest.Review.GRV, c(.02,.98),na.rm = T)
#?trim

PWDAD21.8<- subset(PWDAD21.7, PWDAD21.7$Latest.Review.GRV> quantile(PWDAD21.7$Latest.Review.GRV,
                                                                    c(.02),na.rm=T))   

PWDAD21.9<- subset(PWDAD21.8, PWDAD21.8$Latest.Review.GRV< quantile(PWDAD21.8$Latest.Review.GRV, 
                                                                    c(.98),na.rm = T))
summary(PWDAD21.9$Latest.Review.GRV)
gc()

#Indexed Income

quantile(PWDAD21.9$IndexedIncome, c(.001,.999),na.rm = T)

PWDAD21.10<- subset(PWDAD21.9, PWDAD21.9$IndexedIncome > quantile(PWDAD21.9$IndexedIncome,
                                                                  c(.001),na.rm=T))

PWDAD21.11<- subset(PWDAD21.10, PWDAD21.10$IndexedIncome < quantile(PWDAD21.10$IndexedIncome,
                                                                    c(.999),na.rm = T))

summary(PWDAD21.11$IndexedIncome)
dim(PWDAD21.11)
length(unique(PWDAD21.11[,1]))

#Raw income
quantile(PWDAD21.11$RIncome2, c(.001,.999),na.rm = T)

PWDAD21.12<- subset(PWDAD21.11, PWDAD21.11$RIncome2 > quantile(PWDAD21.11$RIncome2,
                                                               c(.001),na.rm=T))

PWDAD21.13<- subset(PWDAD21.12, PWDAD21.12$RIncome2 < quantile(PWDAD21.12$RIncome2,
                                                               c(.999),na.rm = T))

dim(PWDAD21.13)
length(unique(PWDAD21.13[,1]))


#save(PWDAD21.13, file="PWDAD21.13.RDATA")

# Analysis starts from here ------------------------------------------------
load("PWDAD21.13.new.RDATA")

head(PWDAD21.13.new)
summary(as.numeric(PWDAD21.13.new$WaterUse))
sd(PWDAD21.13.new$WaterUse)
summary(as.numeric(PWDAD21.13.new$RAP.new))
sd(PWDAD21.13.new$RAP.new)
summary(as.numeric(PWDAD21.13.new$RMP.new))
sd(PWDAD21.13.new$RMP.new)
gc()

dim(PWDAD21.13.new)
length(unique(PWDAD21.13.new[,1]))
head(PWDAD21.13.new)

################################################################################
gc()
#Declare the data has a panel structure for both individual and time dimensions
PWDAD21.p<- pdata.frame(PWDAD21.13.new, index=c("Account.Number","tt"),
                        drop.index=T, row.names=TRUE)

#save memory space
#rm(PWDAD21.6,PWDAD21.7,PWDAD21.8,PWDAD21.9,PWDAD21.10,PWDAD21.11,PWDAD21.12)
gc()
head(PWDAD21.p)

#Set the base for user group

summary(as.factor(PWDAD21.p$Entitlement.Type))
PWDAD21.p$Entitlement.Type= as.factor(PWDAD21.p$Entitlement.Type)
StandardUser<- relevel(PWDAD21.p$Entitlement.Type,ref = "StandardUser")


#Set the base for property group
#Proprty.Type= PWDAD21.p$Land.Use.Description
summary(PWDAD21.p$Land.Use.Description)
PWDAD21.p$Land.Use.Description= as.factor(PWDAD21.p$Land.Use.Description)
House<-relevel(PWDAD21.p$Land.Use.Description,ref = "House")

#Set the base for location variable
summary(as.factor(PWDAD21.p$Billing.Sub.Group.Description))
PWDAD21.p$Billing.Sub.Group.Description=as.factor(PWDAD21.p$Billing.Sub.Group.Description)
PLocation<-relevel(PWDAD21.p$Billing.Sub.Group.Description,ref = "Central Metropolitan")

#Rename GRV
GRV<-PWDAD21.p$Latest.Review.GRV

#Rename Meter size
colnames(PWDAD21.p)
Meter.Size= as.numeric(PWDAD21.p$Count.of.active.Meter.20mm)


##Readjusting variables' name for lot-sizes
Area.p<- PWDAD21.p$Area.m2
Proprty.Sizehh<-PWDAD21.p$cadlothharea
Proprty.Sizelot<-PWDAD21.p$cadlotarea

## adjusting the variable name for property type
Proprty.Type<-PWDAD21.p$Land.Use.Description
summary(as.factor(Proprty.Type))
######################################################################
######################################################################

colnames(PWDAD21.p)
colnames(PWDAD21.13.new)

gc()
## Run 2SLE models ------------------------------------------------------------

#RE-2SLS
#log(Proprty.Sizelot)+
II.AM1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              House+ StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,model= "random", inst.method = "bvk")
summary(II.AM1)

confint(II.AM1)
confint(II.AM1, level=0.90)

model1<-II.AM1


#FE-2SLS
#log(Proprty.Sizelot)+
II.FE1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+
              House+ StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,model= "within")
summary(II.FE1)


#Income model
Income.AM1<-plm(log(WaterUse)~log(RAP.new)+ log(RIncome2)+
                  log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                  House+ StandardUser+Janread+Febread+Marread+
                  Aprread+Mayread+Junread+Julread+Augread+
                  Octread+Novread+Decread |. -log(RAP.new)+
                  Rtier1.new + Rtier2.new + Rtier3.new,
                data=PWDAD21.p,model= "random", inst.method = "bvk")
summary(Income.AM1)

#GRV Model
gc()
GRV.AM1<-plm(log(WaterUse)~log(RAP.new)+ log(GRV)+
               log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
               House+ StandardUser+Janread+Febread+Marread+
               Aprread+Mayread+Junread+Julread+Augread+
               Octread+Novread+Decread |. -log(RAP.new)+
               Rtier1.new + Rtier2.new + Rtier3.new,
             data=PWDAD21.p,model= "random", inst.method = "bvk")
summary(GRV.AM1)



#Both GRV and raw income model
gc()

GRVIncome.AM1<-plm(log(WaterUse)~log(RAP.new)+ log(RIncome2)+log(GRV)+
                     log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                     House+ StandardUser+Janread+Febread+Marread+
                     Aprread+Mayread+Junread+Julread+Augread+
                     Octread+Novread+Decread |. -log(RAP.new)+
                     Rtier1.new + Rtier2.new + Rtier3.new,
                   data=PWDAD21.p,model= "random", inst.method = "bvk")
summary(GRVIncome.AM1)
###########################################################################
## Run Models for Marginal price ##

G2SLS.MP1<-plm(log(WaterUse)~log(RMP.new)+ log(IndexedIncome)+
                 log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                 House+ StandardUser+Janread+Febread+Marread+
                 Aprread+Mayread+Junread+Julread+Augread+
                 Octread+Novread+Decread |. -log(RMP.new)+
                 Rtier1.new + Rtier2.new + Rtier3.new,
               data=PWDAD21.p,model= "random", inst.method = "bvk")
summary(G2SLS.MP1)

confint(G2SLS.MP1)
confint(G2SLS.MP1, level=0.90)

model1<-G2SLS.MP1

###########################################################

# Sub-group analysis for user group -------------------------------------------
#rm(PWDAD21.13.new)

summary(as.factor(PWDAD21.p$Entitlement.Type))
PWDAD21.p$Entitlement.Type= as.factor(PWDAD21.p$Entitlement.Type)
UserGroup= PWDAD21.p$Entitlement.Type

tapply(PWDAD21.p$WaterUse,PWDAD21.p$Entitlement.Type,mean)
tapply(PWDAD21.p$RAP.new,PWDAD21.p$Entitlement.Type,mean)

gc()

##Standard user
Standard.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
                  log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                  House+Janread+Febread+Marread+
                  Aprread+Mayread+Junread+Julread+Augread+
                  Octread+Novread+Decread |. -log(RAP.new)+
                  Rtier1.new + Rtier2.new + Rtier3.new,
                data=PWDAD21.p,subset = UserGroup=="StandardUser",
                model= "random", inst.method = "bvk")
summary(Standard.1)

# Partial adjustment model for standard user--------------

Standardlaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                  log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                  House+Janread+Febread+Marread+
                  Aprread+Mayread+Junread+Julread+Augread+
                  Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+
                  Rtier1.new + Rtier2.new + Rtier3.new,
                data=PWDAD21.p,subset = UserGroup=="StandardUser",
                model= "random", inst.method = "bvk")
summary(Standardlaged.1)

##Pensioner
gc()
Pens.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              House+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,subset = UserGroup=="Pensioner",
            model= "random", inst.method = "bvk")
summary(Pens.1)

gc()

#partial adjustment for pensioner

Penslaged.2<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              House+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+
              Rtier1.new + Rtier2.new + Rtier3.new,
             data=PWDAD21.p,subset = UserGroup=="Pensioner",
            model= "random", inst.method = "bvk")
summary(Penslaged.2)


# also tried log(lag2)+ log(lag3)+log(lag4)+log(lag5)+log(lag6) as instruments but did not make sense
                  
gc()

##State Senior 
StateS.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
                log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                House+Janread+Febread+Marread+
                Aprread+Mayread+Junread+Julread+Augread+
                Octread+Novread+Decread |. -log(RAP.new)+
                Rtier1.new + Rtier2.new + Rtier3.new,
              data=PWDAD21.p,subset = UserGroup=="StateSenior",
              model= "random", inst.method = "bvk")
summary(StateS.1)


#partial adjustment for State Senior 

StateSlaged.2<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
                House+Janread+Febread+Marread+
                Aprread+Mayread+Junread+Julread+Augread+
                Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+
                Rtier1.new + Rtier2.new + Rtier3.new,
               data=PWDAD21.p,subset = UserGroup=="StateSenior",
              model= "random", inst.method = "bvk")
summary(StateSlaged.2)


gc()

##State&Cwlth-Senior 
SCS.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
             log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
             House+Janread+Febread+Marread+
             Aprread+Mayread+Junread+Julread+Augread+
             Octread+Novread+Decread |. -log(RAP.new)+
             Rtier1.new + Rtier2.new + Rtier3.new,
           data=PWDAD21.p,subset = UserGroup=="State&Cwlth-Senior",
           model= "random", inst.method = "bvk")
summary(SCS.1)

###
##partial adjustment model for State&Cwlth-Senior 
#
SCSlaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
             log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
             House+Janread+Febread+Marread+
             Aprread+Mayread+Junread+Julread+Augread+
             Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+I(lag2^2)+ 
             Rtier1.new + Rtier2.new + Rtier3.new,
           data=PWDAD21.p,subset = UserGroup=="State&Cwlth-Senior",
           model= "random", inst.method = "bvk")
summary(SCSlaged.1)


##########################################################################
##########################################################################

## Sub-group analysis of Property Type

Proprty.Type= PWDAD21.p$Land.Use.Description
summary(as.factor(Proprty.Type))



#simple mean for each group
tapply(PWDAD21.p$WaterUse,PWDAD21.p$Land.Use.Description,mean)
tapply(PWDAD21.p$RAP.new,PWDAD21.p$Land.Use.Description,mean)

##House
gc()


Hous.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
             data=PWDAD21.p,subset = Proprty.Type=="House",
             model= "random", inst.method = "bvk")
summary(Hous.1)

#partial adjustment model

Houslaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+I(lag2^2)+
              Rtier1.new + Rtier2.new + Rtier3.new,
              data=PWDAD21.p,subset = Proprty.Type=="House",
              model= "random", inst.method = "bvk")
summary(Houslaged.1)


##Units
gc()


Unit.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,subset = Proprty.Type=="Unit",
            model= "random", inst.method = "bvk")
summary(Unit.1)

#Partial adjustment model
Unitlaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+I(lag2^2)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,subset = Proprty.Type=="Unit",
            model= "random", inst.method = "bvk")
summary(Unitlaged.1)


##Attached Dwellings

AD.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
            log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
            StandardUser+Janread+Febread+Marread+
            Aprread+Mayread+Junread+Julread+Augread+
            Octread+Novread+Decread |. -log(RAP.new)+
            Rtier1.new + Rtier2.new + Rtier3.new,
          data=PWDAD21.p,subset = Proprty.Type=="AttachedDwelling",
          model= "random", inst.method = "bvk")
summary(AD.1)

#partial adjustment model

ADlaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
            log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
            StandardUser+Janread+Febread+Marread+
            Aprread+Mayread+Junread+Julread+Augread+
            Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+ lag2+ I(lag2^2)+
            Rtier1.new + Rtier2.new + Rtier3.new,
          data=PWDAD21.p,subset = Proprty.Type=="AttachedDwelling",
          model= "random", inst.method = "bvk")
summary(ADlaged.1)


##Flat
Flat.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,subset = Proprty.Type=="Flat",
            model= "random", inst.method = "bvk")
summary(Flat.1)

#partial adjustment model
#
Flatlaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+ lag2+ I(lag2^2)+
              Rtier1.new + Rtier2.new+ Rtier3.new ,
            data=PWDAD21.p,subset = Proprty.Type=="Flat",
            model= "random", inst.method = "baltagi")
summary(Flatlaged.1)


##Other properties

Other.1<-plm(log(WaterUse)~log(RAP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
              StandardUser+Janread+Febread+Marread+
              Aprread+Mayread+Junread+Julread+Augread+
              Octread+Novread+Decread |. -log(RAP.new)+
              Rtier1.new + Rtier2.new + Rtier3.new,
            data=PWDAD21.p,subset = Proprty.Type=="Other",
            model= "random", inst.method = "bvk")
summary(Other.1)

#I(lag2^2)+
#Partial adjustment model
Otherlaged.1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
               log(Rain+1)+ log(MidTemp)+ PLocation+log(Proprty.Sizelot)+
               StandardUser+Janread+Febread+Marread+
               Aprread+Mayread+Junread+Julread+Augread+
               Octread+Novread+Decread |. -log(RAP.new)-log(lag1)+lag2+
               Rtier1.new + Rtier2.new + Rtier3.new,
             data=PWDAD21.p,subset = Proprty.Type=="Other",
             model= "random", inst.method = "bvk")
summary(Otherlaged.1)

########################################################################

# Summer Vs Winter Model -------------------------------------------------------
sumr.1= subset(PWDAD21.p,PWDAD21.p$amon==12|
                 PWDAD21.p$amon==1| PWDAD21.p$amon==2)
dim(sumr.1)
#Location
sumr.1$Billing.Sub.Group.Description=as.factor(sumr.1$Billing.Sub.Group.Description)
PLocations<-relevel(sumr.1$Billing.Sub.Group.Description,ref = "Central Metropolitan")
gc()

## The effect of property
sumr.1$Land.Use.Description = as.factor(sumr.1$Land.Use.Description)
House.S<-relevel(sumr.1$Land.Use.Description,ref = "House")

#Pensioner and Lotsize effects

sumr.1$Entitlement.Type = as.factor(sumr.1$Entitlement.Type)
SStandardUser<- relevel(sumr.1$Entitlement.Type,ref = "StandardUser")
SLot.size<-sumr.1$cadlotarea

#For Indexed income Model
?plm
gc()
SMR.RE5<-plm(log(WaterUse)~log(RAP.new)+log(IndexedIncome)+log(Rain+1)+
               log(MidTemp)+ PLocations+log(SLot.size)+ House.S+
               SStandardUser | . -log(RAP.new)+ Rtier1.new+
               Rtier2.new+Rtier3.new, data =  sumr.1,model = "random",
             inst.method = "bvk")
summary(SMR.RE5)

confint(SMR.RE5)
confint(SMR.RE5, level=0.90)

model2<-SMR.RE5

#Temp as quadratic function

SMR.RE6<-plm(log(WaterUse)~log(RAP.new)+log(IndexedIncome)+log(Rain+1)+
               MidTemp+I(MidTemp^2)+ PLocations+log(SLot.size)+ House.S+
               SStandardUser | . -log(RAP.new)+ Rtier1.new+
               Rtier2.new+Rtier3.new, data =  sumr.1,model = "random",
             inst.method = "bvk")
summary(SMR.RE6)

gc()
##########################################################
#Run models for lagged values
#Below is the best model
SMRLaged1.RE5<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+log(IndexedIncome)+log(Rain+1)+
               log(MidTemp)+ PLocations+log(SLot.size)+ House.S+
               SStandardUser | . -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+ Rtier1.new+
               Rtier2.new+Rtier3.new, data =  sumr.1,model = "random",
              inst.method = "bvk")
summary(SMRLaged1.RE5)

##+log(lag2)+log(lag4)+log(lag5)+log(lag6)
SMRLaged2.RE5<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+log(IndexedIncome)+log(Rain+1)+
                    log(MidTemp)+ PLocations+log(SLot.size)+ House.S+
                    SStandardUser | . -log(RAP.new)-log(lag1)+log(lag2)+log(lag3)+
                     Rtier1.new+Rtier2.new+Rtier3.new, data =  sumr.1,model = "random",
                  inst.method = "bvk")
summary(SMRLaged2.RE5)


#Lagged model using the Baltagi method
gc()
SMRLaged3.RE5<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+log(IndexedIncome)+log(Rain+1)+
                    log(MidTemp)+ PLocations+log(SLot.size)+ House.S+
                    SStandardUser | . -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+ Rtier1.new+
                    Rtier2.new+Rtier3.new, data =  sumr.1,model = "random",
                   inst.method = "baltagi")
summary(SMRLaged3.RE5)


#########################################################
## Run Models for Marginal price##
SMR.MP2<-plm(log(WaterUse)~log(RMP.new)+log(IndexedIncome)+log(Rain+1)+
               log(MidTemp)+ PLocations+log(SLot.size)+ House.S+
               SStandardUser | . -log(RMP.new)+ Rtier1.new+
               Rtier2.new+Rtier3.new, data =  sumr.1,model = "random",
             inst.method = "bvk")
summary(SMR.MP2)
###########################################################


# LR vs SR estimates for a single month-----------------------------------------
gc()
#Summer

sumr.LS= subset(PWDAD21.p,PWDAD21.p$amon==1)

dim(sumr.LS)

sumr.LS$Billing.Sub.Group.Description=as.factor(sumr.LS$Billing.Sub.Group.Description)
LSLocation<-relevel(sumr.LS$Billing.Sub.Group.Description,ref = "Central Metropolitan")

sumr.LS$Land.Use.Description = as.factor(sumr.LS$Land.Use.Description)
LSHouse<-relevel(sumr.LS$Land.Use.Description,ref = "House")

sumr.LS$Entitlement.Type = as.factor(sumr.LS$Entitlement.Type)
LSStandUser<- relevel(sumr.LS$Entitlement.Type,ref = "StandardUser")
LSProprtySize<-sumr.LS$cadlotarea
LSGRV<- sumr.LS$Latest.Review.GRV


gc()
#log(lag1)+
#-log(lag1)+lag2
#Indexed income
#Lag1
?plm

##Overall, this is the best model
REIV.SMR1<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+ log(MidTemp)+ LSLocation+log(LSProprtySize)+
                 LSHouse+ LSStandUser |. -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+
                 Rtier1.new+ Rtier2.new+ Rtier3.new,
               data=sumr.LS,model= "random", inst.method = "bvk")
summary(REIV.SMR1)

confint(REIV.SMR1)
confint(REIV.SMR1, level=0.90)
################################################
##Marginal Price
OD.MP3<-plm(log(WaterUse)~log(lag1)+log(RMP.new)+ log(IndexedIncome)+
              log(Rain+1)+ log(MidTemp)+ LSLocation+log(LSProprtySize)+
              LSHouse+ LSStandUser |. -log(RMP.new)-log(lag1)+lag2+ I(lag2^2)+
              Rtier1.new+ Rtier2.new+ Rtier3.new,
            data=sumr.LS,model= "random", inst.method = "bvk")
summary(OD.MP3)

confint(OD.MP3)
confint(OD.MP3, level=0.90)
##############################################

#Below is the best model for PE
REIV.SMR1.2<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                   log(Rain+1)+ log(MidTemp)+ LSLocation+log(LSProprtySize)+
                   LSHouse+ LSStandUser |. -log(RAP.new)-log(lag1)+lag2+ I(lag2^2)+
                   Rtier1.new+ Rtier2.new+ Rtier3.new,
                 data=sumr.LS,model= "random", inst.method = "baltagi")
summary(REIV.SMR1.2)

confint(REIV.SMR1.2)
confint(REIV.SMR1.2, level=0.90)

model3<-REIV.SMR1.2

#######################################################
#For Marginal Price
OD.MP3.1<-plm(log(WaterUse)~log(lag1)+log(RMP.new)+ log(IndexedIncome)+
                log(Rain+1)+ log(MidTemp)+ LSLocation+log(LSProprtySize)+
                LSHouse+ LSStandUser |. -log(RMP.new)-log(lag1)+lag2+ I(lag2^2)+
                Rtier1.new+ Rtier2.new+ Rtier3.new,
              data=sumr.LS,model= "random", inst.method = "baltagi")
summary(OD.MP3.1)

confint(OD.MP3.1)
confint(OD.MP3.1, level=0.90)

model3<-OD.MP3.1
########################################################

#trying log(lag2) up to log(lag6)
REIV.SMR2<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+ log(MidTemp)+ LSLocation+log(LSProprtySize)+
                 LSHouse+ LSStandUser |. -log(RAP.new)-log(lag1)+log(lag2)+ 
                 log(lag3) + log(lag4)+log(lag5)+log(lag6)+
                 Rtier1.new+ Rtier2.new+ Rtier3.new,
               data=sumr.LS,model= "random", inst.method = "bvk")
summary(REIV.SMR2)

#try temperature as quadratic

REIV.SMR3<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+MidTemp+ I(MidTemp^2)+ LSLocation+log(LSProprtySize)+
                 LSHouse+ LSStandUser |. -log(RAP.new)-log(lag1)+(lag2)+ I(lag2^2)+
                 Rtier1.new+ Rtier2.new+ Rtier3.new,
               data=sumr.LS,model= "random", inst.method = "bvk")
summary(REIV.SMR3)

#Lag2
REIV.SMR4<-plm(log(WaterUse)~log(lag2)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+ log(MidTemp)+ LSLocation+log(LSProprtySize)+
                 LSHouse+ LSStandUser |. -log(RAP.new)-log(lag2)+(lag3)+ I(lag3^2)+
                 Rtier1.new+ Rtier2.new+ Rtier3.new,
               data=sumr.LS,model= "random", inst.method = "bvk")
summary(REIV.SMR4)
###################################################################
###################################################################

##Winter
gc()
wintr.1= subset(PWDAD21.p, PWDAD21.p$amon==7 | PWDAD21.p$amon==8| PWDAD21.p$amon==9)

#Model 2--the effect of regions
wintr.1$Billing.Sub.Group.Description= as.factor(wintr.1$Billing.Sub.Group.Description)
PLocation.w<-relevel(wintr.1$Billing.Sub.Group.Description,ref = "Central Metropolitan")

#Model 3--the effect of property group and year
wintr.1$Land.Use.Description= as.factor(wintr.1$Land.Use.Description)
House.w<-relevel(wintr.1$Land.Use.Description,ref = "House")

wintr.1$Entitlement.Type= as.factor(wintr.1$Entitlement.Type)
Standard.w<- relevel(wintr.1$Entitlement.Type,ref = "StandardUser")
Lot.size.w<-wintr.1$cadlotarea

#
WNTR.RE5<-plm(log(WaterUse)~log(RAP.new)+log(IndexedIncome)+log(Rain+1)+log(MidTemp)+
                PLocation.w+ log(Lot.size.w)+ House.w+
                Standard.w | . -log(RAP.new)+ Rtier1.new+
                Rtier2.new+Rtier3.new, data =  wintr.1,
              model = "random", inst.method = "baltagi")
summary(WNTR.RE5)

confint(WNTR.RE5)

confint(WNTR.RE5, level=0.90)

model4<-WNTR.RE5

##########
gc()
#Lagged values for winter model for baltagi
#below is the best model
WNTRLaged.RE5<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+log(IndexedIncome)+log(Rain+1)+log(MidTemp)+
                PLocation.w+ log(Lot.size.w)+ House.w+
                Standard.w | . -log(RAP.new)-log(lag1)+
                  lag2+ I(lag2^2)+Rtier1.new+Rtier2.new+
                  Rtier3.new, data =  wintr.1,
                 model = "random", inst.method = "baltagi")
summary(WNTRLaged.RE5)

#Lagged values for winter model for bvk


WNTRLaged2.RE5<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+log(IndexedIncome)+log(Rain+1)+log(MidTemp)+
                     PLocation.w+ log(Lot.size.w)+ House.w+
                     Standard.w | . -log(RAP.new)-log(lag1)+
                     lag2+ I(lag2^2)+Rtier1.new+Rtier2.new+
                     Rtier3.new, data =  wintr.1,
                   model = "random", inst.method = "bvk")
summary(WNTRLaged2.RE5)

########################################################


#Marginal Price Model
WNTR.MP4<-plm(log(WaterUse)~log(RMP.new)+log(IndexedIncome)+log(Rain+1)+log(MidTemp)+
                PLocation.w+ log(Lot.size.w)+ House.w+
                Standard.w | . -log(RMP.new)+ Rtier1.new+
                Rtier2.new+Rtier3.new, data =  wintr.1,
              model = "random", inst.method = "baltagi")
summary(WNTR.MP4)

confint(WNTR.MP4)

confint(WNTR.MP4, level=0.90)

model4<-WNTR.MP4
#####################################################
#Temp as quadratic
WNTR.RE6<-plm(log(WaterUse)~log(RAP.new)+log(IndexedIncome)+log(Rain+1)+
                MidTemp+I(MidTemp^2)+
                PLocation.w+ log(Lot.size.w)+ House.w+
                Standard.w | . -log(RAP.new)+ Rtier1.new+
                Rtier2.new+Rtier3.new, data =  wintr.1,
              model = "random", inst.method = "baltagi")
summary(WNTR.RE6)

######################################################
######################################################

#LR Vs SR estimates again ------------------------------------------------------
#Winter
gc()
wintr.LS= subset(PWDAD21.p,PWDAD21.p$amon==7)
dim(wintr.LS)

wintr.LS$Billing.Sub.Group.Description=as.factor(wintr.LS$Billing.Sub.Group.Description)
LSLocation.w<-relevel(wintr.LS$Billing.Sub.Group.Description,ref = "Central Metropolitan")

wintr.LS$Land.Use.Description = as.factor(wintr.LS$Land.Use.Description)
LSHouse.w<-relevel(wintr.LS$Land.Use.Description,ref = "House")

wintr.LS$Entitlement.Type = as.factor(wintr.LS$Entitlement.Type)
LSStandUser.w<- relevel(wintr.LS$Entitlement.Type,ref = "StandardUser")
LSProprtySize.w<-wintr.LS$cadlotarea
LSGRV.w<- wintr.LS$Latest.Review.GRV


gc()

colnames(wintr.LS)
REIV.WSR<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                log(Rain+1)+log(MidTemp)+  LSLocation.w+log(LSProprtySize.w)+
                LSHouse.w+ LSStandUser.w |. -log(RAP.new)-log(lag1)+ lag2+
                Rtier1.new+ Rtier2.new+Rtier3.new,
              data=wintr.LS,model= "random",inst.method = "bvk")
summary(REIV.WSR)

#log(lag2) up to log(lag6) as instruments--best model so far 

REIV.WSR2<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+log(MidTemp)+  LSLocation.w+log(LSProprtySize.w)+
                 LSHouse.w+ LSStandUser.w |. -log(RAP.new)-log(lag1)+ log(lag2)+
                 log(lag3)+log(lag4)+log(lag5)+log(lag6)+
                 Rtier1.new+ Rtier2.new+ Rtier3.new,
               data=wintr.LS,model= "random",inst.method = "bvk")
summary(REIV.WSR2)

confint(REIV.WSR2)
confint(REIV.WSR2, level=0.90)

model5<-REIV.WSR2

####################################################
#Marginal Price Model

ID.MP5<-plm(log(WaterUse)~log(lag1)+log(RMP.new)+ log(IndexedIncome)+
              log(Rain+1)+log(MidTemp)+  LSLocation.w+log(LSProprtySize.w)+
              LSHouse.w+ LSStandUser.w |. -log(RMP.new)-log(lag1)+ log(lag2)+
              log(lag3)+log(lag4)+log(lag5)+log(lag6)+
              Rtier1.new+ Rtier2.new+ Rtier3.new,
            data=wintr.LS,model= "random",inst.method = "bvk")
summary(ID.MP5)

confint(ID.MP5)
confint(ID.MP5, level=0.90)

model5<-ID.MP5
#######################################################

#Lag2 as endogenous variable
REIV.WSR3<-plm(log(WaterUse)~log(lag2)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+log(MidTemp)+ LSLocation.w+log(LSProprtySize.w)+
                 LSHouse.w + LSStandUser.w |. -log(RAP.new)-log(lag2)+ (lag3)+I(lag3^2)+
                 Rtier1.new+ Rtier2.new+Rtier3.new,
               data=wintr.LS,model= "random",inst.method = "bvk")
summary(REIV.WSR3)

#Temp as quadratic

REIV.WSR4<-plm(log(WaterUse)~log(lag1)+log(RAP.new)+ log(IndexedIncome)+
                 log(Rain+1)+MidTemp+I(MidTemp^2)+ LSLocation.w+log(LSProprtySize.w)+
                 LSHouse.w+ LSStandUser.w |. -log(RAP.new)-log(lag1)+ (lag2)+
                 Rtier1.new+ Rtier2.new+Rtier3.new,
               data=wintr.LS,model= "random",inst.method = "bvk")
summary(REIV.WSR4)

#####################################
######### Plot the CI #############

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Pooled Model")
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Summer Model")
model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Winter Model")

model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Outdoor demand Model")

model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Indoor demand Model")



# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame,
                                  model4Frame,model5Frame))  

#write.csv(allModelFrame, file ="allModelFrame.csv", row.names = F)

allModelFrame <- read.csv("allModelFrame.csv")
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Comparing several models")
print(zp1)  # The trick to these is position_dodge().

