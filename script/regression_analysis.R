install.packages("memisc")
install.packages("pacman")
install.packages("DescTools")
install.packages("betareg")
install.packages("lmerTest")
install.packages("rlang")
install.packages("vctrs")
install.packages("performance")
install.packages("fitdistrplus")

library("memisc")
library(modelsummary)
library(lme4)
library(car)
library(betareg)
library(lmerTest)
library(performance)
library(ggplot2)
library(fitdistrplus)
library(modelsummary)
library(stargazer)

setwd("PATH_TO_THE_SCRIPT") # set the path


# Read the table of estimated travel delay per time slot (15min)

varlong<-read.csv('../data/delay_per_time_slot.csv',header=T)
varlong$peakhours<-as.character(varlong$peakhours)
varlong$daytype<-as.character(varlong$daytype)
varlong$ST_LGT_good<-as.character(varlong$ST_LGT_good)
varlong$Region<-as.character(varlong$Region)

varlong$fSEVERITY<-factor(varlong$SEVERITY)
varlong$fACC_NO<-factor(varlong$ACC_NO)
varlong$roadwork<-as.character(varlong$roadwork)

# change the scale for some variables to make the coefficients easier to be interpreted
varlong$psn_distance<-varlong$psn_distance/1000
varlong$fs_distance<-varlong$fs_distance/1000
varlong$hos_distance<-varlong$hos_distance/1000
varlong$AADT<-varlong$AADT/1000
varlong$PedestrianExposure<-varlong$PedestrianExposure/1000
any(is.na(varlong))

#####
# COMMTR main model (Table 4)

model_crashrandomeffect <- list(
  'CT' = glmer(traveldelay_15min_sum~NO_VEH+factor(heavey_involve)+NO_CSU+
                   relevel(factor(SEVERITY), ref = "Slight")+relevel(factor(collision), ref = "single-vehicle")+
                   relevel(factor(ST_LGT_good), ref = "good")+
                 relevel(factor(WEATHER), ref='Clear')+
                   factor(daytype)+factor(peakhours)+
                   relevel(factor(timeslot_15min), ref = "traveldelay_15min_t4")+factor(roadwork)+
                   (1| ACC_NO),
                 data=varlong, family=gaussian(link="identity")),
  
  'CTRB' = glmer(traveldelay_15min_sum~NO_VEH+factor(heavey_involve)+NO_CSU+
                                    relevel(factor(SEVERITY), ref = "Slight")+relevel(factor(collision), ref = "single-vehicle")+
                                    relevel(factor(ST_LGT_good), ref = "good")+
                   relevel(factor(WEATHER), ref='Clear')+
                                    factor(daytype)+factor(peakhours)+
                                    psn_distance+am_depot_distance+hos_distance+
                                    factor(IN_20M_JCN)+
                                    relevel(factor(Region), ref = "HKI")+
                                    relevel(factor(FRC_string),ref='Minor')+KPH+AADT+PedestrianExposure+
                                    edge_degree+
                                    CMF+CUF+GOV+HNC+RSF+TRS+
                                    relevel(factor(timeslot_15min), ref = "traveldelay_15min_t4")+factor(roadwork)+
                                   (1 | ACC_NO),
                                  data=varlong, family=gaussian(link="identity"))
)

modelsummary(model_crashrandomeffect, shape = term ~ model+statistic,
             estimate="{estimate}{stars}",
             statistic = c("std.error"),
             stars = TRUE, output = "../output/regression_result_table4.docx")


##### 
# model the total delays (for each crash, summing up for 15min delays). This is for predicting total delays for all crashes in 2021

# Read the table of the total delays per crash

var<-read.csv('../data/total_delay_per_crash.csv',header=T) # the most updated variable table

var$peakhours<-as.character(var$peakhours)
var$FRC_string<-as.character(var$FRC_string)
var$daytype<-as.character(var$daytype)
var$ST_LGT_good<-as.character(var$ST_LGT_good)
var$Region<-as.character(var$Region)
var$NO_VEH_2type<-as.character(var$NO_VEH_2type)
var$roadwork<-as.character(var$roadwork)

# change the scale for some variables to make the coefficients easier to be interpreted

var$psn_distance<-var$psn_distance/1000 
var$fs_distance<-var$fs_distance/1000 
var$hos_distance<-var$hos_distance/1000 
var$am_depot_distance<-var$am_depot_distance/1000 
var$AADT<-var$AADT/1000 
var$PedestrianExposure<-var$PedestrianExposure/1000
var$fSEVERITY<-factor(var$SEVERITY)
any(is.na(var))

shortdf_td_sum15min_forprediction <- glm(traveldelay_15min_sum~NO_VEH+factor(heavey_involve)+NO_CSU+
                                           relevel(factor(SEVERITY), ref = "Slight")+relevel(factor(collision), ref = "single-vehicle")+
                                           relevel(factor(ST_LGT_good), ref = "good")+
                                           factor(daytype)+factor(peakhours)+
                                           psn_distance+am_depot_distance+hos_distance+
                                           factor(IN_20M_JCN)+
                                           relevel(factor(Region), ref = "HKI")+
                                           relevel(factor(FRC_string),ref='Minor')+KPH+AADT+PedestrianExposure+
                                           edge_degree+
                                           CMF+CUF+GOV+HNC+RSF+TRS,
                                         data=var, family=gaussian(link="log"),
                                         control=list(maxit=100), mustart=pmax(var$traveldelay_15min_sum,0.001))

model_performance(shortdf_td_sum15min_forprediction)
summary(shortdf_td_sum15min_forprediction)


#####

# Use the established model to predict travel delays in 2021 in hk
# read predictors for 2021 (Jan to Nov, Dec not available) crashes in hk:

var2021<-read.csv('../data/variable_for_all_crashes_in_2021.csv',header=T)

var2021$peakhours<-as.character(var2021$peakhours)
var2021$daytype<-as.character(var2021$daytype)
var2021$ST_LGT_good<-as.character(var2021$ST_LGT_good)
var2021$Region<-as.character(var2021$Region)
var2021$fFRC<-factor(var2021$FRC) 
var2021$fSEVERITY<-factor(var2021$SEVERITY)
var2021$psn_distance<-var2021$psn_distance/1000
var2021$fs_distance<-var2021$fs_distance/1000
var2021$hos_distance<-var2021$hos_distance/1000
var2021$am_depot_distance<-var2021$am_depot_distance/1000
var2021$AADT<-var2021$AADT/1000
var2021$PedestrianExposure<-var2021$PedestrianExposure/1000

# Predict
var2021$prediction2021<-predict(shortdf_td_sum15min_forprediction, var2021, type='response')
# Ouput the result
write.csv(var2021, "../output/predicted_travel_delays_in_2021_localtest.csv", row.names=FALSE)
