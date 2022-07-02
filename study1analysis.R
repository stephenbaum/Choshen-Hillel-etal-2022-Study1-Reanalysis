
#### Reanalysis of "Physicians prescribe fewer analgesics during night shifts than day shifts ####
# Stephen M. Baum #
# Last Updated: 6/30/2022 #

#### load all necessary packages in ####
library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(plotrix)
library(devtools)
library(bwrappers)
library(effsize)
library(misty)
library(miceadds)
library(bda)
library(lmerTest)
library(cowplot) 
library(sjPlot) 
library(sjmisc) 
library(effects)
library(sjstats) 
library(ggpubr)
library(multcomp)
library(medmod)

#### load in the data itself ####
rm(list = ls()) # clean environment #
s1 = read_excel("study1data.xlsx")
sapply(s1, class)

# change participant ID to a factor #
s1$subject_num = as.factor(s1$subject_num)
nlevels(s1$subject_num)

#### empathy measures ####

# first model #
# effect of condition on empathy for pain #
mod1 = lm(EFP_Avg ~ after_night_shiftQ, data = s1)
summary(mod1)

# look at descriptives, etc. #
s1$after_night_shiftQ = as.factor(s1$after_night_shiftQ)
wrap.t.ind(dv1=s1$EFP_Avg,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$EFP_Avg,iv1=s1$after_night_shiftQ)

# second model #
# effect of condition on emphatic accuracy #
# there is weirdness in this data - lots of missing values #
# unclear what this actually means? #
s1$EAmean_corGoodData = as.numeric(s1$EAmean_corGoodData)
wrap.t.ind(dv1=s1$EAmean_corGoodData,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$EAmean_corGoodData,iv1=s1$after_night_shiftQ)

# also, there is one outlier - who is 3SDs away from the mean #
# that is subject number 41 (row 30) #
s1[c(30),] # this is the subject
s1minus_sub41 = s1[-c(30),]
wrap.t.ind(dv1=s1minus_sub41$EAmean_corGoodData,iv1=s1minus_sub41$after_night_shiftQ)
wrap.desc(dv1=s1minus_sub41$EAmean_corGoodData,iv1=s1minus_sub41$after_night_shiftQ)
# this matches the result reported in paper

#### pain assessment measures ####

# backache #
# effect of condition on pain assessment #
wrap.t.ind(dv1=s1$scenario1_painQ_1,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario1_painQ_1,iv1=s1$after_night_shiftQ)

# there are three outliers - who are 3SDs away from the mean #
mean(s1$scenario1_painQ_1,na.rm=T) #73.51
sd(s1$scenario1_painQ_1,na.rm=T) #13.23
# anyone below 33.83 is an outlier #
# subjects number 30,70,76 are the outliers #
# these are rows 22, 52, and 58 #
s1minus_sub30_70_76 = s1[-c(22, 52, 58),]

wrap.t.ind(dv1=s1minus_sub30_70_76$scenario1_painQ_1,iv1=s1minus_sub30_70_76$after_night_shiftQ, var.equal = F)
wrap.desc(dv1=s1minus_sub30_70_76$scenario1_painQ_1,iv1=s1minus_sub30_70_76$after_night_shiftQ)
# this matches what is reported in the paper #

# here, pain was described as 8/10 #
# how many people say 80 by condition ? #
table(s1minus_sub30_70_76$after_night_shiftQ, s1minus_sub30_70_76$scenario1_painQ_1)

# look at the distributions #
back_pain_density = ggplot(data=s1minus_sub30_70_76, aes(x=scenario1_painQ_1, group=after_night_shiftQ, fill=after_night_shiftQ)) +
  geom_density(adjust=1.5, alpha=.4)

# headache #
# effect of condition on pain assessment #
wrap.t.ind(dv1=s1$scenario2_painQ_1,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario2_painQ_1,iv1=s1$after_night_shiftQ)

# there is one outlier - this person is 3SDs away from the mean #
mean(s1$scenario2_painQ_1,na.rm=T) #74.06
sd(s1$scenario2_painQ_1,na.rm=T) #16.74
# anyone below 23.84 gets removed #
# that is subject number 41 (row 30) #
# (we can actually use the data file we created before) #
wrap.t.ind(dv1=s1minus_sub41$scenario2_painQ_1,iv1=s1minus_sub41$after_night_shiftQ)
wrap.desc(dv1=s1minus_sub41$scenario2_painQ_1,iv1=s1minus_sub41$after_night_shiftQ)
# this matches what is reported in the paper #
# here, pain was described as 9/10 #
# how many people say 90 by condition ? #
table(s1minus_sub41$after_night_shiftQ, s1minus_sub41$scenario2_painQ_1)

# look at distributions #
head_pain_density = ggplot(data=s1minus_sub30_70_76, aes(x=scenario2_painQ_1, group=after_night_shiftQ, fill=after_night_shiftQ)) +
  geom_density(adjust=1.5, alpha=.4)

#### prescription measures ####

# backache #
# effect of condition on prescription #
s1$scenario1_medAvg = as.numeric(s1$scenario1_medAvg)
wrap.t.ind(dv1=s1$scenario1_medAvg,iv1=s1$after_night_shiftQ, var.equal = F)
wrap.desc(dv1=s1$scenario1_medAvg,iv1=s1$after_night_shiftQ)
# this reproduces what they have in the paper #

# look at each DV individually for backache #

# non-steroid #
s1$scenario1_medQ_1 = as.numeric(s1$scenario1_medQ_1)
wrap.t.ind(dv1=s1$scenario1_medQ_1,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario1_medQ_1,iv1=s1$after_night_shiftQ)

# oral opioid # 
s1$scenario1_medQ_2 = as.numeric(s1$scenario1_medQ_2)
wrap.t.ind(dv1=s1$scenario1_medQ_2,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario1_medQ_2,iv1=s1$after_night_shiftQ)

# intravenous opioid #
s1$scenario1_medQ_3 = as.numeric(s1$scenario1_medQ_3)
wrap.t.ind(dv1=s1$scenario1_medQ_3,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario1_medQ_3,iv1=s1$after_night_shiftQ)

# headache #
# effect of condition on prescription #
s1$scenario2_medAvg = as.numeric(s1$scenario2_medAvg)
wrap.t.ind(dv1=s1$scenario2_medAvg,iv1=s1$after_night_shiftQ, var.equal = F)
wrap.desc(dv1=s1$scenario2_medAvg,iv1=s1$after_night_shiftQ)
# this reproduces what they have in the paper #

# look at each DV individually for headache #

# non-steroid #
s1$scenario2_medQ_1 = as.numeric(s1$scenario2_medQ_1)
wrap.t.ind(dv1=s1$scenario2_medQ_1,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario2_medQ_1,iv1=s1$after_night_shiftQ)

# oral opioid # 
s1$scenario2_medQ_2 = as.numeric(s1$scenario2_medQ_2)
wrap.t.ind(dv1=s1$scenario2_medQ_2,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario2_medQ_2,iv1=s1$after_night_shiftQ)

# intravenous opioid #
s1$scenario2_medQ_3 = as.numeric(s1$scenario2_medQ_3)
wrap.t.ind(dv1=s1$scenario2_medQ_3,iv1=s1$after_night_shiftQ)
wrap.desc(dv1=s1$scenario2_medQ_3,iv1=s1$after_night_shiftQ)

# visualize effect of condition on prescription measures #

# headache #
head_prescription_density = ggplot(data=s1, aes(x=scenario2_medAvg, group=after_night_shiftQ, fill=after_night_shiftQ)) +
  geom_density(adjust=1.5, alpha=.4)

# backache #
back_prescription_density = ggplot(data=s1, aes(x=scenario1_medAvg, group=after_night_shiftQ, fill=after_night_shiftQ)) +
  geom_density(adjust=1.5, alpha=.4)

#### look at the relationships between empathy measures and pain assessment ####

# empathy for pain & pain assessment #

# backache first #

# EFP and backache #
mod2 = lm(scenario1_painQ_1 ~ EFP_Avg, data = s1)
summary(mod2)

ggplot(s1, aes(x = EFP_Avg, y = scenario1_painQ_1)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# empathic accuracy and backache #
mod3 = lm(scenario1_painQ_1 ~ EAmean_corGoodData, data = s1)
summary(mod3)

ggplot(s1, aes(x = EAmean_corGoodData, y = scenario1_painQ_1)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# headache second #

# EFP and headache #
mod4 = lm(scenario2_painQ_1 ~ EFP_Avg, data = s1)
summary(mod4)

ggplot(s1, aes(x = EFP_Avg, y = scenario2_painQ_1)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# empathic accuracy and headache #
mod5 = lm(scenario2_painQ_1 ~ EAmean_corGoodData, data = s1)
summary(mod5)

ggplot(s1, aes(x = EAmean_corGoodData, y = scenario2_painQ_1)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#### look at the relationship between empathy for pain and prescription decisions ####

# backache first #

# EFP and backache #
mod6 = lm(scenario1_medAvg ~ EFP_Avg, data = s1)
summary(mod6)

ggplot(s1, aes(x = EFP_Avg, y = scenario1_medAvg)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# empathic accuracy and backache #
mod7 = lm(scenario1_medAvg ~ EAmean_corGoodData, data = s1)
summary(mod7)

ggplot(s1, aes(x = EAmean_corGoodData, y = scenario1_medAvg)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# headache second #

# EFP and headache #
mod8 = lm(scenario2_medAvg ~ EFP_Avg, data = s1)
summary(mod8)

ggplot(s1, aes(x = EFP_Avg, y = scenario2_medAvg)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# empathic accuracy and headache #
mod8 = lm(scenario2_medAvg ~ EAmean_corGoodData, data = s1minus_sub41)
summary(mod8)

ggplot(s1, aes(x = EAmean_corGoodData, y = scenario2_medAvg)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#### run exploratory mediation models ####

# backache #
# first, create a new data set and make the IV numeric #
s1med = s1
s1med$after_night_shiftQ = as.numeric(s1med$after_night_shiftQ)

# condition -> empathy for pain -> pain assessment #
mediation1 = med(s1med, dep = "scenario1_painQ_1", med = "EFP_Avg",
                 pred = "after_night_shiftQ", estMethod = "standard", bootstrap = 10000,
                 test = TRUE, ci = TRUE, ciWidth = 95, pm = TRUE,
                 paths = TRUE, label = TRUE, estPlot = FALSE)

# condition -> empathy for pain -> prescription #
mediation2 = med(s1med, dep = "scenario1_medAvg", med = "EFP_Avg",
                 pred = "after_night_shiftQ", estMethod = "standard", bootstrap = 10000,
                 test = TRUE, ci = TRUE, ciWidth = 95, pm = TRUE,
                 paths = TRUE, label = TRUE, estPlot = FALSE)

# headache #

# condition -> empathy for pain -> assessment #
mediation3 = med(s1med, dep = "scenario2_painQ_1", med = "EFP_Avg",
                 pred = "after_night_shiftQ", estMethod = "standard", bootstrap = 10000,
                 test = TRUE, ci = TRUE, ciWidth = 95, pm = TRUE,
                 paths = TRUE, label = TRUE, estPlot = FALSE)

# condition -> empathy for pain -> prescription #
mediation4 = med(s1med, dep = "scenario2_medAvg", med = "EFP_Avg",
                 pred = "after_night_shiftQ", estMethod = "standard", bootstrap = 10000,
                 test = TRUE, ci = TRUE, ciWidth = 95, pm = TRUE,
                 paths = TRUE, label = TRUE, estPlot = FALSE)


