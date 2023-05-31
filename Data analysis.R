  library(readxl)
  library(xlsx)
  library(psych)
  library(car)
  library(ggplot2)
  library(ggExtra) # marginal plots
  library(tidyverse)
  library(gridExtra)
  library(openxlsx)
  library(margins)
  library(plotly)
  require(GGally)
  library(dplyr)
  require(tidyverse)
  library(ggeasy)
  library(cowplot)
  library(patchwork)
  library(grid)
  library(reshape2)
  library(gtable)
  library(stargazer)
  library(lmtest)
  library(sandwich)
  here::here()
  # Set your own working directory here
  setwd("/Users/efthymiakostaki/Dropbox/EffieStefan/IRP/IRP_R")
  source("fn_shccm.R")
  require(Hmisc)
  require(xtable)
  
  # skip exponential notation
  options(scipen = 999)
  
  # GP Practice data
  GP_2022.overall_satisfaction<- read_excel("2022_Practice.xlsx",  sheet= "OVERALL EXPERIENCE", range = cell_rows(11:6519))
  GP_2022.overall_satisfaction<-GP_2022.overall_satisfaction[-c(1),]
  # Retain columns of interest
  GP_2022.overall_satisfaction<- GP_2022.overall_satisfaction[, c("Practice Code","Overall experience of GP practice - Total responses", "Very good","Fairly good", "Neither good nor poor", "Fairly poor", "Very poor")]
  names(GP_2022.overall_satisfaction)
  
  GP_2022.population_characteristics<-  read_excel("2022_Practice.xlsx",  sheet = "SOME QUESTIONS ABOUT YOU", range = cell_rows(11:6519))
  GP_2022.population_characteristics<-GP_2022.population_characteristics[-c(1),]
  names(GP_2022.population_characteristics)
  # Retain columns of interest
  GP_2022.population_characteristics<- GP_2022.population_characteristics[,c("Practice Code","Gender identity - Total responses", "Female", "Male" , "Aged 16-24", "Aged 25-34", "Aged 35-44",
                                                                             "Aged 45-54", "Aged 55-64", "Aged 65-74", "Aged 75-84", "Aged 85+", "Parent or legal guardian - total responses",
                                                                             "Yes I am a parent of or a legal guardian for any children aged under 16", 
                                                                             "No  I am not a parent of or a legal guardian for any children aged under 16")]
  # -c(1) Deletes row of overall UK characteristics

GP_2022<-merge(GP_2022.overall_satisfaction, GP_2022.population_characteristics,by='Practice Code')
names(GP_2022) 

# Create new variable of proportion of female responses 
GP_2022$Female<- as.numeric(GP_2022$Female)

# Disregard cases with no wighted number of female responses or male responses
#GP_2022<- GP_2022[complete.cases(GP_2022$Female_numeric),]

GP_2022$Male<- as.numeric(GP_2022$Male)
GP_2022<- GP_2022[complete.cases(GP_2022$Male),]
GP_2022<- GP_2022[complete.cases(GP_2022$Female),]
GP_2022$proportion_of_female_responders<- GP_2022$Female/(GP_2022$Female+GP_2022$Male)

# Convert patient satisfaction and combine responses "Very good" and "Fairly good" as nominator
GP_2022$`Very good`<- as.numeric(GP_2022$`Very good`)
GP_2022<- GP_2022[complete.cases(GP_2022$`Very good`),]
GP_2022$`Fairly good`<- as.numeric(GP_2022$`Fairly good`)
GP_2022$`Very poor`<- as.numeric(GP_2022$`Very poor`)
GP_2022$`Fairly poor`<- as.numeric(GP_2022$`Fairly poor`)
GP_2022$`Neither good nor poor`<- as.numeric(GP_2022$`Neither good nor poor`)

GP_2022$patient_satisfaction<- (GP_2022$`Very good`+GP_2022$`Fairly good`)/ (GP_2022$`Very good`+GP_2022$`Fairly good`+GP_2022$`Very poor`+GP_2022$`Fairly poor`+GP_2022$`Neither good nor poor`)

lm_simple<- lm(patient_satisfaction~  proportion_of_female_responders, data=GP_2022)
summary(lm_simple)
shccm(lm_simple,"hc3")
# As the percentage of female respondents increases, so does the percentage of patient satisfaction. 

mean(GP_2022$proportion_of_female_responders)
hist(GP_2022$patient_satisfaction)
hist(GP_2022$proportion_of_female_responders)

# Regression with interaction with age 
GP_2022$`Aged 16-24`<- as.numeric(GP_2022$`Aged 16-24`)
GP_2022$`Aged 25-34`<- as.numeric(GP_2022$`Aged 25-34`)
GP_2022$`Aged 35-44`<- as.numeric(GP_2022$`Aged 35-44`)
GP_2022$`Aged 45-54`<- as.numeric(GP_2022$`Aged 45-54`)
GP_2022$`Aged 55-64`<- as.numeric(GP_2022$`Aged 55-64`)
GP_2022$`Aged 65-74`<- as.numeric(GP_2022$`Aged 65-74`)
GP_2022$`Aged 75-84`<- as.numeric(GP_2022$`Aged 75-84`)
GP_2022$`Aged 85+`<- as.numeric(GP_2022$`Aged 85+`)
GP_2022$total_age<- GP_2022$`Aged 16-24`+GP_2022$`Aged 25-34`+GP_2022$`Aged 35-44`+ GP_2022$`Aged 45-54`+GP_2022$`Aged 55-64`+GP_2022$`Aged 65-74`+GP_2022$`Aged 75-84`+ GP_2022$`Aged 85+`
GP_2022$age_20_prop<- GP_2022$`Aged 16-24`/GP_2022$total_age *20
GP_2022$age_30_prop<- GP_2022$`Aged 25-34`/GP_2022$total_age *30
GP_2022$age_40_prop<- GP_2022$`Aged 35-44`/GP_2022$total_age*40
GP_2022$age_50_prop<- GP_2022$`Aged 45-54`/GP_2022$total_age*50
GP_2022$age_60_prop<- GP_2022$`Aged 55-64`/GP_2022$total_age*60
GP_2022$age_70_prop<- GP_2022$`Aged 65-74`/GP_2022$total_age*70
GP_2022$age_80_prop<- GP_2022$`Aged 75-84`/GP_2022$total_age*80
GP_2022$age_90_prop<- GP_2022$`Aged 85+`/GP_2022$total_age*90

names(GP_2022)
GP_2022$age_average<- GP_2022$age_20_prop+GP_2022$age_30_prop+ GP_2022$age_40_prop+ GP_2022$age_50_prop+ GP_2022$age_60_prop+ GP_2022$age_70_prop+ GP_2022$age_80_prop+ GP_2022$age_90_prop
GP_2022$age_average_demeaned<- GP_2022$age_average
GP_2022$age_average<- GP_2022$age_average - mean(GP_2022$age_average)
lm_simple_age<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$age_average, data=GP_2022)
summary(lm_simple_age)
shccm(lm_simple_age,"hc3")


# Include previous appointment - Question when was your last appointment?
GP_2022.past_appointment<-  read_excel("2022_Practice.xlsx",  sheet = "YOUR LAST APPOINTMENT", range = cell_rows(11:6519))
GP_2022.past_appointment<-GP_2022.past_appointment[-c(1),]
GP_2022.past_appointment<- GP_2022.past_appointment[,c("Practice Code","In the past 3 months", "Between 3 and 6 months ago", "Between 6 and 12 months ago", "More than 12 months ago", "I haven't had an appointment since being registered with my current GP practice")]
names(GP_2022.past_appointment)

# Merge with overall dataset
GP_2022<-merge(GP_2022, GP_2022.past_appointment,by='Practice Code', all.x=T)
# need to merge with the Practice codes that were not NA
names(GP_2022) 

# Define proportion of low frequency of visit
GP_2022$`More than 12 months ago`<- as.numeric(GP_2022$`More than 12 months ago`)
GP_2022$`I haven't had an appointment since being registered with my current GP practice`<- as.numeric(GP_2022$`I haven't had an appointment since being registered with my current GP practice`)
GP_2022$`In the past 3 months`<- as.numeric(GP_2022$`In the past 3 months`)
GP_2022$`Between 3 and 6 months ago`<- as.numeric(GP_2022$`Between 3 and 6 months ago`)
GP_2022$`Between 6 and 12 months ago`<- as.numeric(GP_2022$`Between 6 and 12 months ago`)
GP_2022$total_frequency<- GP_2022$`More than 12 months ago` + GP_2022$`I haven't had an appointment since being registered with my current GP practice`+GP_2022$`In the past 3 months` + GP_2022$`Between 3 and 6 months ago`+ GP_2022$`Between 6 and 12 months ago` 
GP_2022$low_frequency_visit<- (GP_2022$`More than 12 months ago` + GP_2022$`I haven't had an appointment since being registered with my current GP practice`)/ GP_2022$total_frequency

hist(GP_2022$low_frequency_visit) # normally distributed around 25%

# simple interaction - supervision February 10th
lm_simple_age<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$low_frequency_visit, data=GP_2022)
summary(lm_simple_age)
shccm(lm_simple_age,"hc3")

lm_simple_age<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$low_frequency_visit+ GP_2022$age_average, data=GP_2022)
summary(lm_simple_age)
shccm(lm_simple_age,"hc3")
# much harder to interpret if I add interaction is too much with age and prop of female responders

lm_simple_age<- lm(patient_satisfaction~  proportion_of_female_responders*  GP_2022$age_average+GP_2022$low_frequency_visit, data=GP_2022)
summary(lm_simple_age)
shccm(lm_simple_age,"hc3")

lm_simple_age<- lm(proportion_of_female_responders~  GP_2022$age_average+GP_2022$low_frequency_visit, data=GP_2022)
summary(lm_simple_age)
shccm(lm_simple_age,"hc3")

# Regression including low frequency
lm_simple_age_frequency<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$age_average+ proportion_of_female_responders*low_frequency_visit, data=GP_2022)
summary(lm_simple_age_frequency)
shccm(lm_simple_age_frequency,"hc3")

# Interact all together
lm_simple_female_age_frequency<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$age_average*low_frequency_visit, data=GP_2022)
summary(lm_simple_female_age_frequency)
shccm(lm_simple_female_age_frequency,"hc3")

# Defining GP gender composition
GP_2022.workforce_composition<- read.csv("GPWPracticeCSV.122021/8. General Practice – December 2021 Practice Level.csv")
GP_2022.workforce_composition<- GP_2022.workforce_composition[, c("PRAC_CODE","TOTAL_PATIENTS", "TOTAL_MALE","TOTAL_FEMALE", "TOTAL_GP_FTE", "MALE_GP_FTE", 
                      "FEMALE_GP_FTE","MALE_GP_EXTGL_HC_UNDER30","MALE_GP_EXTGL_HC_30TO34","MALE_GP_EXTGL_HC_35TO39", "MALE_GP_EXTGL_HC_40TO44","MALE_GP_EXTGL_HC_45TO49",
                      "MALE_GP_EXTGL_HC_50TO54", "MALE_GP_EXTGL_HC_55TO59","MALE_GP_EXTGL_HC_60TO64","MALE_GP_EXTGL_HC_65TO69","MALE_GP_EXTGL_HC_70PLUS",
                      "MALE_GP_EXTGL_HC_UNKNOWN_AGE","FEMALE_GP_EXTGL_HC_UNDER30","FEMALE_GP_EXTGL_HC_30TO34","FEMALE_GP_EXTGL_HC_35TO39",
                      "FEMALE_GP_EXTGL_HC_40TO44","FEMALE_GP_EXTGL_HC_45TO49","FEMALE_GP_EXTGL_HC_50TO54","FEMALE_GP_EXTGL_HC_55TO59",
                      "FEMALE_GP_EXTGL_HC_60TO64","FEMALE_GP_EXTGL_HC_65TO69","FEMALE_GP_EXTGL_HC_70PLUS","FEMALE_GP_EXTGL_HC_UNKNOWN_AGE")]
names(GP_2022.workforce_composition)
names(GP_2022.workforce_composition)[names(GP_2022.workforce_composition) == 'PRAC_CODE'] <- 'Practice Code'
# rename column for merging 

# Merge with overall dataset
GP_2022<-merge(GP_2022, GP_2022.workforce_composition,by='Practice Code', all.x=T)
# need to merge with the Practice codes that were not NA
names(GP_2022) 

GP_2022$`TOTAL_MALE`<- as.numeric(GP_2022$`TOTAL_MALE`)
GP_2022$`TOTAL_FEMALE`<- as.numeric(GP_2022$`TOTAL_FEMALE`)
GP_2022$`TOTAL_PATIENTS`<- as.numeric(GP_2022$`TOTAL_PATIENTS`)
scatterplot(I(GP_2022$TOTAL_FEMALE+GP_2022$TOTAL_MALE), GP_2022$`TOTAL_PATIENTS`)
hist(I(GP_2022$`TOTAL_FEMALE`/GP_2022$`TOTAL_PATIENTS`))
# low variation in the gender of the registered patients for each practice

# Calculate proportion of female full time GPs
GP_2022$`FEMALE_GP_FTE`<- as.numeric(GP_2022$`FEMALE_GP_FTE`)
GP_2022$`MALE_GP_FTE`<- as.numeric(GP_2022$`MALE_GP_FTE`)
GP_2022$proportion_female_FTE_GPs<- GP_2022$MALE_GP_FTE/(GP_2022$MALE_GP_FTE+GP_2022$FEMALE_GP_FTE)
hist(GP_2022$proportion_female_FTE_GPs)  


# Linear regression with workforce composition
# only with age interaction - no frequency
lm_simple_age_female_GP<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$age_average+proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
summary(lm_simple_age_female_GP)
shccm(lm_simple_age_female_GP,"hc3")

lm1<- lm(patient_satisfaction~ proportion_female_FTE_GPs, data=GP_2022)
summary(lm1)
shccm(lm1,"hc3")

hist(GP_2022$age_average)

# Let's add workforce workload
GP_2022$`TOTAL_PATIENTS`<- as.numeric(GP_2022$`TOTAL_PATIENTS`)
GP_2022$`TOTAL_GP_FTE`<- as.numeric(GP_2022$`TOTAL_GP_FTE`)
GP_2022<- GP_2022[complete.cases(GP_2022$`TOTAL_GP_FTE`),]
GP_2022$practice_workload<- GP_2022$`TOTAL_PATIENTS`/GP_2022$`TOTAL_GP_FTE`

# Supervisor meeting - Delete practices with more than 4000 patients per GP

hist(log(GP_2022$practice_workload)) # remember to take the log 

# Missing data for total patients - 0 - only two rows
which(is.na(GP_2022$log_practice_workload))
hist(GP_2022[GP_2022$practice_workload<=1000, "practice_workload"])
GP_2022[GP_2022$practice_workload==0, "practice_workload"]
which(GP_2022$practice_workload==0)
GP_2022<-GP_2022[GP_2022$TOTAL_PATIENTS!=0,]
which(GP_2022$practice_workload==Inf)
GP_2022<-GP_2022[GP_2022$practice_workload!=Inf,]

# Supervisor meeting - Delete practices with more than 4000 patients per GP
GP_2022<-GP_2022[GP_2022$practice_workload<=4000,]
hist(GP_2022$practice_workload)
hist(log(GP_2022$practice_workload)) # remember to take the log - not necessary

GP_2022$log_practice_workload<- log(GP_2022$practice_workload)

# More issues missing number of FTE doctors
# let's make a simple regression
lm_simple_age_workforce<- lm(patient_satisfaction~  proportion_of_female_responders*age_average+ GP_2022$practice_workload, data=GP_2022)
summary(lm_simple_age_workforce)
shccm(lm_simple_age_workforce, "hc3")

# supervisor - interact with frequency rather than age
lm_simple_age_workforce<- lm(patient_satisfaction~  proportion_of_female_responders*low_frequency_visit+ GP_2022$practice_workload, data=GP_2022)
summary(lm_simple_age_workforce)
shccm(lm_simple_age_workforce, "hc3")

lm_simple_age_workforce<- lm(patient_satisfaction~ GP_2022$practice_workload, data=GP_2022)
summary(lm_simple_age_workforce)
shccm(lm_simple_age_workforce, "hc3")

# More histograms
hist(GP_2022$TOTAL_PATIENTS)
hist(GP_2022$TOTAL_MALE)
hist(GP_2022$TOTAL_FEMALE)
hist(GP_2022$TOTAL_GP_FTE)
hist(GP_2022$practice_workload)

# Bringing financial data
GP_2022.finance<- read.csv("NHS_Payments_to_GPs_Nov_2022.csv")
names(GP_2022.finance)[names(GP_2022.finance) == 'Practice.Code'] <- 'Practice Code'
names(GP_2022.finance)[names(GP_2022.finance) == 'Number.of.Registered.Patients..Last.Known.Figure.'] <- 'Number.of.Registered.Patients'
names(GP_2022.finance)[names(GP_2022.finance) == 'Number.of.Weighted.Patients..Last.Known.Figure.'] <- 'Number.of.Weighted.Patients'

GP_2022.finance<- GP_2022.finance[, c("Practice Code", "Practice.Rurality",
                                      "Practice.Close.Date", "CCG.Code","CCG.NAME" ,"Average.payments.per.registered.patient", 
                                      "Average.payments.per.weighted.patient", "Total.NHS.Payments.to.General.Practice",
                                      "Number.of.Registered.Patients", "Number.of.Weighted.Patients"
                                      )]
names(GP_2022.finance)
names(GP_2022)
# added registered and weighted patients to calculate patient mix
GP_2022.finance<-GP_2022.finance[GP_2022.finance$Practice.Close.Date=="",]
hist(log(GP_2022.finance$Average.payments.per.weighted.patient))
describe(GP_2022.finance)

sum(GP_2022.finance$Practice.Rurality=="Missing")

subset(GP_2022.finance,Average.payments.per.registered.patient<51)
subset(GP_2022,GP_2022$`Practice Code`=="Y03434")
typeof(GP_2022$`Practice Code`)

# Let's merge them all together and see what issues occur
# Merge with overall dataset
GP_2022<-merge(GP_2022, GP_2022.finance,by='Practice Code', all.x=T)
# need to merge with the Practice codes that were not NA
names(GP_2022) 
# delete practice with no financial data
# GP_2022<-subset(GP_2022,is.na(GP_2022$CCG.Code)==F)
# subset(GP_2022.finance,Average.payments.per.registered.patient<51)

subset(GP_2022,Average.payments.per.registered.patient<51)
subset(GP_2022,GP_2022$`Practice Code`=="Y03434")
hist(GP_2022$Average.payments.per.weighted.patient)
describe(GP_2022)
densityPlot(GP_2022$Average.payments.per.weighted.patient)

# Let's use marginal plots in a simple regression
lm_simple_age<- lm(patient_satisfaction~  proportion_of_female_responders*GP_2022$low_frequency_visit, data=GP_2022)
summary(lm_simple_age)

# create scatter plot using ggplot() function
plot <- ggplot(GP_2022, aes(x=proportion_of_female_responders, y=patient_satisfaction))+
  geom_point()+
  theme(legend.position="none")

# use ggMarginal function to create
# marginal histogram, boxplot and density plot
plot1 <- ggMarginal(plot, type="histogram")
plot2 <- ggMarginal(plot, type="boxplot")
plot3 <- ggMarginal(plot, type="density")

# combine plots in a grid
grid.arrange( plot1, plot2, plot3, ncol=3)

# Giving marginal plots another try to resemble STATA outcome
# simple model with age average mediator
simple_lm<-lm(patient_satisfaction~proportion_of_female_responders*age_average, data=GP_2022)
newdata = data.frame(proportion_of_female_responders=c(0,0,0,0,0,1,1,1,1,1), 
                     age_average=c(-10,-5,0,5,10, -10,-5,0,5,10))
newdata
p<-predict(simple_lm, newdata, interval="confidence") ;
p<- cbind(p,newdata)
p

# Plot
hist(GP_2022$age_average,breaks=100, freq=F, xlim=c(-10,10), main = paste("Adjusted predictions with 95% CIs"))
par(new = TRUE) 
require(plotrix)
newdata$proportion_of_female_responders<- as.factor(newdata$proportion_of_female_responders)
plot(newdata[newdata$proportion_of_female_responders==0,]$age_average, 
     p[p$proportion_of_female_responders==0,1],type="b",
     pch = 16, col ="black",# Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
lines(newdata[newdata$proportion_of_female_responders==1,]$age_average, 
      p[p$proportion_of_female_responders==1,1],type="b", 
      pch = 16, col ="red", xlab = "", ylab = "")# Create second plot without axes
par(new = TRUE) 
# plotCI(newdata$age_average, p[,1], ui=p[,2], li=p[,3],pch=16,gap=0.02,axes = FALSE, xlab = "", ylab = "", col =newdata$proportion_of_female_responders)
arrows(newdata$age_average, p[,2], newdata$age_average, p[,3], 
       length=0.05, angle=90, code=3,
       col =newdata$proportion_of_female_responders)

axis(side = 4, at = pretty(range(p[,1])))# Add second axis
mtext("Linear prediction", side = 4)  
legend("topleft", legend = c("Male only", "Female only"),
       col = c("black", "red"), lty = 1, cex =1)

# Individual work 20th of February 2022
# Workforce composition additions
# Find average age of female and male GPs for each GP practice
GP_2022$MALE_GP_EXTGL_HC_UNDER30<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_UNDER30)
GP_2022$MALE_GP_EXTGL_HC_30TO34<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_30TO34)
GP_2022$MALE_GP_EXTGL_HC_35TO39<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_35TO39)
GP_2022$MALE_GP_EXTGL_HC_40TO44<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_40TO44)
GP_2022$MALE_GP_EXTGL_HC_45TO49<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_45TO49)
GP_2022$MALE_GP_EXTGL_HC_50TO54<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_50TO54)
GP_2022$MALE_GP_EXTGL_HC_55TO59<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_55TO59)
GP_2022$MALE_GP_EXTGL_HC_60TO64<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_60TO64)
GP_2022$MALE_GP_EXTGL_HC_65TO69<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_65TO69)
GP_2022$MALE_GP_EXTGL_HC_70PLUS<- as.numeric(GP_2022$MALE_GP_EXTGL_HC_70PLUS)

GP_2022$TOTAL_MALE_GP_EXTGL_HC<- GP_2022$MALE_GP_EXTGL_HC_UNDER30+GP_2022$MALE_GP_EXTGL_HC_30TO34+GP_2022$MALE_GP_EXTGL_HC_35TO39+
                                  GP_2022$MALE_GP_EXTGL_HC_40TO44+GP_2022$MALE_GP_EXTGL_HC_45TO49+GP_2022$MALE_GP_EXTGL_HC_50TO54+
                                  GP_2022$MALE_GP_EXTGL_HC_55TO59+ GP_2022$MALE_GP_EXTGL_HC_60TO64+GP_2022$MALE_GP_EXTGL_HC_65TO69+
                                  GP_2022$MALE_GP_EXTGL_HC_70PLUS
hist(GP_2022$TOTAL_MALE_GP_EXTGL_HC)

GP_2022$male_gp_age_UNDER30_prop<-  GP_2022$MALE_GP_EXTGL_HC_UNDER30/GP_2022$TOTAL_MALE_GP_EXTGL_HC *28
GP_2022$male_gp_age_32_prop<-  GP_2022$MALE_GP_EXTGL_HC_30TO34/GP_2022$TOTAL_MALE_GP_EXTGL_HC *32
GP_2022$male_gp_age_37_prop<-  GP_2022$MALE_GP_EXTGL_HC_35TO39/GP_2022$TOTAL_MALE_GP_EXTGL_HC *37
GP_2022$male_gp_age_42_prop<-  GP_2022$MALE_GP_EXTGL_HC_40TO44/GP_2022$TOTAL_MALE_GP_EXTGL_HC *42
GP_2022$male_gp_age_47_prop<-  GP_2022$MALE_GP_EXTGL_HC_45TO49/GP_2022$TOTAL_MALE_GP_EXTGL_HC *47
GP_2022$male_gp_age_52_prop<-  GP_2022$MALE_GP_EXTGL_HC_50TO54/GP_2022$TOTAL_MALE_GP_EXTGL_HC *52
GP_2022$male_gp_age_57_prop<-  GP_2022$MALE_GP_EXTGL_HC_55TO59/GP_2022$TOTAL_MALE_GP_EXTGL_HC *57
GP_2022$male_gp_age_62_prop<-  GP_2022$MALE_GP_EXTGL_HC_60TO64/GP_2022$TOTAL_MALE_GP_EXTGL_HC *62
GP_2022$male_gp_age_67_prop<-  GP_2022$MALE_GP_EXTGL_HC_65TO69/GP_2022$TOTAL_MALE_GP_EXTGL_HC *67
GP_2022$male_gp_age_72_prop<-  GP_2022$MALE_GP_EXTGL_HC_70PLUS/GP_2022$TOTAL_MALE_GP_EXTGL_HC *72

GP_2022$male_gp_average_age<- GP_2022$male_gp_age_UNDER30_prop+GP_2022$male_gp_age_32_prop+GP_2022$male_gp_age_37_prop+
                              GP_2022$male_gp_age_42_prop+GP_2022$male_gp_age_47_prop+GP_2022$male_gp_age_52_prop+
                              GP_2022$male_gp_age_57_prop+GP_2022$male_gp_age_62_prop+GP_2022$male_gp_age_67_prop+GP_2022$male_gp_age_72_prop
hist(GP_2022$male_gp_average_age)

GP_2022$FEMALE_GP_EXTGL_HC_UNDER30<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_UNDER30)
GP_2022$FEMALE_GP_EXTGL_HC_30TO34<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_30TO34)
GP_2022$FEMALE_GP_EXTGL_HC_35TO39<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_35TO39)
GP_2022$FEMALE_GP_EXTGL_HC_40TO44<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_40TO44)
GP_2022$FEMALE_GP_EXTGL_HC_45TO49<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_45TO49)
GP_2022$FEMALE_GP_EXTGL_HC_50TO54<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_50TO54)
GP_2022$FEMALE_GP_EXTGL_HC_55TO59<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_55TO59)
GP_2022$FEMALE_GP_EXTGL_HC_60TO64<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_60TO64)
GP_2022$FEMALE_GP_EXTGL_HC_65TO69<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_65TO69)
GP_2022$FEMALE_GP_EXTGL_HC_70PLUS<- as.numeric(GP_2022$FEMALE_GP_EXTGL_HC_70PLUS)

GP_2022$TOTAL_FEMALE_GP_EXTGL_HC<- GP_2022$FEMALE_GP_EXTGL_HC_UNDER30+GP_2022$FEMALE_GP_EXTGL_HC_30TO34+GP_2022$FEMALE_GP_EXTGL_HC_35TO39+
  GP_2022$FEMALE_GP_EXTGL_HC_40TO44+GP_2022$FEMALE_GP_EXTGL_HC_45TO49+GP_2022$FEMALE_GP_EXTGL_HC_50TO54+GP_2022$FEMALE_GP_EXTGL_HC_55TO59+
  GP_2022$FEMALE_GP_EXTGL_HC_60TO64+GP_2022$FEMALE_GP_EXTGL_HC_65TO69+GP_2022$FEMALE_GP_EXTGL_HC_70PLUS

GP_2022$female_gp_age_UNDER30_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_UNDER30/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *28
GP_2022$female_gp_age_32_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_30TO34/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *32
GP_2022$female_gp_age_37_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_35TO39/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *37
GP_2022$female_gp_age_42_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_40TO44/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *42
GP_2022$female_gp_age_47_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_45TO49/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *47
GP_2022$female_gp_age_52_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_50TO54/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *52
GP_2022$female_gp_age_57_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_55TO59/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *57
GP_2022$female_gp_age_62_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_60TO64/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *62
GP_2022$female_gp_age_67_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_65TO69/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *67
GP_2022$female_gp_age_72_prop<-  GP_2022$FEMALE_GP_EXTGL_HC_70PLUS/GP_2022$TOTAL_FEMALE_GP_EXTGL_HC *72

GP_2022$female_gp_average_age<- GP_2022$female_gp_age_UNDER30_prop+GP_2022$female_gp_age_32_prop+GP_2022$female_gp_age_37_prop+
  GP_2022$female_gp_age_42_prop+GP_2022$female_gp_age_47_prop+GP_2022$female_gp_age_52_prop+GP_2022$female_gp_age_57_prop+
  GP_2022$female_gp_age_62_prop+GP_2022$female_gp_age_67_prop+GP_2022$female_gp_age_72_prop

# Some useful histograms
hist(GP_2022$male_gp_average_age)
hist(GP_2022$female_gp_average_age)

summary(GP_2022$male_gp_average_age)
summary(GP_2022$female_gp_average_age)
summary(GP_2022$TOTAL_FEMALE_GP_EXTGL_HC==0)
summary(GP_2022$TOTAL_MALE_GP_EXTGL_HC==0 )
summary(GP_2022$TOTAL_MALE_GP_EXTGL_HC==0 | GP_2022$TOTAL_FEMALE_GP_EXTGL_HC==0)
summary(GP_2022$TOTAL_MALE_GP_EXTGL_HC==0 & GP_2022$TOTAL_FEMALE_GP_EXTGL_HC==0) # for only 18 GPs both are zero we should remove them

GP_2022<- GP_2022[!(GP_2022$TOTAL_MALE_GP_EXTGL_HC==0 & GP_2022$TOTAL_FEMALE_GP_EXTGL_HC==0),]
## Subset the data to include only practices positive number of female doctors
GP_2022.sub.positive_n_female_GP<- subset(GP_2022,TOTAL_FEMALE_GP_EXTGL_HC>0 )
# Demean age averages
GP_2022.sub.positive_n_female_GP$female_gp_average_age<- GP_2022.sub.positive_n_female_GP$female_gp_average_age- 
                                                        mean(GP_2022.sub.positive_n_female_GP$female_gp_average_age)

lm_female_GP<- lm(patient_satisfaction~female_gp_average_age, data=GP_2022.sub.positive_n_female_GP)
shccm(lm_female_GP,"hc3")


## Subset the data to include only practices positive number of male doctors
GP_2022.sub.positive_n_male_GP<- subset(GP_2022,TOTAL_MALE_GP_EXTGL_HC>0 )
# Demean age averages
GP_2022.sub.positive_n_male_GP$male_gp_average_age<- GP_2022.sub.positive_n_male_GP$male_gp_average_age- 
  mean(GP_2022.sub.positive_n_male_GP$male_gp_average_age)

lm_male_GP<- lm(patient_satisfaction~ male_gp_average_age, data=GP_2022.sub.positive_n_male_GP)
shccm(lm_male_GP,"hc3")


## Subset the data to include only practices positive number of both male and female doctors
GP_2022.sub.positive_n_GP<- subset(GP_2022,TOTAL_MALE_GP_EXTGL_HC>0 &TOTAL_FEMALE_GP_EXTGL_HC>0 )
# Demean age averages
GP_2022.sub.positive_n_GP$male_gp_average_age<- GP_2022.sub.positive_n_GP$male_gp_average_age- 
  mean(GP_2022.sub.positive_n_GP$male_gp_average_age)
GP_2022.sub.positive_n_GP$female_gp_average_age<- GP_2022.sub.positive_n_GP$female_gp_average_age- 
  mean(GP_2022.sub.positive_n_GP$female_gp_average_age)

lm_male_GP<- lm(patient_satisfaction~ male_gp_average_age+female_gp_average_age, data=GP_2022.sub.positive_n_GP)
shccm(lm_male_GP,"hc3")


## Rurality

# Let's create a dummy variable for rurality
sum(GP_2022$Practice.Rurality=="Missing")
table(GP_2022$Practice.Rurality)

# For 70 practices the rurality is missing 
# We will subset the dataset not to include these practices

GP_2022<- GP_2022[GP_2022$Practice.Rurality %in% c("Rural","Urban"),]
GP_2022$Is_GP_Rural<- ifelse(GP_2022$Practice.Rurality=="Rural", 1, 0)
table(GP_2022$Is_GP_Rural)

lm_rurality<- lm(patient_satisfaction~ proportion_female_FTE_GPs+practice_workload, data=GP_2022)
shccm(lm_rurality,"hc3")

lm_rurality<- lm(patient_satisfaction~ Is_GP_Rural+proportion_female_FTE_GPs+practice_workload, data=GP_2022)
shccm(lm_rurality,"hc3")

### Adding parents 
GP_2022$Is_Legal_Guardian<- as.numeric(GP_2022$`Yes I am a parent of or a legal guardian for any children aged under 16`)
GP_2022$Is_Not_Legal_Guardian<-as.numeric(GP_2022$`No  I am not a parent of or a legal guardian for any children aged under 16`)
hist(GP_2022$Is_Legal_Guardian)
hist(GP_2022$Is_Not_Legal_Guardian)
GP_2022$prop_legal_guardians<- GP_2022$Is_Legal_Guardian/(GP_2022$Is_Not_Legal_Guardian+GP_2022$Is_Legal_Guardian)
hist(GP_2022$prop_legal_guardians)

# Let's run a simple regression on patient satisfaction
lm_legal_guardian<- lm(patient_satisfaction~prop_legal_guardians+proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_legal_guardian,"hc3")

# Let's run a simple regression on patient satisfaction
lm_legal_guardian.1<- lm(prop_legal_guardians~proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_legal_guardian.1,"hc3")

## Adding patient mix risk score
names(GP_2022)
GP_2022$risk_score<- GP_2022$Number.of.Weighted.Patients/GP_2022$Number.of.Registered.Patients
hist(GP_2022$risk_score)  
describe(GP_2022$risk_score)

# simple regression with risk score
lm_risk<- lm(patient_satisfaction~risk_score,data=GP_2022)
shccm(lm_risk, "hc3")

lm_risk<- lm(patient_satisfaction~risk_score+low_frequency_visit,data=GP_2022)
shccm(lm_risk, "hc3")

lm_risk<- lm(patient_satisfaction~risk_score*I(1-low_frequency_visit),data=GP_2022)
shccm(lm_risk, "hc3")

## Add quality framework
GP_2022.quality<- read_excel("qof-2122.xlsx",  sheet= "Overall domain achievement", range = cell_rows(12:6482))
names(GP_2022.quality)
GP_2022.quality<- GP_2022.quality[, c("Practice code","Points achieved/ max. QOF points available (%)...13")]
names(GP_2022.quality)[names(GP_2022.quality) == 'Practice code'] <- 'Practice Code'
names(GP_2022.quality)[names(GP_2022.quality) == 'Points achieved/ max. QOF points available (%)...13'] <- 'QOF_Score'

hist(GP_2022.quality$QOF_Score)
describe(GP_2022.quality$QOF_Score)

GP_2022<-merge(GP_2022, GP_2022.quality,by='Practice Code', all.x=T)

# Simple regression
lm_qof<- lm(patient_satisfaction~QOF_Score ,data=GP_2022)
shccm(lm_qof,"hc3")

## Exploring different dependent variables

# Making an appointment sheet
GP_2022.appointment<-NULL
GP_2022.appointment<- read_excel("2022_Practice.xlsx",  sheet= "MAKING AN APPOINTMENT", range = cell_rows(11:6519))
GP_2022.appointment<-GP_2022.appointment[-c(1),]
# Retain columns of interest
GP_2022.appointment<- GP_2022.appointment[, c("Practice Code","Overall experience of making an appointment - Total responses", "Very good","Fairly good", "Neither good nor poor", "Fairly poor", "Very poor")]
names(GP_2022.appointment)

GP_2022<-merge(GP_2022, GP_2022.appointment,by='Practice Code', all.x=T)
names(GP_2022) 

# create dummy variable for positive appointment satisfaction
GP_2022$`Very good.y`<- as.numeric(GP_2022$`Very good.y`)
GP_2022<- GP_2022[complete.cases(GP_2022$`Very good.y`),] # only 1 missing
GP_2022$`Fairly good.y`<- as.numeric(GP_2022$`Fairly good.y`)
GP_2022$`Very poor.y`<- as.numeric(GP_2022$`Very poor.y`)
GP_2022$`Fairly poor.y`<- as.numeric(GP_2022$`Fairly poor.y`)
GP_2022$`Neither good nor poor.y`<- as.numeric(GP_2022$`Neither good nor poor.y`)

GP_2022$patient_appointment_satisfaction<- (GP_2022$`Very good.y`+GP_2022$`Fairly good.y`)/ (GP_2022$`Very good.y`+GP_2022$`Fairly good.y`+GP_2022$`Very poor.y`+GP_2022$`Fairly poor.y`+GP_2022$`Neither good nor poor.y`)

hist(GP_2022$patient_appointment_satisfaction)

# Simple regression
lm_appointment<- lm(patient_appointment_satisfaction~proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_appointment,"hc3")

# Simple regression - add interaction with frequency of visit
lm_appointment<- lm(patient_appointment_satisfaction~proportion_of_female_responders*low_frequency_visit,data=GP_2022)
shccm(lm_appointment,"hc3")

# Simple regression - add  frequency of visit
lm_appointment<- lm(patient_appointment_satisfaction~proportion_of_female_responders*age_average+low_frequency_visit,data=GP_2022)
shccm(lm_appointment,"hc3")

# correlation patient satisfaction
lm_appointment<- lm(patient_appointment_satisfaction~patient_satisfaction,data=GP_2022)
shccm(lm_appointment,"hc3")

# Include previous appointment - Question when was your last appointment?
GP_2022.past_appointment.1<-  read_excel("2022_Practice.xlsx",  sheet = "YOUR LAST APPOINTMENT", range = cell_rows(11:6519))
GP_2022.past_appointmen.1<-GP_2022.past_appointment.1[-c(1),]
GP_2022.past_appointment.1<- GP_2022.past_appointment.1[,c("Practice Code","Did appointment happen at the time, or during the slot, given - Total responses",
                                                       "It was earlier than the time or slot I was given",
                                                       "It was on time or during the slot I was given",
                                                       "It was later than the time or slot I was given",
                                                       "Rating of healthcare professional giving you enough time - Total responses",
                                                       "Very good...79",
                                                       "Good...80",
                                                       "Neither good nor poor...81",
                                                       "Poor...82",
                                                       "Very poor...83",
                                                       "Rating of healthcare professional listening to you - Total responses"   ,
                                                       "Very good...95" ,
                                                       "Good...96" ,
                                                       "Neither good nor poor...97",
                                                       "Poor...98",
                                                       "Very poor...99" ,
                                                       "Rating of healthcare professional treating you with care and concern - Total responses",
                                                       "Very good...111" ,
                                                       "Good...112" ,
                                                       "Neither good nor poor...113" ,
                                                       "Poor...114" ,
                                                       "Very poor...115"  ,
                                                       "Confidence and trust in the healthcare professional - Total responses"   ,
                                                       "Yes, definitely...152" ,
                                                       "Yes, to some extent...153"  ,
                                                       "No, not at all...154"  ,
                                                       "Don't know / can't say...155",
                                                       "Were needs met - Total responses",
                                                       "Yes, definitely...164" ,
                                                       "Yes, to some extent...165",
                                                       "No, not at all...166"  ,
                                                       "Don't know / can't say...167" 
                                                       )]
names(GP_2022.past_appointment.1)

# Merge
GP_2022<-merge(GP_2022, GP_2022.past_appointment.1,by='Practice Code', all.x=T)
names(GP_2022) 

# Dependent variable = last appointment on time
GP_2022$`It was earlier than the time or slot I was given`<- as.numeric(GP_2022$`It was earlier than the time or slot I was given`)
sum(is.na(GP_2022$`It was earlier than the time or slot I was given`)) # 1 NA
GP_2022<- GP_2022[complete.cases(GP_2022$`It was earlier than the time or slot I was given`),]
GP_2022$`It was on time or during the slot I was given`<- as.numeric(GP_2022$`It was on time or during the slot I was given`)
GP_2022$`It was later than the time or slot I was given`<- as.numeric(GP_2022$`It was later than the time or slot I was given`)

GP_2022$last_appointment_late<-GP_2022$`It was later than the time or slot I was given`/(GP_2022$`It was later than the time or slot I was given`+
                                                                                           GP_2022$`It was on time or during the slot I was given`+
                                                                                           GP_2022$`It was earlier than the time or slot I was given`)
hist(GP_2022$last_appointment_late)

# Simple regression: late appointment and overall patient satisfaction
lm_late<- lm(patient_satisfaction~ last_appointment_late,data=GP_2022)
shccm(lm_late, "hc3")

# Regression: last_appointment_late ~ proportion of female responders *age
lm_late<- lm(last_appointment_late~proportion_of_female_responders*age_average, data=GP_2022)
shccm(lm_late, "hc3")
linearHypothesis(lm_late, c("proportion_of_female_responders","age_average", "proportion_of_female_responders:age_average"),test="Chisq")

lm_late<- lm(last_appointment_late~proportion_of_female_responders*low_frequency_visit, data=GP_2022)
shccm(lm_late, "hc3")

lm_late<- lm(last_appointment_late~Is_Legal_Guardian, data=GP_2022)
shccm(lm_late, "hc3")

# Let's look at the rurality of the practice
lm_late_rurality<- lm(last_appointment_late~Is_GP_Rural, data=GP_2022)
shccm(lm_late_rurality, "hc3")

# Dependent variable = rating of healthcare professional giving you time
GP_2022$`Very good...79`<- as.numeric(GP_2022$`Very good...79`)
GP_2022$`Good...80`<- as.numeric(GP_2022$`Good...80`)
GP_2022$`Neither good nor poor...81`<- as.numeric(GP_2022$`Neither good nor poor...81`)
GP_2022$`Poor...82`<- as.numeric(GP_2022$`Poor...82`)
GP_2022$`Very poor...83`<- as.numeric(GP_2022$`Very poor...83`)

GP_2022$not_enough_time<- (GP_2022$`Poor...82`+GP_2022$`Very poor...83`)/
                          (GP_2022$`Poor...82`+GP_2022$`Very poor...83`+GP_2022$`Neither good nor poor...81`+GP_2022$`Good...80`+GP_2022$`Very good...79`)
hist(GP_2022$not_enough_time)

lm_time<- lm(patient_satisfaction~not_enough_time,data=GP_2022)
shccm(lm_time,"hc3")

lm_time<- lm(not_enough_time~GP_2022$proportion_of_female_responders*GP_2022$age_average,data=GP_2022)
shccm(lm_time,"hc3")

lm_time<- lm(not_enough_time~Is_Legal_Guardian,data=GP_2022)
shccm(lm_time,"hc3")

# Dependent variable: healthcare professional listening to you
names(GP_2022)
GP_2022$`Very good...95`<- as.numeric(GP_2022$`Very good...95`)
GP_2022$`Good...96`<- as.numeric(GP_2022$`Good...96`)
GP_2022$`Neither good nor poor...97`<- as.numeric(GP_2022$`Neither good nor poor...97`)
GP_2022$`Poor...98`<- as.numeric(GP_2022$`Poor...98`)
GP_2022$`Very poor...99`<- as.numeric(GP_2022$`Very poor...99`)

GP_2022$not_enough_listening<- (GP_2022$`Poor...98`+GP_2022$`Very poor...99`)/
  (GP_2022$`Poor...98`+GP_2022$`Very poor...99`+GP_2022$`Neither good nor poor...97`+GP_2022$`Good...96`+GP_2022$`Very good...95`)
hist(GP_2022$not_enough_listening)

lm_listen<- lm(patient_satisfaction~ not_enough_listening,data=GP_2022)
shccm(lm_listen,"hc3")

lm_listen<- lm(not_enough_listening~ proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_listen,"hc3")

lm_listen<- lm(not_enough_listening~ low_frequency_visit,data=GP_2022)
shccm(lm_listen,"hc3")

lm_listen<- lm(not_enough_listening~ Is_Legal_Guardian,data=GP_2022)
shccm(lm_listen,"hc3")

# Dependent variable: healthcare professional treated with care and concern
GP_2022$`Very good...111`<- as.numeric(GP_2022$`Very good...111`)
GP_2022$`Good...112`<- as.numeric(GP_2022$`Good...112`)
GP_2022$`Neither good nor poor...113`<- as.numeric(GP_2022$`Neither good nor poor...113`)
GP_2022$`Poor...114`<- as.numeric(GP_2022$`Poor...114`)
GP_2022$`Very poor...115`<- as.numeric(GP_2022$`Very poor...115`)

GP_2022$not_enough_care_concern<- (GP_2022$`Poor...114`+GP_2022$`Very poor...115`)/
  (GP_2022$`Poor...114`+GP_2022$`Very poor...115`+GP_2022$`Neither good nor poor...113`+GP_2022$`Good...112`+GP_2022$`Very good...111`)
hist(GP_2022$not_enough_care_concern)

# create scatter plot using ggplot() function
plot <- ggplot(GP_2022, aes(x=proportion_of_female_responders, y=patient_satisfaction))+
  geom_point()+
  theme(legend.position="none")

# use ggMarginal function to create
# marginal histogram, boxplot and density plot
plot1 <- ggMarginal(plot, type="histogram")
plot2 <- ggMarginal(plot, type="boxplot")
plot3 <- ggMarginal(plot, type="density")

# combine plots in a grid
grid.arrange( plot1, plot2, plot3, ncol=3)

lm_care<- lm(patient_satisfaction~ not_enough_care_concern,data=GP_2022)
shccm(lm_care,"hc3")

lm_care<- lm(not_enough_care_concern~ proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_care,"hc3")

lm_care<- lm(not_enough_care_concern~ low_frequency_visit,data=GP_2022)
shccm(lm_care,"hc3")

lm_care<- lm(not_enough_care_concern~ Is_Legal_Guardian,data=GP_2022)
shccm(lm_care,"hc3")

# Dependent variable: healthcare professional confidence and trust
GP_2022$`Yes, definitely...152`<- as.numeric(GP_2022$`Yes, definitely...152`)
GP_2022$`Yes, to some extent...153`<- as.numeric(GP_2022$`Yes, to some extent...153`)
GP_2022$`No, not at all...154`<- as.numeric(GP_2022$`No, not at all...154`)

GP_2022$not_enough_trust<-GP_2022$`No, not at all...154`/(GP_2022$`No, not at all...154`+GP_2022$`Yes, definitely...152`+GP_2022$`Yes, to some extent...153`)
hist(GP_2022$not_enough_trust)

# Simple regression correlation
lm_trust<- lm(patient_satisfaction~ not_enough_trust,data=GP_2022)
shccm(lm_trust,"hc3")

lm_trust<- lm(not_enough_trust~ proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_trust,"hc3")

lm_trust<- lm(not_enough_trust~ low_frequency_visit,data=GP_2022)
shccm(lm_trust,"hc3")

lm_trust<- lm(not_enough_trust~ Is_Legal_Guardian,data=GP_2022)
shccm(lm_trust,"hc3")


# Dependent variable: were needs met
GP_2022$`Yes, definitely...164`<- as.numeric(GP_2022$`Yes, definitely...164`)
GP_2022$`Yes, to some extent...165`<- as.numeric(GP_2022$`Yes, to some extent...165`)
GP_2022$`No, not at all...166`<- as.numeric(GP_2022$`No, not at all...166`)

GP_2022$needs_not_met<-GP_2022$`No, not at all...166`/(GP_2022$`No, not at all...166`+GP_2022$`Yes, definitely...164`+GP_2022$`Yes, to some extent...165`)
hist(GP_2022$needs_not_met)

# Simple regression correlation
lm_needs<- lm(patient_satisfaction~ needs_not_met,data=GP_2022)
shccm(lm_needs,"hc3")

lm_needs<- lm(needs_not_met~ proportion_of_female_responders*age_average,data=GP_2022)
shccm(lm_needs,"hc3")

lm_needs<- lm(needs_not_met~ low_frequency_visit,data=GP_2022)
shccm(lm_needs,"hc3")

lm_needs<- lm(needs_not_met~ Is_Legal_Guardian,data=GP_2022)
shccm(lm_needs,"hc3")

# GP your health
GP_2022.health<- read_excel("2022_Practice.xlsx",  sheet= "YOUR HEALTH", range = cell_rows(11:6519))
GP_2022.health<-GP_2022.health[-c(1),]
# Retain columns of interest
GP_2022.health<- GP_2022.health[, c("Practice Code",
                                "Long-term physical or mental health condition, disability or illness - Total responses",
                                "Yes",
                                "No...22" ,  
                                "Had enough support from local services - Total responses" ,
                                "Yes, definitely"    ,
                                "Yes, to some extent" ,
                                "No...104" 
                                    )]
names(GP_2022.health)
names(GP_2022)
# Merge with overall dataset
GP_2022<-merge(GP_2022, GP_2022.health,by='Practice Code', all.x=T)

GP_2022$`Yes`<- as.numeric(GP_2022$`Yes`)
GP_2022$`No...22`<- as.numeric(GP_2022$`No...22`)
GP_2022$prop_long_term_condition<- GP_2022$`Yes`/(GP_2022$`Yes`+GP_2022$`No...22`)
hist(GP_2022$prop_long_term_condition)

# simple regression
lm_long<-lm(prop_long_term_condition ~ risk_score,data=GP_2022)
shccm(lm_long,"hc3")

lm_long<-lm(patient_satisfaction ~prop_long_term_condition + risk_score,data=GP_2022)
shccm(lm_long,"hc3")
vif(lm_long)

hist(GP_2022$QOF_Score)
lm_long<-lm(patient_satisfaction ~practice_workload* risk_score,data=GP_2022)
shccm(lm_long,"hc3")
vif(lm_long)

lm_1<- lm(patient_satisfaction~proportion_of_female_responders+age_average+low_frequency_visit+Is_Legal_Guardian, data=GP_2022)

shccm(lm_1, "hc3")

# Save GP_2022 to excel
write.xlsx(GP_2022, 'GP_2022_Data.xlsx')

# Time to use the margins library
# lm_1<- lm(patient_satisfaction~proportion_of_female_responders, data=GP_2022)
# m <- margins(lm_1)
# summary(m)  
# margins_summary(lm_1)  
# plot(m)

# cplot(lm_1, "proportion_of_female_responders", what = "effect", main = "Average Marginal Effect of Proportion of female responders")
# cplot(lm_1, "proportion_of_female_responders", what = "prediction", main = "Predicted patient satisfaction, given proportion of female responders")

# lm_2<- lm(patient_satisfaction~proportion_of_female_responders, data=GP_2022)

## Supervisor comments break QOF score into bins and then conduct multinomial regression
# Drop outlier- done
# GP_2022$QOF_Score[GP_2022$QOF_Score ==max(GP_2022$QOF_Score, na.rm=T)] <- NA
summary(GP_2022$QOF_Score)
quantile(GP_2022$QOF_Score, probs=c(0.25,0.75), na.rm=TRUE)
IQR(GP_2022$QOF_Score,  na.rm=TRUE)
dim(as.matrix(GP_2022[,c("QOF_Score")]))
q<-quantile(GP_2022$QOF_Score, probs = seq(from=0.0000, to=1, by=0.1), na.rm = T);q

numbers_of_bins = 10
GP_2022<-GP_2022%>%mutate(QOF_Score_bins = cut(QOF_Score,
                                              breaks = 
                                              unique(quantile(QOF_Score,
                                              probs=seq.int(0,1, 
                                              by=1/numbers_of_bins), na.rm=T)),
                                              include.lowest = T))
                              
summary(GP_2022$QOF_Score_bins)
# GP_2022[is.na(GP_2022$QOF_Score_bins)==T,c("QOF_Score_bins","QOF_Score")]
GP_2022%>%group_by(QOF_Score_bins)%>%count()

summary(ntile(GP_2022$QOF_Score, n=10))

# make int bins to use in regression
GP_2022<-GP_2022%>%mutate(QOF_Score_bins_int =  ntile(QOF_Score, n=10),include.lowest = T) 

head(GP_2022[c("QOF_Score_bins_int", "QOF_Score_bins")])

diff(q , lag = 1 ) # length of each bin
diff(diff(q , lag = 1 ),lag=1) # differences in lengths

# Regression
GP_2022$QOF_Score_bins_int<- as.factor(GP_2022$QOF_Score_bins_int)
shccm(lm(patient_satisfaction~QOF_Score_bins_int, data= GP_2022))

plot(GP_2022$QOF_Score_bins_int, GP_2022$patient_satisfaction)
abline(lm(patient_satisfaction~QOF_Score_bins_int, data= GP_2022), col = "blue")


## Tom's comment run regression to understand how each factor explains patient satisfaction
ggpairs(GP_2022[,c("proportion_of_female_responders", 
                   "patient_satisfaction", 
                   "proportion_female_FTE_GPs", 
                   "age_average",
                   "female_gp_average_age",
                   "practice_workload","low_frequency_visit")])
ggpairs(GP_2022[,c("proportion_of_female_responders", 
                   "patient_satisfaction", 
                   "proportion_female_FTE_GPs", 
                   "age_average",
                   "female_gp_average_age",
                   "practice_workload","low_frequency_visit")], 
        columnLabels = gsub('_', ' ', colnames(GP_2022[,c("proportion_of_female_responders", 
                                                          "patient_satisfaction", 
                                                          "proportion_female_FTE_GPs", 
                                                          "age_average",
                                                          "female_gp_average_age",
                                                          "practice_workload","low_frequency_visit")]), 
        fixed = T), 
        labeller = label_wrap_gen(10))
ggpairs(GP_2022[, c("patient_satisfaction","proportion_of_female_responders", "patient_appointment_satisfaction",
                    "last_appointment_late", "not_enough_time","not_enough_listening",
                    "not_enough_care_concern","not_enough_trust", "needs_not_met")])
xtable(cor(GP_2022[, c("not_enough_time","not_enough_listening",
                "not_enough_care_concern","not_enough_trust", "needs_not_met")], use = "complete.obs"))

factors<- lm(patient_satisfaction~patient_appointment_satisfaction+
     last_appointment_late+ not_enough_time+not_enough_listening+
     not_enough_care_concern+not_enough_trust+ needs_not_met, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
names(GP_2022)

plot(GP_2022$prop_legal_guardians, GP_2022$proportion_of_female_responders)
abline(lm(proportion_of_female_responders~prop_legal_guardians*age_average, data=GP_2022))

factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_listening*proportion_of_female_responders, data=GP_2022)
shccm(factors, "hc3")


factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_time*proportion_of_female_responders+ not_enough_listening, data=GP_2022)
shccm(factors, "hc3")

# creating the 2*2 matrices for mechanisms
# Not enough time
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_time*proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
confint(factors)

# 25% and 75% quantile of proportion of female doctors FTEs
q<- quantile(GP_2022$proportion_female_FTE_GPs, probs = c(0.25,0.75), na.rm = T)
q[1]<- round(q[1],3)
q[2]<- round(q[2],3)

# quantile for not enough time
time_q<- quantile(GP_2022$not_enough_time, probs = c(0.25,0.75), na.rm = T)
time_q[1]<- round(time_q[1],3)
time_q[2]<- round(time_q[2],3)

# quantile for prop female responders
fem_q<- quantile(GP_2022$proportion_of_female_responders, probs = c(0.25,0.75), na.rm = T)
fem_q[1]<- round(fem_q[1],3)
fem_q[2]<- round(fem_q[2],3)
# proportion_of_female_responders=c(0,0,1,1,0,0,1,1)
newdata = data.frame(proportion_of_female_responders=c(0,0,1,1,0,0,1,1), 
                     age_average=c(0,0,0,0,0,0,0,0),
                     not_enough_time=c(0,0,0,0,1,1,1,1),
                     proportion_female_FTE_GPs=c(q[1],q[2],q[1],q[2],q[1],q[2],q[1],q[2]))
newdata
predict(factors, newdata, interval="confidence") 
p<-cbind(newdata,predict(factors, newdata, interval="confidence") );p

# April 12th - adjust margins plot
plot<- ggplot() + 
  geom_line(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_time))) +
  geom_point(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_time))) +
  geom_errorbar(data=p, aes(x=proportion_of_female_responders, ymin=lwr, ymax=upr), width=.1, position=position_dodge(.1))+
  ylab('predicted positive patient satisfaction')+
  xlab('proportion of female responders')+
  ggtitle('Adjusted prediction with 95% CIs')
plot1<- ggplot() + geom_histogram(data = GP_2022, aes(x = proportion_of_female_responders, y= after_stat(count / sum(count))),bins = 100, alpha = .5)+
  ylab('frequency') +xlim(0,1)

g1 <- ggplotGrob(plot1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(plot)#

# set the same widths for both blots
g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)

# stack them afterwards
g <- rbind(g1, g2, size="last") # stack the two plots

g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)
p

# Not enough listening
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_listening*proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
confint(factors)

newdata = data.frame(proportion_of_female_responders=c(0,0,1,1,0,0,1,1), 
                     age_average=c(0,0,0,0,0,0,0,0),
                     not_enough_listening=c(0,0,0,0,1,1,1,1),
                     proportion_female_FTE_GPs=c(q[1],q[2],q[1],q[2],q[1],q[2],q[1],q[2]))
newdata
predict(factors, newdata, interval="confidence") 
p<-cbind(newdata,predict(factors, newdata, interval="confidence") );p

# April 12th - adjust margins plot
plot<- ggplot() + 
  geom_line(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_listening))) +
  geom_point(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_listening))) +
  geom_errorbar(data=p, aes(x=proportion_of_female_responders, ymin=lwr, ymax=upr), width=.1, position=position_dodge(.1))+
  ylab('predicted positive patient satisfaction')+
  xlab('proportion of female responders')+
  ggtitle('Adjusted prediction with 95% CIs')
plot1<- ggplot() + geom_histogram(data = GP_2022, aes(x = proportion_of_female_responders, y= after_stat(count / sum(count))),bins = 100, alpha = .5)+
  ylab('frequency') +xlim(0,1)

g1 <- ggplotGrob(plot1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(plot)#

# set the same widths for both blots
g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)

# stack them afterwards
g <- rbind(g1, g2, size="last") # stack the two plots

g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)

# Not enough care concern
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_care_concern*proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
confint(factors)

newdata = data.frame(proportion_of_female_responders=c(0,0,1,1,0,0,1,1), 
                     age_average=c(0,0,0,0,0,0,0,0),
                     not_enough_care_concern=c(0,0,0,0,1,1,1,1),
                     proportion_female_FTE_GPs=c(q[1],q[2],q[1],q[2],q[1],q[2],q[1],q[2]))
newdata
predict(factors, newdata, interval="confidence") 
p<-cbind(newdata,predict(factors, newdata, interval="confidence") );p

# April 12th - adjust margins plot
plot<- ggplot() + 
  geom_line(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_care_concern))) +
  geom_point(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_care_concern))) +
  geom_errorbar(data=p, aes(x=proportion_of_female_responders, ymin=lwr, ymax=upr), width=.1, position=position_dodge(.1))+
  ylab('predicted positive patient satisfaction')+
  xlab('proportion of female responders')+
  ggtitle('Adjusted prediction with 95% CIs')
plot1<- ggplot() + geom_histogram(data = GP_2022, aes(x = proportion_of_female_responders, y= after_stat(count / sum(count))),bins = 100, alpha = .5)+
  ylab('frequency') +xlim(0,1)

g1 <- ggplotGrob(plot1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(plot)#

# set the same widths for both blots
g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)

# stack them afterwards
g <- rbind(g1, g2, size="last") # stack the two plots

g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)

# Not enough trust
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_trust*proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
confint(factors)

newdata = data.frame(proportion_of_female_responders=c(0,0,1,1,0,0,1,1), 
                     age_average=c(0,0,0,0,0,0,0,0),
                     not_enough_trust=c(0,0,0,0,1,1,1,1),
                     proportion_female_FTE_GPs=c(q[1],q[2],q[1],q[2],q[1],q[2],q[1],q[2]))
newdata
predict(factors, newdata, interval="confidence") 
p<-cbind(newdata,predict(factors, newdata, interval="confidence") );p

# April 12th - adjust margins plot
plot<- ggplot() + 
  geom_line(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_trust))) +
  geom_point(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,not_enough_trust))) +
  geom_errorbar(data=p, aes(x=proportion_of_female_responders, ymin=lwr, ymax=upr), width=.1, position=position_dodge(.1))+
  ylab('predicted positive patient satisfaction')+
  xlab('proportion of female responders')+
  ggtitle('Adjusted prediction with 95% CIs')
plot1<- ggplot() + geom_histogram(data = GP_2022, aes(x = proportion_of_female_responders, y= after_stat(count / sum(count))),bins = 100, alpha = .5)+
  ylab('frequency') +xlim(0,1)

g1 <- ggplotGrob(plot1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(plot)#

# set the same widths for both blots
g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)

# stack them afterwards
g <- rbind(g1, g2, size="last") # stack the two plots

g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)

# needs not met
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               needs_not_met*proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
confint(factors)

newdata = data.frame(proportion_of_female_responders=c(0,0,1,1,0,0,1,1), 
                     age_average=c(0,0,0,0,0,0,0,0),
                     needs_not_met=c(0,0,0,0,1,1,1,1),
                     proportion_female_FTE_GPs=c(q[1],q[2],q[1],q[2],q[1],q[2],q[1],q[2]))
newdata
predict(factors, newdata, interval="confidence") 
p<-cbind(newdata,predict(factors, newdata, interval="confidence") );p

# April 12th - adjust margins plot
plot<- ggplot() + 
  geom_line(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,needs_not_met))) +
  geom_point(data = p, aes(x = proportion_of_female_responders, y = fit, colour = interaction(proportion_female_FTE_GPs,needs_not_met))) +
  geom_errorbar(data=p, aes(x=proportion_of_female_responders, ymin=lwr, ymax=upr), width=.1, position=position_dodge(.1))+
  ylab('predicted positive patient satisfaction')+
  xlab('proportion of female responders')+
  ggtitle('Adjusted prediction with 95% CIs')
plot1<- ggplot() + geom_histogram(data = GP_2022, aes(x = proportion_of_female_responders, y= after_stat(count / sum(count))),bins = 100, alpha = .5)+
  ylab('frequency') +xlim(0,1)

g1 <- ggplotGrob(plot1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(plot)#

# set the same widths for both blots
g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)

# stack them afterwards
g <- rbind(g1, g2, size="last") # stack the two plots

g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)

# last appointment late
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               last_appointment_late*proportion_of_female_responders*proportion_female_FTE_GPs, data=GP_2022)
shccm(factors, "hc3")
vif(factors)
confint(factors)

newdata = data.frame(proportion_of_female_responders=c(0,0,1,1,0,0,1,1), 
                     age_average=c(0,0,0,0,0,0,0,0),
                     last_appointment_late=c(0,0,0,0,1,1,1,1),
                     proportion_female_FTE_GPs=c(0,1,0,1,0,1,0,1))
newdata
predict(factors, newdata, interval="confidence") 
cbind(newdata,predict(factors, newdata, interval="confidence"))

# important code for understanding mechanism
factors<- lm(patient_satisfaction~proportion_of_female_responders*age_average+
               not_enough_time*proportion_of_female_responders+ not_enough_listening, data=GP_2022)
shccm(factors, "hc3")
vif(factors)

confint(factors)

# Examine sample selection concerns
lm_simple<- lm( proportion_of_female_responders~ TOTAL_PATIENTS+Is_GP_Rural+practice_workload, data=GP_2022)
shccm(lm_simple,"hc3")

# Extras
#fit <- factanal(GP_2022[, c("patient_appointment_satisfaction",
#                            "last_appointment_late", "not_enough_time","not_enough_listening",
#                            "not_enough_care_concern","not_enough_trust", "needs_not_met")], 3, rotation="varimax")
#print(fit, digits=2, cutoff=.3, sort=TRUE)
#load <- fit$loadings[,c(1,3)]
#plot(load,type="n") # set up plot
#text(load,labels=c("patient_appointment_satisfaction",
#                   "last_appointment_late", "not_enough_time","not_enough_listening",
#                   "not_enough_care_concern","not_enough_trust", "needs_not_met"),cex=.7) # add variable names


# compare with registered proportion of females 
# the results are adjusted to be representative but of course these inconsistencies are natural
GP_2022$registered_prop_female<- GP_2022$TOTAL_FEMALE/(GP_2022$TOTAL_FEMALE+GP_2022$TOTAL_MALE)
cor(GP_2022$registered_prop_female, GP_2022$proportion_of_female_responders)
plot(GP_2022$registered_prop_female,  GP_2022$proportion_of_female_responders)
abline(lm(proportion_of_female_responders~registered_prop_female, data=GP_2022))
summary(lm(proportion_of_female_responders~registered_prop_female, data=GP_2022))
names(GP_2022)

hist(GP_2022$age_average,breaks=50)

# Prepare regressions for latex output - Hypothesis 1
l_basic<- lm(patient_satisfaction ~proportion_of_female_responders, data=GP_2022)
shccm(l_basic, "hc3")
l_basic_with_age<- lm(patient_satisfaction ~proportion_of_female_responders+age_average, data=GP_2022)
shccm(l_basic_with_age, "hc3")
summary(l_basic_with_age)
l_basic_with_age_rurality<- lm(patient_satisfaction ~proportion_of_female_responders+age_average+Is_GP_Rural, data=GP_2022)
shccm(l_basic_with_age_rurality, "hc3")
l_basic_with_age_rurality_frequency<- lm(patient_satisfaction ~proportion_of_female_responders+age_average+Is_GP_Rural+low_frequency_visit, data=GP_2022)
shccm(l_basic_with_age_rurality_frequency, "hc3")

# shorten names to use stargazer values
b1<-l_basic
b2<- l_basic_with_age
b3<- l_basic_with_age_rurality
b4<- l_basic_with_age_rurality_frequency

# names(GP_2022)

selm<-coeftest(l_basic,
              vcov = vcovHC(l_basic,
                            type = "HC3"))[,2]

selm_pval<- coeftest(l_basic,
                     vcov = vcovHC(l_basic,
                                   type = "HC3"))[,4]

selm_age<-coeftest(l_basic_with_age,
              vcov = vcovHC(l_basic_with_age,
                            type = "HC3"))[,2]

selm_age_pval<- coeftest(l_basic_with_age,
                     vcov = vcovHC(l_basic_with_age,
                                   type = "HC3"))[,4]

selm_age_rurality<-coeftest(l_basic_with_age_rurality,
                  vcov = vcovHC(l_basic_with_age_rurality,
                                type = "HC3"))[,2]

selm_age_rurality_pval<-coeftest(l_basic_with_age_rurality,
                            vcov = vcovHC(l_basic_with_age_rurality,
                                          type = "HC3"))[,4]

selm_age_rurality_frequency<-coeftest(l_basic_with_age_rurality_frequency,
                           vcov = vcovHC(l_basic_with_age_rurality_frequency,
                                         type = "HC3"))[,2]

selm_age_rurality_frequency_pval<-coeftest(l_basic_with_age_rurality_frequency,
                                      vcov = vcovHC(l_basic_with_age_rurality_frequency,
                                                    type = "HC3"))[,4]

inputvec<-list(selm, selm_age, selm_age_rurality, selm_age_rurality_frequency)
inputvec_pval<- list(selm_pval, selm_age_pval, selm_age_rurality_pval, selm_age_rurality_frequency_pval)

# print results nicely in LaTex
stargazer(b1, b2,b3,b4,
          title="Regression Results",
          type="text",
          dep.var.labels=c("Proportion of Positive Patient Satisfaction"),
          covariate.labels=c("Proportion of Female Responders", "Age average demeaned", "Is GP Rural, 1 if Rural", "Proportion of low frequency patients"),
          object.names = TRUE,
          se=inputvec,
          p=inputvec_pval,
          align=TRUE,
          no.space=TRUE)

# mediation hypothesis - access - Hypothesis 2
m1<-lm(patient_satisfaction ~    proportion_of_female_responders+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_before=coeftest(m1,
                                         vcov = vcovHC(m1,
                                                       type = "HC3"))[,2]
shccm(m1)
m2<-lm(patient_satisfaction ~       patient_appointment_satisfaction+proportion_of_female_responders+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_after=coeftest(m2,
                                        vcov = vcovHC(m2,
                                                      type = "HC3"))[,2]
shccm(m2)
m3<-lm(patient_satisfaction ~            patient_appointment_satisfaction+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)
shccm(m3)
selm_mediators_appointment_satisfaction=coeftest(m3,
                                                           vcov = vcovHC(m3,
                                                                         type = "HC3"))[,2]

m4<-lm(patient_appointment_satisfaction ~
         proportion_of_female_responders+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_appointment=coeftest(m4,
                                              vcov = vcovHC(m4,
                                                            type = "HC3"))[,2]
shccm(m4)
inputvec3=list(selm_mediators_before,selm_mediators_after, selm_mediators_appointment_satisfaction,selm_mediators_appointment)

stargazer(m1,m2,m3,m4,
          title="Appointment Mediator Results",
          type="text",
          #dep.var.labels=c("Proportion of Positive Patient Overall Satisfaction",
          #                 "Proportion of Positive Patient Satisfaction when making an appointment"),
          object.names = TRUE,
          se=inputvec3,
          align=TRUE,
          no.space=TRUE)

# moderation hypothesis - Hypothesis 3
hist(GP_2022$proportion_female_FTE_GPs)
l1<- lm(patient_satisfaction ~proportion_of_female_responders*proportion_female_FTE_GPs+age_average+Is_GP_Rural+low_frequency_visit, data=GP_2022)
shccm(l1)
l2<- lm(patient_satisfaction ~proportion_of_female_responders+age_average+Is_GP_Rural*proportion_female_FTE_GPs+low_frequency_visit, data=GP_2022)
shccm(l2)

l3<- lm(patient_satisfaction ~proportion_of_female_responders*proportion_female_FTE_GPs+patient_appointment_satisfaction+age_average+Is_GP_Rural+low_frequency_visit, data=GP_2022)
shccm(l3)

selm_l1=coeftest(l1,
                                     vcov = vcovHC(l1,
                                                   type = "HC3"))[,2]
selm_l2=coeftest(l2,
                                        vcov = vcovHC(l2,
                                                      type = "HC3"))[,2]
selm_l3=coeftest(l3,
                                                 vcov = vcovHC(l3,
                                                               type = "HC3"))[,2]
# model b4 is the basic model
# coef test list: selm_age_rurality_frequency
inputvec1=list(selm_age_rurality_frequency,selm_l1,selm_l3, selm_l2)

stargazer(b4,l1,l3,l2,
          title="Moderation Results",
          type="text",
          dep.var.labels=c("Proportion of Positive Patient Satisfaction"),
          covariate.labels=c("Proportion of Female Responders", "Proportion Female FTE GPs","Patient appointment satisfaction", "Age average demeaned", "Is GP Rural, 1 if Rural", "Proportion of low frequency patients","Proportion of female responders* Proportion female FTE GPs", "Is GP Rural*proportion female FTE GPs"),
          object.names = TRUE,
          se=inputvec1,
          align=TRUE,
          no.space=TRUE)

# Regression with all possible mediators for quality of service
lm_mediators_with_controls<-lm(patient_satisfaction ~proportion_of_female_responders+age_average+Is_GP_Rural+low_frequency_visit+proportion_of_female_responders*not_enough_care_concern+proportion_of_female_responders*not_enough_listening+proportion_of_female_responders*not_enough_time+proportion_of_female_responders*needs_not_met+proportion_of_female_responders*not_enough_trust, data=GP_2022)
selm_mediators_with_controls=coeftest(lm_mediators_with_controls,
                                                 vcov = vcovHC(lm_mediators_with_controls,
                                                               type = "HC3"))[,2]

lm_mediators_no_controls<-lm(patient_satisfaction ~proportion_of_female_responders+age_average+proportion_of_female_responders*not_enough_care_concern+proportion_of_female_responders*not_enough_listening+proportion_of_female_responders*not_enough_time+proportion_of_female_responders*needs_not_met+proportion_of_female_responders*not_enough_trust, data=GP_2022)
selm_mediators_no_controls=coeftest(lm_mediators_no_controls,
                                      vcov = vcovHC(lm_mediators_no_controls,
                                                    type = "HC3"))[,2]
inputvec2=list(selm_mediators_with_controls,selm_mediators_no_controls)
stargazer(lm_mediators_no_controls,lm_mediators_with_controls,title="Regression Results",
          type="text",
          dep.var.labels=c("Proportion of Positive Patient Satisfaction"),
          #covariate.labels=c("Proportion of Female Responders", "Proportion Female FTE GPs","Age average demeaned", "Is GP Rural, 1 if Rural", "Proportion of low frequency patients","Proportion of female responders* Proportion female FTE GPs"),
          object.names = TRUE,
          se=inputvec2,
          align=TRUE,
          no.space=TRUE)
vif(lm_mediators)

# mediation hypothesis - quality of service - Hypothesis 4 (was not added in the final submission)
m1<-lm(patient_satisfaction ~
         proportion_of_female_responders+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_listening_before=coeftest(m1,
                                         vcov = vcovHC(m1,
                                                       type = "HC3"))[,2]

m2<-lm(patient_satisfaction ~
         not_enough_listening+proportion_of_female_responders+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_listening_after=coeftest(m2,
                                        vcov = vcovHC(m2,
                                                      type = "HC3"))[,2]

m3<-lm(patient_satisfaction ~
         not_enough_listening+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_listening_appointment_satisfaction=coeftest(m3,
                                                           vcov = vcovHC(m3,
                                                                         type = "HC3"))[,2]

m4<-lm(not_enough_listening ~
         proportion_of_female_responders+age_average+
         Is_GP_Rural+
         low_frequency_visit, data=GP_2022)

selm_mediators_listening_appointment=coeftest(m4,
                                              vcov = vcovHC(m4,
                                                            type = "HC3"))[,2]

inputvec3=list(selm_mediators_listening_before,selm_mediators_listening_after, selm_mediators_listening_appointment_satisfaction,selm_mediators_listening_appointment)

stargazer(m1,m2,m3,m4,
          title="Not enough listening Mediator Results",
          type="text",
          #dep.var.labels=c("Proportion of Positive Patient Overall Satisfaction",
          #                 "Proportion of Positive Patient Satisfaction when making an appointment"),
          object.names = TRUE,
          se=inputvec3,
          align=TRUE,
          no.space=TRUE)


# Descriptive statistics
plot(GP_2022$age_average, GP_2022$patient_satisfaction)

describeBy(subset(GP_2022, select=c("proportion_of_female_responders", 
                                  "patient_satisfaction", "Female", 
                                  "age_average",
                                  "male_gp_average_age",
                                  "female_gp_average_age",
                                  "proportion_female_FTE_GPs",
                                  "age_average_demeaned",
                                  "low_frequency_visit",
                                  "practice_workload",
                                  "patient_appointment_satisfaction")))
hist(GP_2022$proportion_of_female_responders)
hist(GP_2022$patient_satisfaction)

quantile(GP_2022$proportion_female_FTE_GPs, probs=seq(0,1,0.05),  na.rm=TRUE)
hist(GP_2022$proportion_female_FTE_GPs)
apply(GP_2022[,c("proportion_of_female_responders", 
                 "patient_satisfaction", "Female", 
                 "age_average",
                 "male_gp_average_age",
                 "female_gp_average_age",
                 "proportion_female_FTE_GPs",
                 "age_average_demeaned",
                 "low_frequency_visit",
                 "practice_workload",
                 "patient_appointment_satisfaction")],2,quantile,probs=c(0.25,0.75), na.rm=TRUE)

apply(GP_2022[,c("proportion_of_female_responders", 
                 "patient_satisfaction", "Female", 
                 "age_average",
                 "male_gp_average_age",
                 "female_gp_average_age",
                 "proportion_female_FTE_GPs",
                 "age_average_demeaned",
                 "low_frequency_visit",
                 "practice_workload",
                 "patient_appointment_satisfaction")],2,IQR, na.rm=TRUE)
