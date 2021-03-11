# COVID EFFECS ON NAVAJON NATION INDIAN TRIBE

# WORKING DIRECTORY MUST BE SET EACH TIME THIS FILE IS
# OPENED TO ALLOW FOR CovidDataSet2.csv TO BE PROPERLY LOADED


## IMPORT LIBRARIES
library("dplyr")
library("rcompanion")
library("car")
library("fastR2")
library("IDPmisc")

## IMPORT DATA SET 
CovidDataSet <- read.csv("CovidDataSet2.csv")

## SORTING OUT AZ AND NM STATE FIPS CODES
covid2 <- filter(CovidDataSet, 
                 FIPS %in% c(4001, 4003, 4005, 4007, 4009, 4011, 4012, 4013, 4015, 4017, 4019, 4021, 4023, 4025,4027,
                             35001, 35003, 35005, 35006, 35007, 35009, 35011, 35013, 35015, 35017, 35019, 35021, 35023, 35025, 35027, 35028, 35029, 35031, 35033, 35035, 35037, 35039, 35041, 35045, 35047, 35043, 35049, 35051, 35053, 35055, 35057, 35059, 35061))

## TRIMMING FIELDS FOR DATA SET
covidtest2 <- select(covid2, FIPS, Province_State, Lat, Long_, 
                     Confirmed, Deaths, Active, Incident_Rate, 
                     Case_Fatality_Ratio)

## Recode with Navajo binary

### CREATING NavajoR VARIABLE/COLUMN IN DATASET
covidtest2$NavajoR <- NA

### Navajo = 1
covidtest2$NavajoR[covidtest2$FIPS== 4001] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 4005] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 4017] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 35031] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 35045] <- 1

### Non-Navajo = 0
covidtest2$NavajoR[covidtest2$FIPS== 4003] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4007] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4009] <- 0 
covidtest2$NavajoR[covidtest2$FIPS== 4011] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4012] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4013] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4015] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4019] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4021] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4023] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4025] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 4027] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35001] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35003] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35005] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35006] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35007] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35009] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35011] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35013] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35015] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35017] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35019] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35021] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35023] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35025] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35027] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35028] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35029] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35033] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35035] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35037] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35039] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35041] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35043] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35047] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35049] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35051] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35053] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35055] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35057] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35059] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 35061] <- 0
covidtest2$NavajoR[covidtest2$FIPS== 90049] <- 0

## CREATING Covidtest3 DATAFRAME FOR RUNNING ANOVAS
covidtest3 <- select(covidtest2, FIPS, Province_State, Lat, Long_, 
                     Confirmed, Deaths, Active, Incident_Rate, 
                     Case_Fatality_Ratio, NavajoR)

## ANOVA FOR VARIABLE "Confirmed Cases"

### Assumptions

#### Normality

#### PLOTTING NORMAL HISTOGRAM 
plotNormalHistogram(covidtest3$Confirmed)


#### HISTOGRAM POSITIVELY SKEWED, TRYING ADJUSTMENT WITH SQRT 
covidtest3$ConfirmedSQRT <- sqrt(covidtest3$Confirmed)
plotNormalHistogram((covidtest3$ConfirmedSQRT))
 

#### STILL POSITIVELY SKEWED WITH SQRT TRYING ADJUSTMENT WITH LOG
covidtest3$ConfirmedLOG <- log(covidtest3$Confirmed)
plotNormalHistogram(covidtest3$ConfirmedLOG)
#### WITH LOG ADJUSTMENT, NOW MEETS ASSUMPTION FOR NORMALITY


### Homogeneity of Variance

#### RUNNING BARTLETT'S TEST 
bartlett.test(covidtest3$ConfirmedLOG ~ NavajoR, data= covidtest3)
#### P-VALUE IS .0001855 < .05 AND IS SIGNIFICANT
#### DOES NOT MEET ASSUMPTION FOR HOMOGENEITY OF VARIANCE

### SAMPLE SIZE IS 48 > 20 AND MEETS ASSUMPTION 

### INDEPENDENCE - THERE IS NO TEST FOR THIS
### DATA MEETS THIS ASSUMPION BASED ON WHAT WE KNOW

### RUNNING WELCH'S ONE WAY BECAUSE DOES NOT MEET ASSUMPTION 
### FOR HOMOGENEITY OF VARIANCE. 

##### Welch's One-Way Test
ANOVA <- lm(ConfirmedLOG ~ NavajoR, data=covidtest3)
Anova(ANOVA, Type="II", white.adjust=TRUE)
#### AFTER RUNNING WELCH'S ONE-WAY TEST
#### P-VALUE = .001 < .05 AND IS SIGNIFICANT
#### THERE IS A SIGNFICANT DIFFERENCE BETWEEN THE TWO IVs

### Post-Hoc

#### Bonferroni Adj
pairwise.t.test(covidtest3$ConfirmedLOG, covidtest3$NavajoR, 
                p.adjust="bonferroni", pool.sd = FALSE)
#### P-VALUE = 0.000015 < .05 AND IS SIGNIFICANT
#### THEREFORE THERE IS A Significant difference 
#### BETWEEN CONFIRMED CASES Navajo / Non-Navajo GROUPS


### Means & conclusions
ConfirmedMeans <- covidtest3 %>% group_by(NavajoR) %>% 
  summarize(Mean = mean(Confirmed))
View(ConfirmedMeans)
#### There is a Significant difference between Navajo & Non-Navajo 
#### Counties, on average confirmed cases in the Non-Navajo counties 
#### is 7974 more than that of the Navajo Nation counties


## ANOVA FOR "Deaths"

## Assumptions

### Normality
plotNormalHistogram(covidtest3$Deaths)
covidtest3$DeathsLOG <- log(covidtest3$Deaths)

### Drop NAs
covidtest4 <-NaRV.omit(covidtest3)

### Homogeneity of Variance
fligner.test(covidtest4$DeathsLOG ~ NavajoR, data= covidtest4)
#### P-VALUE IS .01262 < .05 AND IS SIGNIFICANT

bartlett.test(covidtest4$DeathsLOG ~ NavajoR, data= covidtest4)
#### P-VALUE IS 0.0002804 < .05 AND IS SIGNIFICANT

#### DOES NOT MEET ASSUMPTION


### Welch's One-Way Test
ANOVA2 <- lm(DeathsLOG ~ NavajoR, data=covidtest4)
Anova(ANOVA2, Type="II", white.adjust=TRUE)
#### P-VALUE = .001 < .05 AND IS SIGNIFICANT
#### THERE IS A SIGNIFICANT DIFFERENCE BETWEEN 
#### THE TWO IVs - NAVAJO AND NON-NAVAJO DEATHS


## Post-Hoc

### Bonferroni Adj
pairwise.t.test(covidtest4$DeathsLOG, covidtest4$NavajoR, p.adjust="bonferroni", pool.sd = FALSE)
#### Significant difference in Deaths between Navajo / Non-Navajo counties

### Means & Conclusions
DeathsMeans <- covidtest4 %>% group_by(NavajoR) %>% 
  summarize(Mean = mean(Deaths))
#### On Average 18 more deaths in Non-Navajo Counties


## ANOVA for "INCIDENT_RATE" = CASES PER 100,000 PEOPLE

### TESTING FOR NORMALITY 
plotNormalHistogram(covidtest3$Incident_Rate)
#### Normal distribution and meets assumption

### Homogeneity of Variance
bartlett.test(Incident_Rate ~ NavajoR, data= covidtest3)
# P-Value = .05 < .362 and iS not significant
# THIS MEETS ASSUMPTION FOR HOMOGENIETY OF VARIANCE

### ANOVA
IncidentANOVA <- aov(covidtest3$Incident_Rate ~ covidtest3$NavajoR)

summary(IncidentANOVA)
#### P-VALUE =  .01 < .05 AND IS SIGNFICANT 
#### Test is significant

### Post-Hoc
pairwise.t.test(covidtest3$Incident_Rate, covidtest3$NavajoR, p.adjust="bonferroni")
#### Significant difference between rates in and out of Navajo Nation counties

### Means & Conclusions
IncidentMeans <- covidtest3 %>% group_by(NavajoR) %>% summarize(Mean = mean(Incident_Rate))
#### Significantly higher incident rates within Navajo Nation counties with Navajo Nation counties having an average of 6,956 higher incident rates than counties without Navajo Nation

## ANOVA for "CASE_FATALITY_RATIO" 

### Normality
plotNormalHistogram(covidtest3$Case_Fatality_Ratio)
covidtest4$RatioLOG <- log(covidtest4$Case_Fatality_Ratio)
plotNormalHistogram(covidtest4$RatioLOG)
#### LOG MEETS ASSUMPION FOR NORMALITY

### Homogeneity of Variance
bartlett.test(RatioLOG ~ NavajoR, data= covidtest4)
#### P-VALUE = .05 < .225 AND IS NOT SIGNIFICANT
#### WHICH MEETS ASSUMPTION 

### ANOVA
RatioANOVA <- aov(covidtest4$RatioLOG ~ covidtest4$NavajoR)
summary(RatioANOVA)
#### DIFFERENCE IN NOT SIGNIFICANT between Navajo and Non-Navajo County Ratios

### Post-Hoc
pairwise.t.test(covidtest4$RatioLOG, covidtest4$NavajoR, p.adjust="bonferroni")
#### P-VALUE IS .05 < .17 AND IS NOT SIGNIFICANT

#### CONCLUSION:  THERE IS AN INSIGNIFICANT DIFFERENCE IN THE 
#### CASE_FATALITY_RATIO AS A PERCENTAGE, 
#### WHICH = NUMBER OF DEATHS / NUMBER OF CASES BETWEEN 
#### NON-NAVAJO VS NAVAJO NATION.  

#### LOOKING AT AN INCIDENT_RATE SHOWING 63.24% MORE CASES PER 
#### 100,000 MORE CASES PER 100K PEOPLE, 
#### THEN WE CAN CONCLUDE 
#### THAT THE NAVAJO NATION HAS HAD MORE THAN A GREATER NUMBER 
#### DEATHS PER 100,000 PEOPLE AS WELL.  

#### THE PEOPLE OF THE NAVAJO NATION ARE GETTING CRUSHED BY 
#### COVID-19 COMPARED TO SURROUNDING AREA POPULATIONS 
#### IN AZ AND NM