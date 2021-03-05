# FINAL GROUP PROJECT IN R

# WORKING DIRECTORY MUST BE SET EACH TIME THIS FILE IS
# OPENED.   
# FOR EACH INSTANCE OF WORKING WITH DATASET BELOW
# IN ORDER TO IMPORT DATASET CovidDataSet.csv AND 
# FOR R TO WORK PROPERTLY. 

# IMPORT LIBRARIES
library("dplyr")
library("rcompanion")
library("car")
library("fastR2")
library("IDPmisc")

# IMPORT DATA SET 
CovidDataSet <- read.csv("CovidDataSet2.csv")

# SORTING OUT AZ AND NM STATE FIPS CODES
covid2 <- filter(CovidDataSet, 
                 FIPS %in% c(4001, 4003, 4005, 4007, 4009, 4011, 4012, 4013, 4015, 4017, 4019, 4021, 4023, 4025,4027,
                             35001, 35003, 35005, 35006, 35007, 35009, 35011, 35013, 35015, 35017, 35019, 35021, 35023, 35025, 35027, 35028, 35029, 35031, 35033, 35035, 35037, 35039, 35041, 35045, 35047, 35043, 35049, 35051, 35053, 35055, 35057, 35059, 35061))

# TRIMMING FIELDS FOR DATA SET
covidtest2 <- select(covid2, FIPS, Province_State, Lat, Long_, 
                     Confirmed, Deaths, Active, Incident_Rate, 
                     Case_Fatality_Ratio)

# Recode with Navajo binary
covidtest2$NavajoR <- NA

## Navajo = 1
covidtest2$NavajoR[covidtest2$FIPS== 4001] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 4005] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 4017] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 35031] <- 1
covidtest2$NavajoR[covidtest2$FIPS== 35045] <- 1

## Non-Navajo = 0
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



covidtest3 <- select(covidtest2, FIPS, Province_State, Lat, Long_, 
                     Confirmed, Deaths, Active, Incident_Rate, 
                     Case_Fatality_Ratio, NavajoR)

### ANOVA for Confirmed Cases

##Assumptions

#Normality
plotNormalHistogram(covidtest3$Confirmed)
covidtest3$ConfirmedLOG <- log(covidtest3$Confirmed)
plotNormalHistogram(covidtest3$ConfirmedLOG)
#Log is Good

# Homogeneity of Variance
bartlett.test(covidtest3$ConfirmedLOG ~ NavajoR, data= covidtest3)
# P-VALUE IS .0001855 < .05 AND IS NOT SIGNIFICANT
# DOES NOT MEET ASSUMPTION FOR HOMOGENEITY OF VARIANCE

# SAMPLE SIZE IS 20< AND MEETS ASSUMPTION 



## Welch's One-Way Test
ANOVA <- lm(ConfirmedLOG ~ NavajoR, data=covidtest3)
Anova(ANOVA, Type="II", white.adjust=TRUE)
# Significant difference between two IVs

## Post-Hoc

## Bonferroni Adj
pairwise.t.test(covidtest3$ConfirmedLOG, covidtest3$NavajoR, 
                p.adjust="bonferroni", pool.sd = FALSE)
# Significant difference in Confirmed Cases between 
# Navajo / Non-Navajo GROUPS


# Means & conclusions
ConfirmedMeans <- covidtest3 %>% group_by(NavajoR) %>% 
  summarize(Mean = mean(Confirmed))
View(ConfirmedMeans)
## There is a Significant difference between Navajo & Non-Navajo 
## Counties, on average confirmed cases in the Non-Navajo counties 
## is 7974 more than that of the Navajo Nation counties


### ANOVA for Deaths

## Assumptions

# Normality
plotNormalHistogram(covidtest3$Deaths)
covidtest3$DeathsLOG <- log(covidtest3$Deaths)

# Drop NAs
covidtest4 <-NaRV.omit(covidtest3)



# Homogeneity of Variance
bartlett.test(covidtest4$DeathsLOG ~ NavajoR, data= covidtest4)
# P-VALUE IS 0.002804 < .05 AND 
# Assumptions Not Met


## Welch's One-Way Test
ANOVA2 <- lm(DeathsLOG ~ NavajoR, data=covidtest4)
Anova(ANOVA2, Type="II", white.adjust=TRUE)
# Significant Difference Between 2 IVs


## Post-Hoc

# Bonferroni Adj
pairwise.t.test(covidtest4$DeathsLOG, covidtest4$NavajoR, p.adjust="bonferroni", pool.sd = FALSE)
# Significant difference in Deaths between Navajo / Non-Navajo counties

# Means & Conclusions
DeathsMeans <- covidtest4 %>% group_by(NavajoR) %>% 
  summarize(Mean = mean(Deaths))
## On Average 18 more deaths in Non-Navajo Counties



