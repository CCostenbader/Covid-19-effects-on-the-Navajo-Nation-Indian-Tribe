# FINAL GROUP PROJECT IN R

# NEED TO CONFIRM HOW AND WHAT TO DO TO SET WORKING DIRECTORY 
# FOR EACH INSTANCE OF WORKING WITH DATASET BELOW
# IN ORDER TO IMPORT DATASET CovidDataSet.csv AND 
# FOR R TO WORK PROPERTLY. 

# IMPORT LIBRARIES
library("rcompanion")
library("car")
library("fastR2")
library("IDPmisc")

# IMPORT DATA SET 
CovidDataSet <- read.csv("CovidDataSet.csv")

covid2 <- filter(CovidDataSet, FIPS %in% c(4001, 4003, 4005, 4007, 4009, 4011, 4012, 4013, 4015, 4017, 4019, 4021, 4023, 4025,4027,35001, 35003, 35005, 35006, 35007, 35009, 35011, 35013, 35015, 35017, 35019, 35021, 35023, 35025, 35027, 35028, 35029, 35031, 35033, 35035, 35037, 35039, 35041, 35045, 35047, 35043, 35049, 35051, 35053, 35055, 35057, 35059, 35061))

covidtest2 <- select(covid2, FIPS, Confirmed, Deaths, Recovered, Active)

# Recode with Navajo binary

covidtest$NavajoR <- NA

## Navajo = 1
covidtest$NavajoR[covidtest$FIPS== 4001] <- 1
covidtest$NavajoR[covidtest$FIPS== 4005] <- 1
covidtest$NavajoR[covidtest$FIPS== 4017] <- 1
covidtest$NavajoR[covidtest$FIPS== 35031] <- 1
covidtest$NavajoR[covidtest$FIPS== 35045] <- 1

## Non-Navajo = 0
covidtest$NavajoR[covidtest$FIPS== 4003] <- 0
covidtest$NavajoR[covidtest$FIPS== 40012] <- 0
covidtest$NavajoR[covidtest$FIPS== 4007] <- 0
covidtest$NavajoR[covidtest$FIPS== 35006] <- 0
covidtest$NavajoR[covidtest$FIPS== 35001] <- 0
covidtest$NavajoR[covidtest$FIPS== 4012] <- 0
covidtest$NavajoR[covidtest$FIPS== 4013] <- 0
covidtest$NavajoR[covidtest$FIPS== 4015] <- 0
covidtest$NavajoR[covidtest$FIPS== 4019] <- 0
covidtest$NavajoR[covidtest$FIPS== 4021] <- 0
covidtest$NavajoR[covidtest$FIPS== 4023] <- 0
covidtest$NavajoR[covidtest$FIPS== 4025] <- 0
covidtest$NavajoR[covidtest$FIPS== 4027] <- 0
covidtest$NavajoR[covidtest$FIPS== 35029] <- 0
covidtest$NavajoR[covidtest$FIPS== 35003] <- 0
covidtest$NavajoR[covidtest$FIPS== 35028] <- 0
covidtest$NavajoR[covidtest$FIPS== 35033] <- 0
covidtest$NavajoR[covidtest$FIPS== 35035] <- 0
covidtest$NavajoR[covidtest$FIPS== 35013] <- 0
covidtest$NavajoR[covidtest$FIPS== 35011] <- 0
covidtest$NavajoR[covidtest$FIPS== 35009] <- 0
covidtest$NavajoR[covidtest$FIPS== 35039] <- 0
covidtest$NavajoR[covidtest$FIPS== 35007] <- 0
covidtest$NavajoR[covidtest$FIPS== 35028] <- 0
covidtest$NavajoR[covidtest$FIPS== 35017] <- 0
covidtest$NavajoR[covidtest$FIPS== 35027] <- 0
covidtest$NavajoR[covidtest$FIPS== 35025] <- 0
covidtest$NavajoR[covidtest$FIPS== 35015] <- 0
covidtest$NavajoR[covidtest$FIPS== 35019] <- 0
covidtest$NavajoR[covidtest$FIPS== 35021] <- 0
covidtest$NavajoR[covidtest$FIPS== 35037] <- 0
covidtest$NavajoR[covidtest$FIPS== 35047] <- 0
covidtest$NavajoR[covidtest$FIPS== 35043] <- 0
covidtest$NavajoR[covidtest$FIPS== 35049] <- 0
covidtest$NavajoR[covidtest$FIPS== 35051] <- 0
covidtest$NavajoR[covidtest$FIPS== 35053] <- 0
covidtest$NavajoR[covidtest$FIPS== 35055] <- 0
covidtest$NavajoR[covidtest$FIPS== 35057] <- 0
covidtest$NavajoR[covidtest$FIPS== 90035] <- 0
covidtest$NavajoR[covidtest$FIPS== 35059] <- 0
covidtest$NavajoR[covidtest$FIPS== 35061] <- 0

## NAs left off question if NN
covidtest$NavajoR[covidtest$FIPS== 4009] <- 0 # Graham, AZ
covidtest$NavajoR[covidtest$FIPS== 4011] <- 0 # Greenlee, AZ
covidtest$NavajoR[covidtest$FIPS== 35005] <- 0 # Chaves, NM
covidtest$NavajoR[covidtest$FIPS== 35023] <- 0 # Hidalgo, NM
covidtest$NavajoR[covidtest$FIPS== 35041] <- 0 # Roosevelt, NM

covidtest2 <- select(covidtest, Confirmed, Deaths, Recovered, Active, NavajoR)

### ANOVA for Confirmed Cases

##Assumptions

#Normality
plotNormalHistogram(covidtest2$Confirmed)
covidtest2$ConfirmedLOG <- log(covidtest2$Confirmed)
plotNormalHistogram(covidtest2$ConfirmedLOG)
#Log is Good

# Homogeneity of Variance
bartlett.test(covidtest2$ConfirmedLOG ~ NavajoR, data= covidtest2)
#Assumption not met

## Welch's One-Way Test
ANOVA <- lm(ConfirmedLOG ~ NavajoR, data=covidtest2)
Anova(ANOVA, Type="II", white.adjust=TRUE)
# Significant difference between two IVs

## Post-Hoc

##Bonferroni Adj
pairwise.t.test(covidtest2$ConfirmedLOG, covidtest2$NavajoR, p.adjust="bonferroni", pool.sd = FALSE)
# Significant difference in Confirmed Cases between Navajo / Non-Navajo counties

# Means & conclusions
ConfirmedMeans <- covidtest2 %>% group_by(NavajoR) %>% summarize(Mean = mean(Confirmed))
## Significant difference between Navajo & Non-Navajo Counties, on average confirmed cases in the Non-Navajo counties is 7608 more than that of the Navajo Nation counties


### ANOVA for Deaths

## Assumptions

# Normality
plotNormalHistogram(covidtest2$Deaths)
covidtest2$DeathsLOG <- log(covidtest2$Deaths)

# Drop NAs
covidtest3 <-NaRV.omit(covidtest2)

plotNormalHistogram(covidtest3$DeathsLOG)
# Log is good

# Homogeneity of Variance
bartlett.test(covidtest3$DeathsLOG ~ NavajoR, data= covidtest3)
# Assumptions Not Met


## Welch's One-Way Test
ANOVA2 <- lm(DeathsLOG ~ NavajoR, data=covidtest3)
Anova(ANOVA2, Type="II", white.adjust=TRUE)
# Significant Difference Between 2 IVs


## Post-Hoc

# Bonferroni Adj
pairwise.t.test(covidtest3$DeathsLOG, covidtest3$NavajoR, p.adjust="bonferroni", pool.sd = FALSE)
# Significant difference in Deaths between Navajo / Non-Navajo counties

# Means & Conclusions
DeathsMeans <- covidtest3 %>% group_by(NavajoR) %>% summarize(Mean = mean(Deaths))
## On Average 3 more deaths in Non-Navajo Counties


