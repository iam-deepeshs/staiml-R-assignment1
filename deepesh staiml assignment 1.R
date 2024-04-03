# Load necessary libraries
library(ggplot2)
library(readr)

# Read the dataset from the URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"
data <- read.table(url, header = FALSE, sep = ",", na.strings = "?")

# Assign column names based on dataset description
colnames(data) <- c("state", "county", "community", "communityname", "fold", "population", "householdsize", 
                    "racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", "agePct12t21", "agePct12t29", 
                    "agePct16t24", "agePct65up", "numbUrban", "pctUrban", "medIncome", "pctWWage", "pctWFarmSelf", 
                    "pctWInvInc", "pctWSocSec", "pctWPubAsst", "pctWRetire", "medFamInc", "perCapInc", "whitePerCap", 
                    "blackPerCap", "indianPerCap", "AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov", 
                    "PctPopUnderPov", "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed", 
                    "PctEmploy", "PctEmplManu", "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", 
                    "MalePctDivorce", "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par", 
                    "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", 
                    "NumIlleg", "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8", 
                    "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", "PctRecImmig10", 
                    "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", "PctLargHouseOccup", 
                    "PersPerOccupHous", "PersPerOwnOccHous", "PersPerRentOccHous", "PctPersOwnOccup", 
                    "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", "PctHousOccup", 
                    "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", 
                    "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart", "RentLowQ", 
                    "RentMedian", "RentHighQ", "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", 
                    "MedOwnCostPctIncNoMtg", "NumInShelters", "NumStreet", "PctForeignBorn", "PctBornSameState", 
                    "PctSameHouse85", "PctSameCity85", "PctSameState85", "LemasSwornFT", "LemasSwFTPerPop", 
                    "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", 
                    "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack", 
                    "PctPolicHisp", "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", 
                    "PolicAveOTWorked", "LandArea", "PopDens", "PctUsePubTrans", "PolicCars", "PolicOperBudg", 
                    "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "LemasPctOfficDrugUn", "PolicBudgPerPop", 
                    "murders", "murdPerPop", "rapes", "rapesPerPop", "robberies", "robbbPerPop", "assaults", 
                    "assaultPerPop", "burglaries", "burglPerPop", "larcenies", "larcPerPop", "autoTheft", 
                    "autoTheftPerPop", "arsons", "arsonsPerPop", "ViolentCrimesPerPop")


# a)
ggplot(data, aes(x = state, y = ViolentCrimesPerPop)) +
  geom_boxplot() +
  labs(title = "Distribution of Violent Crimes per Population Across States", x = "State", y = "Violent Crimes per Population")

# b) 
ggplot(data, aes(x = medIncome, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Household Income and Violent Crimes", x = "Median Income", y = "Violent Crimes per Population")

# c)
ggplot(data, aes(x = PctUnemployed, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Unemployment Rate and Violent Crimes", x = "Percentage Unemployed", y = "Violent Crimes per Population")

# d) 
ggplot(data, aes(x = as.factor(pctUrban), y = ViolentCrimesPerPop)) +
  geom_boxplot() +
  labs(title = "Difference in Violent Crime Rates between Urban and Rural Areas", x = "Urban/Rural", y = "Violent Crimes per Population")

# e)
ggplot(data, aes(x = racepctblack, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Percentage of African American Population and Violent Crimes", x = "Percentage African American", y = "Violent Crimes per Population")

ggplot(data, aes(x = racePctWhite, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Percentage of Caucasian Population and Violent Crimes", x = "Percentage Caucasian", y = "Violent Crimes per Population")

ggplot(data, aes(x = racePctHisp, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Percentage of Hispanic Population and Violent Crimes", x = "Percentage Hispanic", y = "Violent Crimes per Population")

# f) 
cor.test(data$PolicRate, data$ViolentCrimesPerPop)
ggplot(data, aes(x = PolicRate, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Police Presence and Violent Crime Rate",
       x = "Police Rate", y = "Violent Crimes per Population")

# g) 
ggplot(data, aes(x = MedYrHousBuilt, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Median Year of Housing Units Built and Violent Crimes", x = "Median Year of Housing Units Built", y = "Violent Crimes per Population")

# i) 
ggplot(data, aes(x = pctWPubAsst, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Percentage of Households with Public Assistance Income and Violent Crimes", x = "Percentage of Households with Public Assistance Income", y = "Violent Crimes per Population")

# j) 
cor.test(data$pctUrban, data$ViolentCrimesPerPop)
ggplot(data, aes(x = pctUrban, y = ViolentCrimesPerPop)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Population Density and Violent Crime Rate",
       x = "Percentage of Urban Population", y = "Violent Crimes per Population")

# h) 
ggplot(data, aes(x = PctLargHouseFam, y = ViolentCrimesPerPop)) +
  geom_point() +
  labs(x = "Presence of gang units",
       y = "Violent Crimes per Population")
