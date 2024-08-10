library(dplyr)
library(ggplot2)

# Load and Clean Data
setwd("C:/Users/Sarvesh/Desktop/R Programs & Notes/College Basketball")
cbb_data <- data.frame(read.csv("cbb.csv"))

cbb_data$WINPER <- (cbb_data$W / cbb_data$G)
cbb_data$PREDWINPER <- 0
cbb_data$POSTSEASON_SCORE <- 0


names(cbb_data) <- c("TEAM", "CONF", "G", "W", "ADJOE", 
                     "ADJDE", "BARTHAG", "EFG_O", "EFG_D",
                     "TOR", "TORD", "ORB", "DRB", "FTR", 
                     "FTRD", "TWOPT", "DTWOPT", "OFFTH", "DEFTH", 
                     "ADJ_T", "WAB", "POSTSEASON", "SEED", "YEAR", "WINPER", "PREDWINPER", 
                     "POSTSEASON_SCORE")

SOS_data <- read.csv("SOS_DATA.csv")

names(SOS_data) <- c("TEAM", "SOS_RATING", "YEAR")

for (i in 1:nrow(SOS_data))
{
  string <- SOS_data$TEAM[i]
  words <- strsplit(string, "\\(")[[1]]
  
  teamName <- trimws(words[1])
  
  SOS_data$TEAM[i] <- teamName
}

merged_data <- cbb_data %>%
  left_join(SOS_data %>% dplyr::select(TEAM, SOS_RATING, YEAR), by = c("TEAM", "YEAR"))

cbb_data <- merged_data

naRatingIndex <- which(is.na(cbb_data$SOS_RATING))

cbb_data <- cbb_data[-naRatingIndex, ]

winPerOutlier <- which(cbb_data$WINPER > 1 | cbb_data$WINPER < 0)

cbb_data <- cbb_data[-winPerOutlier, ]

naPost <- which(is.na(cbb_data$POSTSEASON))
cbb_data$POSTSEASON[naPost] <- "N/A"

#-------------------------------------------------------------------------------

# Calculate Correlations

predictorNames <- c("ADJOE", "ADJDE", "EFG_O", "EFG_D", 
                    "TOR", "TORD", "ORB", "DRB", "FTR", 
                    "FTRD", "TWOPT", "DTWOPT", "OFFTH", "DEFTH", "SOS_RATING", 
                    "BARTHAG", "WAB")

storeCor <- c()
storeRSqd <- c()

for (i in 1:length(predictorNames))
{
  indCor <- cor(cbb_data[ ,predictorNames[i]], cbb_data$WINPER)
  storeCor <- c(storeCor, indCor)
  storeRSqd <- c(storeRSqd, indCor^2)
}

names(storeCor) <- predictorNames
names(storeRSqd) <- predictorNames
storeCor
storeRSqd

#-------------------------------------------------------------------------------

# Create Models

# MODEL 1 (PREDICTOR(S): WAB)

# Reason: WAB is selected due to its strong correlation with WINPER (0.8561277).
#   WAB also has the highest R^2 value (0.73295464)

lmCheck1 <- lm(WINPER ~ WAB, data = cbb_data)
summary(lmCheck1)$coefficients

# p-value for WAB = 0 < 0.05 (statistically significant model)

storecalcMSE1 <- c()

for (i in 1:10000)
{
  n1 <- sample(1:nrow(cbb_data), floor(0.8 * nrow(cbb_data)))
  train_data1 <- cbb_data[n1, ]
  test_data1 <- cbb_data[-n1, ]
  
  lm1 <- lm(WINPER ~ WAB, data = train_data1)
  storePred <- c(predict(lm1, test_data1[ , ]))
  
  indcalcMSE1 <- mean((test_data1[ , "WINPER"] - predict(lm1, test_data1[ , ]))^2)
  storecalcMSE1 <- c(storecalcMSE1, indcalcMSE1)
}

hist(storecalcMSE1, main = "The Sampling Distribution of MSE
     for 10,000 trials of M1", xlab = "MSE")
mean(storecalcMSE1)

# MODEL 2 (PREDICTOR(S): WAB & TWOPT)

# WAB is highly correlated with WINPER and has a high R^2 value
# TWOPT is moderately correlated with WINPER 
# WAB and TWOPT are only moderately correlated to one another
# Let's see the results 

lmCheck2 <- lm(WINPER ~ WAB + TWOPT, data = cbb_data)
summary(lmCheck2)$coefficients

# p-value for WAB = 0 < 0.05 (statistically significant)
# p-value for TWOPT = 7.8e^-95 (statistically significant)

storecalcMSE2 <- c()

for (i in 1:10000)
{
  n2 <- sample(1:nrow(cbb_data), floor(0.80 * nrow(cbb_data)))
  train_data2 <- cbb_data[n2, ]
  test_data2 <- cbb_data[-n2, ]
  
  lm2 <- lm(WINPER ~ WAB + TWOPT, data = train_data2)
  storePred <- c(predict(lm2, test_data2[ , ]))
  
  indcalcMSE2 <- mean((test_data2[ , "WINPER"] - predict(lm2, test_data2[ , ]))^2)
  storecalcMSE2 <- c(storecalcMSE2, indcalcMSE2)
}

hist(storecalcMSE2, main = "The Sampling Distribution of MSE
     for 10,000 trials of M2", xlab = "MSE")
mean(storecalcMSE2)

# MODEL 3 (PREDICTOR(S): FTR, OFFTHREE, BARTHAG)

# OFFTH is moderately correlated to WINPER
# FTR is weakly correlated to WINPER (let's discover what happens)
# BARTHAG is strongly correlated to WINPER

lmCheck3 <- lm(WINPER ~ BARTHAG + OFFTH + FTR, data = cbb_data)
summary(lmCheck3)$coefficients

# p-value for BARTHAG = 0 < 0.05 (statistically significant)
# p-value for TWOPT = 3.86e-50 (statistically significant)
# p-value for FTR = 6.69e-06 (statistically significant)

storecalcMSE3 <- c()

for (i in 1:10000)
{
  n3 <- sample(1:nrow(cbb_data), floor(0.80 * nrow(cbb_data)))
  train_data3 <- cbb_data[n3, ]
  test_data3 <- cbb_data[-n3, ]
  
  lm3 <- lm(WINPER ~ FTR + OFFTH + BARTHAG, data = train_data3)
  storePred <- c(predict(lm3, test_data3[ , ]))
  
  indcalcMSE3 <- mean((test_data3[ , "WINPER"] - predict(lm3, test_data3[ , ]))^2)
  storecalcMSE3 <- c(storecalcMSE3, indcalcMSE3)
}

hist(storecalcMSE3, main = "The Sampling Distribution of MSE
     for 10,000 trials of M3", xlab = "MSE")
mean(storecalcMSE3)

# MODEL 4 (PREDICTOR(S): SOS_RATING, ADJDE, EFG_O, DEFTH)

lmCheck4 <- lm(WINPER ~ SOS_RATING + ADJDE + EFG_O + DEFTH, data = cbb_data)
summary(lmCheck4)$coefficients

# p-value for SOS_RATING = 2.14e-108 < 0.05 (statistically significant)
# p-value for ADJDE = 2.3e-296 (statistically significant)
# p-value for EFG_O = 0 (statistically significant)
# p-value for DEFTH = 7.82e-25 (statistically significant)

storecalcMSE4 <- c()

for (i in 1:10000)
{
  n4 <- sample(1:nrow(cbb_data), floor(0.80 * nrow(cbb_data)))
  train_data4 <- cbb_data[n4, ]
  test_data4 <- cbb_data[-n4, ]
  
  lm4 <- lm(WINPER ~ SOS_RATING + ADJDE + EFG_O + DEFTH, data = train_data4)
  storePred <- c(predict(lm4, test_data4[ , ]))
  
  indcalcMSE4 <- mean((test_data4[ , "WINPER"] - predict(lm4, test_data4[ , ]))^2)
  storecalcMSE4 <- c(storecalcMSE4, indcalcMSE4)
}

hist(storecalcMSE4, main = "The Sampling Distribution of MSE
     for 10,000 trials of M4", xlab = "MSE")
mean(storecalcMSE4)

# MODEL 5 (PREDICTOR(S): BARTHAG)

lmCheck5 <- lm(WINPER ~ BARTHAG, data = cbb_data)
summary(lmCheck5)$coefficients

# p-value for WAB = 0 < 0.05 (statistically significant model)

par(mfrow = c(1, 2))

plot(lmCheck1, which = 1)

plot(lmCheck1, which = 2)

storecalcMSE5 <- c()

for (i in 1:1000)
{
  n5 <- sample(1:nrow(cbb_data), floor(0.8 * nrow(cbb_data)))
  train_data5 <- cbb_data[n5, ]
  test_data5 <- cbb_data[-n5, ]
  
  lm5 <- lm(WINPER ~ BARTHAG, data = train_data5)
  storePred <- c(predict(lm5, test_data[ , ]))
  
  indcalcMSE5 <- mean((test_data5[ , "WINPER"] - predict(lm5, test_data5[ , ]))^2)
  storecalcMSE5 <- c(storecalcMSE5, indcalcMSE5)
}

hist(storecalcMSE5)
mean(storecalcMSE5)

# MODEL 5 (PREDICTOR(S): ALL except FTR AND ADJOE/ADJDE)

# FTR and ADJOE/ADJDE are not significant to the model

lmCheck5 <- lm(WINPER ~ WAB + TWOPT + OFFTH + DEFTH + DTWOPT + TOR + TORD + BARTHAG
                 + ORB + DRB + FTRD + SOS_RATING, data = cbb_data)
summary(lmCheck5)$coefficients

storecalcMSE5 <- c()

for (i in 1:10000)
{
  n5 <- sample(1:nrow(cbb_data), floor(0.80 * nrow(cbb_data)))
  train_data5 <- cbb_data[n5, ]
  test_data5 <- cbb_data[-n5, ]
  
  lm5 <- lm(WINPER ~ WAB + TWOPT + DEFTH + OFFTH + DTWOPT + TOR + TORD + BARTHAG + 
              ORB + DRB + FTRD + SOS_RATING, data = train_data5)
  storePred <- c(predict(lm5, test_data5[ , ]))
  
  indcalcMSE5 <- mean((test_data5[ , "WINPER"] - predict(lm5, test_data5[ , ]))^2)
  storecalcMSE5 <- c(storecalcMSE5, indcalcMSE5)
}

hist(storecalcMSE5, main = "The Sampling Distribution of MSE
     for 10,000 trials of M5", xlab = "MSE")
mean(storecalcMSE5)

#-------------------------------------------------------------------------------

# SCORING SYSTEM BASED ON 11 YEARS OF DATA
# CAN BE USED FOR PREDICTING 2024 RANKINGS
# BETTER USE CASE MAY BE FOR SEEING THE MOST 
# CONSISTENT TEAMS IN CBB THROUGHOUT THE YEARS
# THIS CAN HELP WITH SELECTING THE RIGHT TEAMS ON BRACKETS FOR 2025

# Clean Postseason data 

for (i in 1:nrow(cbb_data))
{
  if (cbb_data$POSTSEASON[i] == "N/A")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 0
  }
  
  if (cbb_data$POSTSEASON[i] == "Champions")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 10
  }
  
  if (cbb_data$POSTSEASON[i] == "2ND")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 8
  }
  
  if (cbb_data$POSTSEASON[i] == "F4")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 7
  }
  
  if (cbb_data$POSTSEASON[i] == "E8")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 6
  }
  
  if (cbb_data$POSTSEASON[i] == "S16")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 4
  }
  
  if (cbb_data$POSTSEASON[i] == "R32")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 2
  }
  
  if (cbb_data$POSTSEASON[i] == "R64")
  {
    cbb_data$POSTSEASON_SCORE[i] <- 1
  }
}

# USE MODEL TO PREDICT TOP 25 TEAMS FROM 2013-2024

# MARCH MADNESS SCORE
# CHAMPION -- 10
# 2ND -- 8
# F4 -- 7
# E8 -- 6
# S16 -- 4
# R32 -- 2
# R64 -- 1

# Store Pred Win Percentages
storeBestPred <- predict(lm5, cbb_data[, ])

# Assign Pred Win Percentages to cbb_data
for (i in 1:nrow(cbb_data))
{
  cbb_data$PREDWINPER[i] <- storeBestPred[i]
}

# Store every team uniquely from 2013-2024
uniqueTeams <- unique(c(cbb_data$TEAM))

# New data frame to store scores
pred_data <- data.frame(uniqueTeams)

# Calculate averages for win percentages
for (i in 1:nrow(pred_data))
{
  storePredWinPer <- c()
  teamIndex <- which(cbb_data$TEAM == uniqueTeams[i])
  storePredWinPer <- c(cbb_data[teamIndex, "PREDWINPER"])
  avgPredWinPer <- mean(storePredWinPer)
  pred_data$AVGWINPER[i] <- avgPredWinPer
}

# Add MM scores
for (i in 1:nrow(pred_data))
{
  storeMMSuc <- c()
  teamIndex <- which(cbb_data$TEAM == uniqueTeams[i])
  storeMMSuc <- c(cbb_data[teamIndex, "POSTSEASON_SCORE"])
  avgMMScore <- mean(storeMMSuc)
  pred_data$MMSCORE[i] <- avgMMScore
}

# ADD SOS_RATING 
for (i in 1:nrow(pred_data))
{
  storeSOS_RATING <- c()
  teamIndex <- which(cbb_data$TEAM == uniqueTeams[i])
  storeSOS_RATING <- c(cbb_data[teamIndex, "SOS_RATING"])
  avgSOS_RATING <- mean(storeSOS_RATING)
  pred_data$AVGSOSRATING[i] <- avgSOS_RATING
}

names(pred_data) <- c("TEAM", "AVG_PRED_WINPER", "MM_SCORE", "AVG_SOS_RTG")

pred_data$NORM_PRED_WINPER <- scale(pred_data$AVG_PRED_WINPER, 
                                    center = min(pred_data$AVG_PRED_WINPER), 
                                    scale = max(pred_data$AVG_PRED_WINPER) - min(pred_data$AVG_PRED_WINPER))

pred_data$NORM_MM_SCORE <- scale(pred_data$MM_SCORE, 
                                 center = min(pred_data$MM_SCORE), 
                                 scale = max(pred_data$MM_SCORE) - min(pred_data$MM_SCORE))

pred_data$NORM_SOS_RTG <- scale(pred_data$AVG_SOS_RTG, 
                                   center = min(pred_data$AVG_SOS_RTG), 
                                   scale = max(pred_data$AVG_SOS_RTG) - min(pred_data$AVG_SOS_RTG))

pred_data$WEIGHTED_SCORE <- (pred_data$NORM_PRED_WINPER * 0.25) + (pred_data$NORM_MM_SCORE * 0.5) + 
  (pred_data$NORM_SOS_RTG * 0.25)

pred_data$WEIGHTED_SCORE <- pred_data$WEIGHTED_SCORE * 100

pred_data <- pred_data[order(pred_data$WEIGHTED_SCORE, decreasing = TRUE), ]

print(pred_data[1:25, ])

#-------------------------------------------------------------------------------

# WAY TOO EARLY TOP 25 FOR 2025 (INSPIRATION: ESPN)

cbb_24 <- subset(cbb_data, YEAR == "2024")
cbb_24

predTop25 <- data.frame(cbb_24$TEAM, cbb_24$PREDWINPER, cbb_24$POSTSEASON_SCORE, 
                        cbb_24$SOS_RATING)

names(predTop25) <- c("TEAM", "PRED_WINPER", "MM_SCORE", "SOS_RATING")

predTop25$NORM_PRED_WINPER <- scale(predTop25$PRED_WINPER, 
                                    center = min(predTop25$PRED_WINPER), 
                                    scale = max(predTop25$PRED_WINPER) - min(predTop25$PRED_WINPER))

predTop25$NORM_MM_SCORE <- scale(predTop25$MM_SCORE, 
                                 center = min(predTop25$MM_SCORE), 
                                 scale = max(predTop25$MM_SCORE) - min(predTop25$MM_SCORE))

predTop25$NORM_SOS_RATING <- scale(predTop25$SOS_RATING, 
                                center = min(predTop25$SOS_RATING), 
                                scale = max(predTop25$SOS_RATING) - min(predTop25$SOS_RATING))

predTop25$WEIGHTED_SCORE <- (predTop25$NORM_PRED_WINPER * 0.25) + (predTop25$NORM_MM_SCORE * 0.5) + 
  (predTop25$NORM_SOS_RATING * 0.25)

predTop25$WEIGHTED_SCORE <- predTop25$WEIGHTED_SCORE * 100

predTop25 <- predTop25[order(predTop25$WEIGHTED_SCORE, decreasing = TRUE), ]

print(predTop25[1:25, ])

#-------------------------------------------------------------------------------
# PLOTTING T25 TEAMS BASED ON 2013-2024 DATA

library(ggplot2)
library(ggimage)
library(png)

GZIndex <- which(pred_data$TEAM == "Gonzaga")
DukeIndex <- which(pred_data$TEAM == "Duke")
KSIndex <- which(pred_data$TEAM == "Kansas")
UNCIndex <- which(pred_data$TEAM == "North Carolina")
VillIndex <- which(pred_data$TEAM == "Villanova")
WIIndex <- which(pred_data$TEAM == "Wisconsin")
UCLAIndex <- which(pred_data$TEAM == "UCLA")
AZIndex <- which(pred_data$TEAM == "Arizona")
KYIndex <- which(pred_data$TEAM == "Kentucky")
MIIndex <- which(pred_data$TEAM == "Michigan")
MIStIndex <- which(pred_data$TEAM == "Michigan St.")
PurdueIndex <- which(pred_data$TEAM == "Purdue")
VAIndex <- which(pred_data$TEAM == "Virginia")
UconnIndex <- which(pred_data$TEAM == "Connecticut")
BaylorIndex <- which(pred_data$TEAM == "Baylor")
OregonIndex <- which(pred_data$TEAM == "Oregon")
FLIndex <- which(pred_data$TEAM == "Florida")
UHIndex <- which(pred_data$TEAM == "Houston")
IAStIndex <- which(pred_data$TEAM == "Iowa St.")
TennIndex <- which(pred_data$TEAM == "Tennessee")
CrIndex <- which(pred_data$TEAM == "Creighton")
SyIndex <- which(pred_data$TEAM == "Syracuse")
SDStIndex <- which(pred_data$TEAM == "San Diego St.")
LouisIndex <- which(pred_data$TEAM == "Louisville")
FLMIAIndex <- which(pred_data$TEAM == "Miami FL")

# Create a data frame with the coordinates and logo
logo_data <- data.frame(
  winPer = c(pred_data$AVG_PRED_WINPER[GZIndex], pred_data$AVG_PRED_WINPER[DukeIndex], 
             pred_data$AVG_PRED_WINPER[KSIndex], pred_data$AVG_PRED_WINPER[UNCIndex], 
             pred_data$AVG_PRED_WINPER[VillIndex], pred_data$AVG_PRED_WINPER[WIIndex], 
             pred_data$AVG_PRED_WINPER[UCLAIndex], pred_data$AVG_PRED_WINPER[AZIndex], 
             pred_data$AVG_PRED_WINPER[KYIndex], pred_data$AVG_PRED_WINPER[MIIndex], 
             pred_data$AVG_PRED_WINPER[MIStIndex], pred_data$AVG_PRED_WINPER[PurdueIndex], 
             pred_data$AVG_PRED_WINPER[VAIndex], pred_data$AVG_PRED_WINPER[UconnIndex], 
             pred_data$AVG_PRED_WINPER[BaylorIndex], pred_data$AVG_PRED_WINPER[OregonIndex], 
             pred_data$AVG_PRED_WINPER[FLIndex], pred_data$AVG_PRED_WINPER[UHIndex], 
             pred_data$AVG_PRED_WINPER[IAStIndex], pred_data$AVG_PRED_WINPER[TennIndex], 
             pred_data$AVG_PRED_WINPER[CrIndex], pred_data$AVG_PRED_WINPER[SyIndex], 
             pred_data$AVG_PRED_WINPER[SDStIndex], pred_data$AVG_PRED_WINPER[LouisIndex], 
             pred_data$AVG_PRED_WINPER[FLMIAIndex]),
  mmScore = c(pred_data$MM_SCORE[GZIndex], pred_data$MM_SCORE[DukeIndex], 
              pred_data$MM_SCORE[KSIndex], pred_data$MM_SCORE[UNCIndex], 
              pred_data$MM_SCORE[VillIndex], pred_data$MM_SCORE[WIIndex], 
              pred_data$MM_SCORE[UCLAIndex], pred_data$MM_SCORE[AZIndex], 
              pred_data$MM_SCORE[KYIndex], pred_data$MM_SCORE[MIIndex], 
              pred_data$MM_SCORE[MIStIndex], pred_data$MM_SCORE[PurdueIndex], 
              pred_data$MM_SCORE[VAIndex], pred_data$MM_SCORE[UconnIndex], 
              pred_data$MM_SCORE[BaylorIndex], pred_data$MM_SCORE[OregonIndex], 
              pred_data$MM_SCORE[FLIndex], pred_data$MM_SCORE[UHIndex], 
              pred_data$MM_SCORE[IAStIndex], pred_data$MM_SCORE[TennIndex], 
              pred_data$MM_SCORE[CrIndex], pred_data$MM_SCORE[SyIndex], 
              pred_data$MM_SCORE[SDStIndex], pred_data$MM_SCORE[LouisIndex], 
              pred_data$MM_SCORE[FLMIAIndex]), 
  logo = c("https://content.sportslogos.net/logos/31/691/full/gonzaga_bulldogs_logo_secondary_19984325.png", 
           "https://content.sportslogos.net/logos/31/663/full/duke_blue_devils_logo_primary_19712631.png", 
           "https://content.sportslogos.net/logos/32/718/full/kansas_jayhawks_logo_primary_20059568.png", 
           "https://content.sportslogos.net/logos/33/775/full/north_carolina_tar_heels_logo_primary_20152828.png", 
           "https://content.sportslogos.net/logos/35/897/full/villanova_wildcats_logo_primary_20024534.png", 
           "https://content.sportslogos.net/logos/35/914/full/wisconsin_badgers_logo_primary_2017_sportslogosnet-3691.png", 
           "https://content.sportslogos.net/logos/35/882/full/ucla_bruins_logo_primary_19969500.png", 
           "https://content.sportslogos.net/logos/30/603/full/arizona_wildcats_logo_secondary_2011_sportslogosnet-5717.png", 
           "https://content.sportslogos.net/logos/32/721/full/kentucky_wildcats_logo_primary_20167330.png", 
           "https://content.sportslogos.net/logos/32/750/full/michigan_wolverines_logo_alternate_2016_sportslogosnet-8427.png", 
           "https://content.sportslogos.net/logos/32/751/full/michigan_state_spartans_logo_primary_19778752.png", 
           "https://content.sportslogos.net/logos/33/809/full/purdue_boilermakers_logo_primary_20128157.png", 
           "https://content.sportslogos.net/logos/35/898/full/7315_virginia_cavaliers-primary-2020.png", 
           "https://content.sportslogos.net/logos/35/884/full/uconn_huskies_logo_primary_20139813.png",
           "https://content.sportslogos.net/logos/30/613/full/baylor_bears_logo_primary_20197416.png", 
           "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f8/Oregon_Ducks_logo.svg/1200px-Oregon_Ducks_logo.svg.png", 
           "https://cdn.freebiesupply.com/logos/large/2x/florida-gators-2-logo-png-transparent.png", 
           "https://content.sportslogos.net/logos/31/700/full/houston_cougars_logo_primary_2017_sportslogosnet-1807.png", 
           "https://content.sportslogos.net/logos/32/713/full/iowa_state_cyclones_logo_secondary_20088357.png", 
           "https://content.sportslogos.net/logos/34/861/full/tennessee_volunteers_logo_primary_20159624.png", 
           "https://content.sportslogos.net/logos/30/652/full/creighton_bluejays_logo_primary_20135465.png", 
           "https://content.sportslogos.net/logos/34/859/full/syracuse_orange_logo_secondary_20063635.png", 
           "https://content.sportslogos.net/logos/34/826/full/san_diego_state_aztecs_logo_primary_20022997.png", 
           "https://content.sportslogos.net/logos/32/734/full/louisville_cardinals_logo_primary_20133882.png",
           "https://1000logos.net/wp-content/uploads/2018/09/Miami-Hurricanes-Logo.png")
)

avgWinPer <- mean(logo_data$winPer)
avgMMScore <- mean(logo_data$mmScore)

ggplot(logo_data, mapping = aes(x = winPer, y = mmScore)) +
  geom_hline(yintercept = avgMMScore, linetype = "longdash", linewidth = 1.0) + 
  geom_vline(xintercept = avgWinPer, linetype = "longdash", linewidth = 1.0) + 
  geom_image(aes(image = logo), size = 0.1) +
  labs(title = "Average MM Score vs Average Predicted Win Percentage Among
  T25 Teams from 2013-2024", 
  x = "Average Predicted Win Percentage", y = "March Madness Score") +
  scale_x_continuous() +
  scale_y_continuous()

#--------------------------------------------------------------------------------

# PLOTTING TOP 25 TEAMS FOR THE 2025 SEASON

top25Uconn <- which(predTop25$TEAM == "Connecticut")
top25Purdue <- which(predTop25$TEAM == "Purdue")
top25Bama <- which(predTop25$TEAM == "Alabama")
top25Tenn <- which(predTop25$TEAM == "Tennessee")
top25Ill <- which(predTop25$TEAM == "Illinois")
top25Duke <- which(predTop25$TEAM == "Duke")
top25NCSt <- which(predTop25$TEAM == "North Carolina St.")
top25UH <- which(predTop25$TEAM == "Houston")
top25Clem <- which(predTop25$TEAM == "Clemson")
top25IASt <- which(predTop25$TEAM == "Iowa St.")
top25AZ <- which(predTop25$TEAM == "Arizona")
top25MQ <- which(predTop25$TEAM == "Marquette")
top25UNC <- which(predTop25$TEAM == "North Carolina")
top25Cre <- which(predTop25$TEAM == "Creighton")
top25GZ <- which(predTop25$TEAM == "Gonzaga")
top25SDSt <- which(predTop25$TEAM == "San Diego St.")
top25Baylor <- which(predTop25$TEAM == "Baylor")
top25KS <- which(predTop25$TEAM == "Kansas")
top25MISt <- which(predTop25$TEAM == "Michigan St.")
top25Col <- which(predTop25$TEAM == "Colorado")
top25TX <- which(predTop25$TEAM == "Texas")
top25WashSt <- which(predTop25$TEAM == "Washington St.")
top25Aub <- which(predTop25$TEAM == "Auburn")
top25Day <- which(predTop25$TEAM == "Dayton")
top25North <- which(predTop25$TEAM == "Northwestern")


logo_data_2 <- data.frame(
  teams = c("Connecticut", "Purdue", "Alabama", "Tennessee", "Illinois", "Duke", 
            "NC St.", "Houston", "Clemson", "Iowa St.", "Arizona", "Marquette", 
            "UNC", "Creighton", "Gonzaga", "SDST", "Baylor", 
            "Kansas", "Mich St.", "Colorado", "Texas", "Wash St.", 
            "Auburn", "Dayton", "Northwestern"),
  scores = c(predTop25$WEIGHTED_SCORE[top25Uconn], predTop25$WEIGHTED_SCORE[top25Purdue], 
             predTop25$WEIGHTED_SCORE[top25Bama], predTop25$WEIGHTED_SCORE[top25Tenn], 
             predTop25$WEIGHTED_SCORE[top25Ill], predTop25$WEIGHTED_SCORE[top25Duke], 
             predTop25$WEIGHTED_SCORE[top25NCSt], predTop25$WEIGHTED_SCORE[top25UH], 
             predTop25$WEIGHTED_SCORE[top25Clem], predTop25$WEIGHTED_SCORE[top25IASt], 
             predTop25$WEIGHTED_SCORE[top25AZ], predTop25$WEIGHTED_SCORE[top25MQ], 
             predTop25$WEIGHTED_SCORE[top25UNC], predTop25$WEIGHTED_SCORE[top25Cre], 
             predTop25$WEIGHTED_SCORE[top25GZ], predTop25$WEIGHTED_SCORE[top25SDSt], 
             predTop25$WEIGHTED_SCORE[top25Baylor], predTop25$WEIGHTED_SCORE[top25KS],
             predTop25$WEIGHTED_SCORE[top25MISt], predTop25$WEIGHTED_SCORE[top25Col], 
             predTop25$WEIGHTED_SCORE[top25TX], predTop25$WEIGHTED_SCORE[top25WashSt], 
             predTop25$WEIGHTED_SCORE[top25Aub], predTop25$WEIGHTED_SCORE[top25Day], 
             predTop25$WEIGHTED_SCORE[top25North]), 
  logo = c("https://content.sportslogos.net/logos/35/884/full/uconn_huskies_logo_primary_20139813.png", 
           "https://content.sportslogos.net/logos/33/809/full/purdue_boilermakers_logo_primary_20128157.png", 
           "https://content.sportslogos.net/logos/30/597/full/alabama_crimson_tide_logo_primary_20047887.png", 
           "https://content.sportslogos.net/logos/34/861/full/tennessee_volunteers_logo_wordmark_20053462.png", 
           "https://content.sportslogos.net/logos/32/706/full/illinois_fighting_illini_logo_primary_2022_sportslogosnet-6702.png", 
           "https://content.sportslogos.net/logos/31/663/full/duke_blue_devils_logo_alternate_19813965.png", 
           "https://content.sportslogos.net/logos/33/777/full/north_carolina_state_wolfpack_logo_secondary_20053880.png", 
           "https://content.sportslogos.net/logos/31/700/full/houston_cougars_logo_primary_2017_sportslogosnet-1807.png", 
           "https://www.clemson.edu/brand/resources/logos/seal/orange-purple.png", 
           "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Iowa_State_Cyclones_logo.svg/2560px-Iowa_State_Cyclones_logo.svg.png", 
           "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Arizona_Wildcats_logo.svg/1108px-Arizona_Wildcats_logo.svg.png", 
           "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/Marquette_mu_small.png/640px-Marquette_mu_small.png", 
           "https://seeklogo.com/images/U/unc-tar-heels-logo-B3ABC215A0-seeklogo.com.png", 
           "https://upload.wikimedia.org/wikipedia/en/thumb/6/6f/Creighton_Bluejays_logo.svg/1200px-Creighton_Bluejays_logo.svg.png", 
           "https://seeklogo.com/images/G/gonzaga-bulldogs-logo-2E22CB7EE0-seeklogo.com.png", 
           "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7c/San_Diego_State_Aztecs_logo.svg/2560px-San_Diego_State_Aztecs_logo.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/b/b0/2018_Baylor_Athletics_Logo.png", 
           "https://www.gannett-cdn.com/content-pipeline-sports-images/sports2/cbk/logos/1456.png?format=png8&auto=webp&width=230", 
           "https://www.khcsports.com/images/products/Michigan-State-Spartans-basketball-disc-wall-sign.png", 
           "https://upload.wikimedia.org/wikipedia/en/thumb/d/d3/Colorado_Buffaloes_logo.svg/1200px-Colorado_Buffaloes_logo.svg.png", 
           "https://upload.wikimedia.org/wikipedia/en/thumb/e/e1/University_of_Texas_at_Austin_seal.svg/1200px-University_of_Texas_at_Austin_seal.svg.png", 
           "https://cdn.freebiesupply.com/logos/large/2x/washington-state-cougars-logo-png-transparent.png", 
           "https://cdn.freebiesupply.com/logos/large/2x/auburn-tigers-logo-png-transparent.png", 
           "https://1000logos.net/wp-content/uploads/2019/12/Dayton-Flyers-Logo-2014.png",
           "https://1000logos.net/wp-content/uploads/2019/12/Northwestern-Wildcats-Logo-1981.png")
)

logo_data_2$teams <- factor(logo_data_2$teams, levels = logo_data_2$teams[order(logo_data_2$scores, decreasing = TRUE)])

ggplot(logo_data_2, mapping = aes(x = teams, y = scores)) +
  geom_bar(stat = "identity", fill = c("#000E2F", "#CFB991", "#9E1B32", "#FF8200", "#E84A27", "#003087", 
                                       "#CC0000", "#C8102E", "#522D80", "#F1BE48", "#003366", "#FFCC00", 
                                       "#7BAFD4", "#005CA9", "#C1C6C8", "#000000", "#154734", "#0051BA",
                                       "#18453B", "#CFB87C", "#BF5700", "#981E32", "#0C2340", "#004B8D",
                                       "#4E2A84")) +
  geom_image(aes(image = logo), size = 0.1) +  # Overlay images on top of bars
  labs(title = "Predicted T25 Rankings for the 2024-2025 CBB Season Based on
       the Weighted Score", x = "Team", 
       y = "Weighted Score")  # Add labels
