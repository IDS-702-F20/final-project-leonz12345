# Import libaries
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(grid)
library(rms)
library(stargazer)
library(lme4)
library(stringr)
require(dplyr)
library(dplyr)
library(lme4)

############################
# Import & Prepare Data Set
############################
nba <- read.csv("/Users/leonz/Desktop/IDS702/final-project-leonz12345/Code & Data/nba_cleaned.csv")
nba$team_f <- factor(nba$Team)
nba$win_lose_f <- factor(nba$WINorLOSS)
nba$is_b2b_f <- factor(nba$Is_b2b)
nba$is_b2b_opp_f <- factor(nba$Is_b2b_opp)
nba$DeffRebounds <- nba$TotalRebounds - nba$OffRebounds
nba$FoulDiff <- nba$TotalFouls - nba$Opp.TotalFouls

# Subset data for Game# > 10
mask <- nba['Game'] > 10
nba_subset <- nba[mask, ]
summary(nba_subset)

############################
# Response Variable vs Predictors
############################

# Compare the total team pts for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=TeamPoints, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Total Team Points vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the total field goals for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=FieldGoals, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Total Field Goals vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the field goals % for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=FieldGoals., y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Field Goal % vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the Free Throws for winning games vs losing games
ggplot(nba_subset,aes(x=FreeThrows, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Total Opponent Field Goals vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the Total Rebounds for winning games vs losing games
ggplot(nba_subset,aes(x=TotalRebounds, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Total Rebound vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# *************************************
# Compare the Offensive Rebounds for winning games vs losing games
# *** Transform the data to see defensive rebound ***
ggplot(nba_subset,aes(x=OffRebounds, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Offensive Rebound vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the Defensive Rebounds for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=DeffRebounds, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Defensive Rebound vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
# *************************************

# *************************************
# Defensive rebound is much more valuable than offensive rebound ???
# *************************************

# *************************************
# Compare the Assist for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=Assists, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Assist vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
# *************************************

# Compare the blocks for winning games vs losing games
ggplot(nba_subset,aes(x=Blocks, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Blocks vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the Turnover for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=Turnovers, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Turnovers vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Compare the fouls for winning games vs losing games
# Check foul difference
ggplot(nba_subset,aes(x=TotalFouls, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Fouls vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# *************************************
# Compare the foul difference for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=FoulDiff, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Foul Diffence (t=Team - oppTeam) vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
# *************************************

# Compare the record for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=Records, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Records vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Distribution of steaks
ggplot(nba_subset,aes(Streak)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Wining/Losing Streaks",y="Density") + theme_classic()

# *************************************
# Compare the streak for winning games vs losing games
# USEFUL PREDICTOR
ggplot(nba_subset,aes(x=Streak, y=win_lose_f, fill=win_lose_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Streak vs W/L") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
# *************************************

# *************************************
# Check back to back game for winning games vs losing games
# USEFUL PREDICTOR
b2b_wl <- subset(nba_subset, select = c(win_lose_f, is_b2b_f) )
apply(table(b2b_wl)/sum(table(b2b_wl)),2, function(x) x/sum(x))

# Check opponent back to back game for winning games vs losing games
# USEFUL PREDICTOR
oppb2b_wl <- subset(nba_subset, select = c(win_lose_f, is_b2b_opp_f) )
apply(table(oppb2b_wl)/sum(table(oppb2b_wl)),2, function(x) x/sum(x))
# *************************************

#######################
# Correlation check:
# field goals vs total points
# *** Super correlated: drop one
chisq.test(nba_subset$FieldGoals, nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=FieldGoals, y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Field Goals vs Team Points",x="Field Goals",y="Team Points") +
  theme(plot.title = element_text(hjust = 0.5))

# field goal percent vs total points
# *** Super correlated: drop one
chisq.test(nba_subset$FieldGoals., nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=FieldGoals., y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Field Goal % vs Team Points",x="Field Goal %",y="Team Points") +
  theme(plot.title = element_text(hjust = 0.5))

# *************************************
# Choose one between total points, field goal, and field goal percent
# *************************************

# rebound vs total points
# *** Slightly correlated
chisq.test(nba_subset$TotalRebounds, nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=TotalRebounds, y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Total Rebound vs Team Points",x="Field Goals",y="Rebounds") +
  theme(plot.title = element_text(hjust = 0.5))

# assists vs total points
# Highly Correlated
chisq.test(nba_subset$Assists, nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=Assists, y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Total Assists vs Team Points",x="Total Assists",y="Team Points") +
  theme(plot.title = element_text(hjust = 0.5))

# OffRebounds vs TeamPoints
# *** No Correlation
chisq.test(nba_subset$OffRebounds, nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=OffRebounds, y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Offensive Rebounds vs Team Points",x="Offensive Rebounds",y="Team Points") +
  theme(plot.title = element_text(hjust = 0.5))

# DeffRebounds vs TeamPoints
# Slight Correlation
chisq.test(nba_subset$DeffRebounds, nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=DeffRebounds, y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Deffensive Rebounds vs Team Points",x="Defensive Rebounds",y="Team Points") +
  theme(plot.title = element_text(hjust = 0.5))

# Steal vs opp.turnover
# field goals vs assist
# field goals vs turn over

# Assists vs Field Goal %
chisq.test(nba_subset$Assists, nba_subset$FieldGoals., simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=Assists, y=FieldGoals.)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Total Assists vs Field Goal %",x="Total Assists",y="Field Goal %") +
  theme(plot.title = element_text(hjust = 0.5))


# DeffRebounds vs Field Goal %
# No Correlation (Better than DeffRebounds vs Total points)
chisq.test(nba_subset$DeffRebounds, nba_subset$FieldGoals., simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=DeffRebounds, y=FieldGoals.)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Deffensive Rebounds vs Field Goal %",x="Defensive Rebounds",y="Field Goal %") +
  theme(plot.title = element_text(hjust = 0.5))

# Streak vs Field Goal %
# Slight Correlation (Better than Streak vs Total points)
chisq.test(nba_subset$Streak, nba_subset$FieldGoals., simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=Streak, y=FieldGoals.)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Streak vs Field Goal %",x="Streak",y="Field Goal %") +
  theme(plot.title = element_text(hjust = 0.5))

# Records vs Teampoints
# Slight Correlation (Better than Streak vs Total points)
chisq.test(nba_subset$Records, nba_subset$TeamPoints, simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=Records, y=TeamPoints)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Records vs Field Goal %",x="Records",y="Field Goal %") +
  theme(plot.title = element_text(hjust = 0.5))

# Records vs Field Goal %
# Slight Correlation (Equal to Streak vs Total points)
chisq.test(nba_subset$Records, nba_subset$FieldGoals., simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=Records, y=FieldGoals.)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Records vs Field Goal %",x="Records",y="Field Goal %") +
  theme(plot.title = element_text(hjust = 0.5))

# isb2b vs Field Goal %
# No Correlation (Equal to Streak vs Total points)
ggplot(nba_subset,aes(x=FieldGoals., y=is_b2b_f, fill=is_b2b_f)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Field Goal % vs Is back to back") +
  theme_classic() +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# *****************
# Field Goals Attempts vs Field Goal %
# Strong Correlation
chisq.test(nba_subset$FieldGoalsAttempted, nba_subset$FieldGoals., simulate.p.value=TRUE)
ggplot(nba_subset,aes(x=FieldGoalsAttempted, y=FieldGoals.)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="FieldGoalsAttempted vs Field Goal %",x="FieldGoalsAttempted",y="Field Goal %") +
  theme(plot.title = element_text(hjust = 0.5))
# *****************


################
# For modeling, try:
# TotalPoints/FieldGoals/FieldGoal% pick one that is least correlated with other predictors
# Assist ***
# Defensive rebound ***
# Streak ***
# Record ***
# Is back to back ***
# Foul difference ***
# Offensive rebound **
# Turnover**
################