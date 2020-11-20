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
#require(dplyr)
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
nba$home_f <- factor(nba$Home)
nba$DeffRebounds <- nba$TotalRebounds - nba$OffRebounds
nba$Cumu.DeffRebounds <- nba$Cumu.TotalRebounds - nba$Cumu.OffRebounds
nba$Cumu.Opp.DeffRebounds <- nba$Cumu.Opp.TotalRebounds - nba$Opp.OffRebounds
# Compute difference
nba$Cumu.TeamPoints.Diff <- nba$Cumu.TeamPoints - nba$Cumu.OpponentPoints
nba$Cumu.FieldGoalsP.Diff <- 100*(nba$Cumu.FieldGoals. - nba$Cumu.Opp.FieldGoals.)
nba$Cumu.Assists.Diff <- nba$Cumu.Assists - nba$Cumu.Opp.Assists
nba$Cumu.Foul.Diff <- nba$Cumu.TotalFouls - nba$Cumu.Opp.TotalFouls
nba$Cumu.DeffRebounds.Diff <- nba$Cumu.DeffRebounds - nba$Cumu.Opp.DeffRebounds
nba$Cumu.OffRebounds.Diff <- nba$Cumu.OffRebounds - nba$Cumu.Opp.OffRebounds
nba$Cumu.Turnovers.Diff <- nba$Cumu.Turnovers - nba$Cumu.Opp.Turnovers
nba$Cumu.Streak.Diff <- nba$Streak - nba$OppStreak
nba$Cumu.Records.Diff <- nba$Records - nba$OppRecords

# Subset data for Game# > 10
mask <- nba['Game'] > 5
nba_subset <- nba[mask, ]
summary(nba_subset)

############################
# Model Building & Selection
############################
# FROM EDA:
# Assist ***
# Defensive rebound ***
# Streak ***
# Record ***
# Is back to back ***
# Is Opponent back to back ***
# Foul difference ***
# Offensive rebound **
# Turnover**

# Factor team instead one random effect
Null.Model <- glm(win_lose_f~
                      Cumu.TeamPoints.Diff
                    ,family=binomial(link=logit),data=nba_subset)

summary(Null.Model)
vif(Null.Model)

Full.Model <- glm(win_lose_f~
                  Cumu.TeamPoints.Diff+
                  Cumu.FieldGoalsP.Diff+
                  Cumu.Assists.Diff+
                  Cumu.DeffRebounds.Diff+
                  Cumu.OffRebounds.Diff+
                  Cumu.Foul.Diff+
                  Cumu.Turnovers.Diff+
                  Cumu.Streak.Diff+
                  Cumu.Records.Diff+
                  is_b2b_f+
                  is_b2b_opp_f
                  ,family=binomial(link=logit),data=nba_subset)

summary(Full.Model)
vif(Full.Model)

Final.Model <- glmer(win_lose_f~
                       Cumu.TeamPoints.Diff+
                       Cumu.Assists.Diff+
                       Cumu.DeffRebounds.Diff+
                       Cumu.Turnovers.Diff+
                       Cumu.Streak.Diff+
                       Cumu.Records.Diff+
                       home_f+
                       is_b2b_f+
                       is_b2b_opp_f+
                       (1|team_f)
                     ,family=binomial(link=logit),data=nba_subset)

summary(Final.Model)
vif(Final.Model)
dotplot(ranef(Final.Model, condVar=TRUE))

# AIC, BIC, Stepwise selection

# AIC: Dropped Cumu.OffRebounds.Diff, Cumu.Foul.Diff
aic.model <- step(Null.Model,scope=formula(Full.Model),direction="both",trace=0,k=2)
summary(aic.model)

# BIC: Dropped Cumu.OffRebounds.Diff, Cumu.Foul.Diff, Cumu.Turnovers.Diff,
#      Cumu.Assists.Diff, Cumu.FieldGoalsP.Diff, 
n <- nrow(nba_subset)
bic.model <- step(Null.Model,scope=formula(Full.Model),direction="both",trace=0,k=log(n))
summary(bic.model)

# Stepwise: Dropped Cumu.OffRebounds.Diff, Cumu.Foul.Diff, 
stepwise.model <- step(Null.Model, scope = formula(Full.Model),direction="both",trace=0)
summary(stepwise.model)


# Output summary
stargazer(Final.Model, title = "Summary of the Final Model", 
          align = TRUE, dep.var.labels = c("Win or Lose"), 
          type = "text", no.space = TRUE, out = "Final_Model.html")

############################
# Model Assessment
############################

# ROC curve plot
roc(nba_subset$win_lose_f,fitted(Final.Model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

# Confusion matrix
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(Final.Model) >= 0.508, "W","L")),
                            as.factor(nba_subset$win_lose_f),positive = "W")
Conf_mat$table

# Accuracy: 65.2%
Conf_mat$overall["Accuracy"];

# Sensitivity: 63.4 % and specificity: 66.9 %
Conf_mat$byClass[c("Sensitivity","Specificity")]



