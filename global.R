###################
# global.R
# 
# Anything shared between your ui and server.
###################

library(shiny)
library(shinydashboard)
library(DT)
library(gbm)
library(readr)
library(DALEX)
library(ingredients)
library(iBreakDown)
library(ggplot2)
library(scales)
library(randomForest)
library(imputeTS)
library(dplyr)
library(dashboardthemes)
library(fields)
library(ggthemes)

set.seed(361309)

load("nba_workspace.RData")

# # read data
# nba <- as.data.frame(read.csv("data/nba_dataset.csv"))
# 
# # data modification & selecting variables
# nba <- nba %>% plyr::rename(., c("pay" = "Salary"))
# nba$Pos <- as.factor(nba$Pos)
# nba$Tm <- as.factor(nba$Tm)
# nba['height'] <- nba['height']*2.54
# nba['weight'] <- nba['weight']*0.45
# 
# nba_sel <- nba[,c(2:3, 5:41)] %>%
#   mutate_if(is.numeric, round, digits = 2)
# nba_sel["X3P."] <- round(na_mean(nba_sel["X3P."]), digits = 2)
# nba_sel["FT."] <- round(na_mean(nba_sel["FT."]), digits = 2)
# 
# 
# team_names <- team_names <- data.frame(unique(nba_sel$Tm))
# colnames(team_names) <- c('Tm')
# team_names$TeamFull <- c('Dallas Mavericks', 'Indiana Pacers', 'Orlando Magic', 'Boston Celtics', 'Portland Trail Blazers', 'Phoenix Suns', 'Utah Jazz', 'Oklahoma City Thunder', 'Philadelphia 76ers', 'New Orleans Pelicans', 'Golden State Warriors', 'Detroit Pistons', 'Two team score', 'Memphis Grizzlies', 'Minnesota Timberwolves', 'Sacramento Kings', 'Los Angeles Clippers' , 'Houston Rockets', 'Chicago Bulls', 'Washington Wizzards', 'Los Angeles Lakers', 'Brooklyn Nets', 'Toronto Raptors', 'San Antonio Spurs', 'New York Knicks', 'Cleveland Cavaliers', 'Charlotte Hornets', 'Denver Nuggets', 'Atlanta Hawks', 'Miami Heat', 'Milwaukee Bucks')
# 
# # GBM Model
# # Changeable model parameters
# nba_gbm <- gbm(Salary ~ . , data = nba_sel[,-1], n.trees = 250, interaction.depth = 3)
# 
# # RF Model
# nba_rf <- randomForest(Salary ~ .,
#                        data = nba_sel[,-1],)
# 
# # Explainer for GBM
# nba_gbm_exp <- DALEX::explain(nba_gbm,
#                        data = nba_sel[,-c(1,2)],
#                        y = nba_sel$Salary,
#                        predict_function = function(m,x)
#                          predict(m, x, n.trees = 250))
# 
# # Explainer for RF
# nba_rf_exp <- DALEX::explain(nba_rf,
#                       data = nba_sel[,-c(1,2)],
#                       y = nba_sel$Salary,
#                       predict_function = function(m,x)
#                         predict(m, x, n.trees = 250))
# 
# # Model performance of GBM
# nba_gbm_prf <- model_performance(nba_gbm_exp)
# nba_gbm_part <- model_parts(nba_gbm_exp)
# nba_gbm_md <- model_diagnostics(nba_gbm_exp)
# 
# 
# # Model performance of RF
# nba_rf_prf <- model_performance(nba_rf_exp)
# nba_rf_part <- model_parts(nba_rf_exp)
# nba_rf_md <- model_diagnostics(nba_rf_exp)
# 
# 
# variables_df <- data.frame(colnames(nba_sel))
# colnames(variables_df) <- c("Variable")
# variables_df$Description <- c('Player name', 'Salary', 'Position', 'Age', 'Team', 'Games Played', 'Minutes Played', 'Player Efficiency Rating', 'Field Goals Per 100 Team Possesions', 'Field Goal Attempts Per 100 Team Possesions', 'Field Goal Percentage', '3-Point Field Goals Per 100 Team Possesions', '3-Point Field Goal Attempts Per 100 Team Possesions', '3-Point Field Goal Percentage', '2-Point Field Goals Per 100 Team Possesions', '2-Point Field Goal Attempts Per 100 Team Possesions', '2-Point Field Goal Percentage', 'Effective Field Goal Percentage', 'Free Throws', 'Free Throws Attempts', 'Free Throw Percentage', 'Offensive Rebounds', 'Defensive Rebounds', 'Total Rebounds', 'Assists', 'Steals', 'Blocks', 'Turnovers', 'Personal Fouls', 'Points', 'Minutes Played Per Game', 'Points Per Game', 'Assists Per Game', 'Rebounds Per Game', 'Turnovers Per Game', 'Blocks Per Game', 'Steals Per Game', 'Weight', 'Height')
# variables_df$Type <- c("character", "numeric", "factor", "numeric", "factor", rep("numeric", 34))
