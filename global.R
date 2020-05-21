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
library(corrplot)

set.seed(361309)

# read data
nba <- as.data.frame(read.csv("data/nba_dataset.csv"))

# data modification & selecting variables
nba <- nba %>% plyr::rename(., c("pay" = "Payroll"))
nba$Pos <- as.factor(nba$Pos)
nba$Tm <- as.factor(nba$Tm)
nba_sel <- nba[,c(2:3, 5:41)] %>% 
  mutate_if(is.numeric, round, digits = 2)
nba_sel["X3P."] <- round(na_mean(nba_sel["X3P."]), digits = 2)
nba_sel["FT."] <- round(na_mean(nba_sel["FT."]), digits = 2)

# # Ordered NBA
# nba_ordered <- arrange(nba_sel,desc(pay))
# 
# # GBM Model
# # Changeable model parameters
# nba_gbm <- gbm(pay ~ . , data = nba_sel[,-1], n.trees = 250, interaction.depth = 3)
# 
# 
# # Explainer for GBM
# nba_gbm_exp <- explain(nba_gbm, 
#                        data = nba_sel[,-c(1,2)], 
#                        y = nba_sel$pay, 
#                        predict_function = function(m,x) 
#                          predict(m, x, n.trees = 250))
# 
# 
# # RF Model
# nba_rf <- randomForest(pay ~ .,
#                        data = nba_sel[,-1],)
# 
# # Explainer for RF
# nba_rf_exp <- explain(nba_rf, 
#                       data = nba_sel[,-c(1,2)], 
#                       y = nba_sel$pay, 
#                       predict_function = function(m,x) 
#                         predict(m, x, n.trees = 250))
