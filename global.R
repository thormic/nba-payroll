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
library(dashboardthemes)
library(fields)

set.seed(361309)

load("nba_workspace.RData")

# # read data
# nba <- as.data.frame(read.csv("data/nba_dataset.csv"))
# 
# # data modification & selecting variables
# nba <- nba %>% plyr::rename(., c("pay" = "Payroll"))
# nba$Pos <- as.factor(nba$Pos)
# nba$Tm <- as.factor(nba$Tm)
# nba_sel <- nba[,c(2:3, 5:41)] %>% 
#   mutate_if(is.numeric, round, digits = 2)
# nba_sel["X3P."] <- round(na_mean(nba_sel["X3P."]), digits = 2)
# nba_sel["FT."] <- round(na_mean(nba_sel["FT."]), digits = 2)
# 
# 
# # GBM Model
# # Changeable model parameters
# nba_gbm <- gbm(Payroll ~ . , data = nba_sel[,-1], n.trees = 250, interaction.depth = 3)
# 
# # RF Model
# nba_rf <- randomForest(Payroll ~ .,
#                        data = nba_sel[,-1],)
# 
# # Explainer for GBM
# nba_gbm_exp <- DALEX::explain(nba_gbm,
#                        data = nba_sel[,-c(1,2)],
#                        y = nba_sel$Payroll,
#                        predict_function = function(m,x)
#                          predict(m, x, n.trees = 250))
# 
# # Explainer for RF
# nba_rf_exp <- DALEX::explain(nba_rf,
#                       data = nba_sel[,-c(1,2)],
#                       y = nba_sel$Payroll,
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
