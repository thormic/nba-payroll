library(gbm)
library(readr)
library(DALEX)
library(ingredients)
library(iBreakDown)
library(ggplot2)
library(scales)
library(randomForest)
library(imputeTS)

# read data
nba <- as.data.frame(read.csv("nba_dataset.csv"))

# data modification & selecting variables
nba$Pos <- as.factor(nba$Pos)
nba$Tm <- as.factor(nba$Tm)
nba_sel <- nba[,c(2:3, 5:41)]
nba_sel["X3P."] <- na_mean(nba_sel["X3P."])
nba_sel["FT."] <- na_mean(nba_sel["FT."])

# building gbm model to predict payroll
# Type your own parameters for GBM/distribution
nba_gbm <- gbm(pay ~ . , 
               data = nba_sel[,-1], 
               n.trees = 250, 
               interaction.depth = 3)

# R2 of gbm 0.95
model_performance(nba_gbm_exp)

# running explainer for gbm
# account for varaibles like n.trees
nba_gbm_exp <- explain(nba_gbm, 
                       data = nba_sel[,-c(1,2)], 
                       y = nba_sel$pay, 
                       predict_function = function(m,x) 
                         predict(m, x, n.trees = 250))


# building RandomForest model
nba_rf <- randomForest(pay ~ .,
                       data = nba_sel[,-1],)

# explainer for rf
nba_rf_exp <- explain(nba_rf, 
                       data = nba_sel[,-c(1,2)], 
                       y = nba_sel$pay, 
                       predict_function = function(m,x) 
                         predict(m, x, n.trees = 250))

# compare models
vip_gbm <- variable_importance(nba_gbm_exp, 
                              loss_function = loss_root_mean_square)

vip_rf <- variable_importance(nba_rf_exp, 
                               loss_function = loss_root_mean_square)

plot(vip_rf, vip_gbm, max_vars = 10) +
  ggtitle("Permutation variable importance", "")

# compare models
vip_gbm <- feature_importance(nba_gbm_exp)

vip_rf <- feature_importance(nba_rf_exp)

plot(vip_rf, vip_gbm, max_vars = 10) +
  ggtitle("Permutation variable importance", "")

# plotting the most important variables
# Change number of variables
nba_feat <- ingredients::feature_importance(nba_gbm_exp)
plot(nba_feat, max_vars = 12)


# change of average payroll prediction with Age
# Different variables to choose from
nba_pd <- ingredients::partial_dependency(nba_gbm_exp, variables = "Age")
plot(nba_pd)


pdp_rf <- model_profile(nba_gbm_exp, variables ="PPG", groups = "Tm")
plot(pdp_rf, geom = "profiles") +
  ggtitle("Ceteris Paribus and Partial dependence profiles for age") 

# simulating case for this player observation with one variable changing
# Different variables to choose from
new_observation <- nba_sel[358,]
nba_cp_pg <- ingredients::ceteris_paribus(nba_gbm_exp, new_observation = new_observation, variables = "Age", variable_splits = list(Age = seq(18,45,0.1)))
plot(nba_cp_pg) + geom_vline(xintercept = 22, linetype = "dotted", color = "blue")

# Variables and how they affect certain players payroll
# Box with choosable player
nba_pg_gbm <- break_down(nba_gbm_exp, new_observation = new_observation)
nba_pg_gbm$label = "Break Down for ***PLAYER***"
plot(nba_pg_gbm, digits = 0, max_features = 10) +  
  scale_y_continuous(labels = dollar_format(suffix = "$", prefix = ""), name = "Payroll", limits = 400000*c(1,100), breaks = 1000000*seq(0,45,8))

# Variables contribution to payroll for given player
nba_pg_gbm <- shap(nba_gbm_exp, new_observation = new_observation)
plot(nba_pg_gbm, max_features = 10)


