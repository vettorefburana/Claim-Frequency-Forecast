
##########################################
######### Fit GLM e GBM model with H2O
##########################################

source("./feature_preprocessing.R")

###############################
### homogeneous model #########
###############################
lambda2 <- sum(learn.GLM$ClaimNb)/sum(learn.GLM$Exposure)

loss_hom = c(Poisson.Deviance(learn.GLM$Exposure*lambda2, learn.GLM$ClaimNb),
             Poisson.Deviance(test.GLM$Exposure*lambda2, test.GLM$ClaimNb))


################################
### GLM ###########
################################

## start h2o cluster for glm ###
h2o.init(nthreads = 4, port = 11223)

h2o.no_progress()                    
learn.h2o <- as.h2o(learn_prepped)   
test.h2o <- as.h2o(test_prepped)

x <- setdiff(colnames(learn.h2o), c("ClaimNb", "Offset")) 
y <- "ClaimNb"      
offset <- "Offset"  

## glm model with h2o ############

glm_fit <- h2o.glm(
  x = x, 
  y = y,                                          
  offset_column = offset,
  training_frame = learn.h2o,
  validation_frame = test.h2o,
  remove_collinear_columns = TRUE,
  family = "poisson",
  keep_cross_validation_predictions = TRUE,
  nfolds = 5, 
  seed = 1      
)

# forecast
pred_learn <- predict(glm_fit, learn.h2o)$predict
pred_cv <- h2o.cross_validation_holdout_predictions(glm_fit)$predict
pred_test <- predict(glm_fit, test.h2o)$predict

in_sample <- Poisson.loss(pred_learn, learn.h2o$ClaimNb)
cv <- Poisson.loss(pred_cv, 
                   learn.h2o$ClaimNb)
out_of_sample <- Poisson.loss(pred_test, test.h2o$ClaimNb)

## glm model with base R ###########
glm1 <- glm(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus
              + VehBrand + VehGas + Density + Region + Area, 
              data=learn_prepped, offset=Offset, family=poisson(link = log))

# forecast
learn.GLM$fit <- fitted(glm1)
test.GLM$fit <- predict(glm1, newdata=test_prepped, type="response")

################################
### GBM ###########
################################

## start h2o cluster for gbm ##########
h2o.init(nthreads = 4, port = 11223)

h2o.no_progress()                   
learn.h2o <- as.h2o(learn_gbm)  
test.h2o <- as.h2o(test_gbm)

x <- setdiff(colnames(learn.h2o), c("ClaimNb", "Offset")) 
y <- "ClaimNb"    
offset <- "Offset" 

## fit gbm ##########

boost_fit <- h2o.gbm(
  x = x, 
  y = y,                                          
  offset_column = offset,
  training_frame = learn.h2o,
  
  distribution = "poisson",
  nfolds = 5,
  keep_cross_validation_predictions = TRUE,
  seed = 1 
)

# forecast
pred_learn <- predict(boost_fit, learn.h2o)$predict
pred_cv <- h2o.cross_validation_holdout_predictions(boost_fit)
pred_test <- predict(boost_fit, test.h2o)$predict

in_sample <- Poisson.loss(pred_learn, learn.h2o$ClaimNb)
cv <- Poisson.loss(pred_cv,
                   learn.h2o$ClaimNb)
out_of_sample <- Poisson.loss(pred_test, test.h2o$ClaimNb)


## random grid search for gbm ########

# Search parameter
strategy <- list(strategy = "RandomDiscrete",
                 max_models = 10,
                 max_runtime_secs = 300,
                 seed = 1)

# Define grid
gbm_params <- list(learn_rate = seq(0.001, 0.3, 0.001),
                   max_depth = seq(2, 10),
                   sample_rate = seq(0.5, 1, 0.1),
                   col_sample_rate = seq(0.1, 1.0, 0.1))

# Launch grid search
gbm_grid <- h2o.grid(
  "gbm",
  x = x,
  y = y,
  offset_column = offset,
  distribution = "poisson",
  
  training_frame = learn.h2o,
  nfolds = 5,
  hyper_params = gbm_params,
  search_criteria = strategy,
  
  seed = 1,
  ntrees = 10000,
  stopping_rounds = 5,           # Early stopping
  stopping_tolerance = 0.001,
  keep_cross_validation_predictions = TRUE,
  stopping_metric = "deviance"
)

# best model
best_gbm <- h2o.getModel(gbm_grid@model_ids[[1]]) 

# forecast
pred_learn <- predict(best_gbm, learn.h2o)$predict
pred_cv <- h2o.cross_validation_holdout_predictions(best_gbm)
pred_test <- predict(best_gbm, test.h2o)$predict

in_sample <- Poisson.loss(pred_learn, learn.h2o$ClaimNb)
cv <- Poisson.loss(pred_cv,
                   learn.h2o$ClaimNb)
out_of_sample <- Poisson.loss(pred_test, test.h2o$ClaimNb)



