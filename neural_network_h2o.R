#############################################
######### Fit neural network model with H2O
#############################################

source("./feature_preprocessing.R")

#############################
## import features ##########
#############################

learn = learn_gbm %>% 
  dplyr::select(-Area)

test = test_gbm %>% 
  dplyr::select(-Area) 

h2o.no_progress()                   
learn.h2o <- as.h2o(learn)  
test.h2o <- as.h2o(test)

####################################
## shallow neural network ##########
####################################

shallow_model <- h2o.deeplearning(
  
  x = setdiff(colnames(learn.h2o), c("ClaimNb", "Offset")),
  y = "ClaimNb",
  offset_column = "Offset",
  distribution = "poisson",
  
  training_frame = learn.h2o,
  nfolds = 5,
  seed = 1,
 
  hidden = c(20),
  input_dropout_ratio = 0.2,
  epochs = 100,
  variable_importances = T,
  keep_cross_validation_predictions = TRUE,
  activation = "Tanh"
  
)

# forecast error
pred_learn <- predict(shallow_model, learn.h2o)$predict
pred_cv <- h2o.cross_validation_holdout_predictions(shallow_model)
pred_test <- predict(shallow_model, test.h2o)$predict
in_sample <- Poisson.loss(pred_learn, learn.h2o$ClaimNb)
cv <- Poisson.loss(pred_cv,learn.h2o$ClaimNb)
out_of_sample <- Poisson.loss(pred_test, test.h2o$ClaimNb)

################################
### random grid search #########
################################

search_criteria = list(strategy = "RandomDiscrete",
                       max_models = 10, max_runtime_secs = 1000,
                       seed=123456)

hidden_opt = c(10, 20, 30)
rate_opt = c(0.001, 0.005, 0.01)
l2_opt <- c(0,1e-3,1e-5)
hyper_params <- list(hidden = hidden_opt, l2 = l2_opt, rate = rate_opt)

grid_shallow <-  h2o.grid("deeplearning",

         x = setdiff(colnames(learn.h2o), c("ClaimNb", "Offset")),
         y = "ClaimNb",
         offset_column = "Offset",
         distribution = "poisson",
         
         training_frame = learn.h2o,
         nfolds = 5,
         seed = 1,
         hyper_params = hyper_params,
         search_criteria = search_criteria,
         
         input_dropout_ratio = 0.2,
         activation = "Tanh", 
         
)

best_shallow <- h2o.getModel(grid_shallow@model_ids[[1]]) 

# forecast errors
pred_learn <- predict(best_shallow, learn.h2o)$predict
pred_test <- predict(best_shallow, test.h2o)$predict
in_sample <- Poisson.loss(pred_learn, learn.h2o$ClaimNb)
out_of_sample <- Poisson.loss(pred_test, test.h2o$ClaimNb)



