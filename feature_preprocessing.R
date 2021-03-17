
##########################################
######### Feature preprocessing
######### Reference: Wutricht M. (2016)
##########################################

if(!require(CASdatasets)){
  install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
  library(deSolve)
}

if(!require(xts)){
  install.packages("xts")
  library(xts)
}

if(!require(zoo)){
  install.packages("zoo")
  library(zoo)
}

if(!require(sp)){
  install.packages("sp")
  library(sp)
}

if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(kableExtra)){
  install.packages("kableExtra")
  library(kableExtra)
}

if(!require(moments)){
  install.packages("moments")
  library(moments)
}

if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}

if(!require(AER)){
  install.packages("AER")
  library(AER)
}

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}

if(!require(e1071)){
  install.packages("e1071")
  library(e1071)
}

if(!require(stats)){
  install.packages("stats")
  library(stats)
}

if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}

if(!require(rpart)){
  install.packages("rpart")
  library(rpart)
}

if(!require(xtable)){
  install.packages("xtable")
  library(xtable)
}

if(!require(Hmisc)){
  install.packages("Hmisc")
  library(Hmisc)
}

if(!require(rpart.plot)){
  install.packages("rpart.plot")
  library(rpart.plot)
}

if(!require(h2o)){
  install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zeno/2/R")
  library(h2o)
}

if(!require(glue)){
  install.packages("glue")
  library(glue)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(zeallot)){
  install.packages("zeallot")
  library(zeallot)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(keras)){
  install.packages("keras")
  library(keras)
}

if(!require(recipes)){
  install.packages("recipes")
  library(recipes)
}

############################
### import data ############
############################

data(freMTPL2freq)
dat <- freMTPL2freq
dat$VehGas <- factor(dat$VehGas)
dat$n <- 1
dat$ClaimNb <- pmin(dat$ClaimNb, 4) 
dat$Exposure <- pmin(dat$Exposure, 1)

## loss function ##########
Poisson.loss <- function(pred, obs){200*(mean(pred)-mean(obs)+mean(log((obs/pred)^obs)))}

############################
### dataset for glm ########
############################

dat2 <- dat
dat2$AreaGLM <- as.integer(dat2$Area)
dat2$VehPowerGLM <- as.factor(dat2$VehPower)
VehAgeGLM <- cbind(c(0:110), c(1, rep(2,10), rep(3,100)))
dat2$VehAgeGLM <- as.factor(VehAgeGLM[dat2$VehAge+1,2])
dat2[,"VehAgeGLM"] <-relevel(dat2[,"VehAgeGLM"], ref="2")
DrivAgeGLM <- cbind(c(18:100), c(rep(1,3), rep(2,5), rep(3,5), rep(4,10), rep(5,10), rep(6,20), rep(7,30)))
dat2$DrivAgeGLM <- as.factor(DrivAgeGLM[dat2$DrivAge-17,2])
dat2[,"DrivAgeGLM"] <- relevel(dat2[,"DrivAgeGLM"], ref="5")
dat2$BonusMalusGLM <- as.integer(dat2$BonusMalus)
dat2$DensityGLM <- as.numeric(log(dat2$Density))
dat2[,"Region"] <-relevel(dat2[,"Region"], ref="Centre")

dataset = dat2[, c("IDpol", "ClaimNb", "Exposure","VehPowerGLM", "VehAgeGLM", "DrivAgeGLM",  "BonusMalusGLM", 
                   "VehBrand", "VehGas",  "DensityGLM", "Region", "AreaGLM")]

colnames(dataset) = c("IDpol", "ClaimNb", "Exposure","VehPower", "VehAge", "DrivAge",  "BonusMalus", 
                      "VehBrand", "VehGas",  "Density", "Region", "Area")

set.seed(100)
ll <- sample(c(1:nrow(dataset)), round(0.9*nrow(dataset)), replace = FALSE)
learn.GLM <- dataset[ll,]
test.GLM <- dataset[-ll,]
n_l <- nrow(learn.GLM)
n_t <- nrow(test.GLM)

learn_prepped = learn.GLM %>% 
  dplyr::select(-IDpol) %>% 
  as_tibble() %>% 
  dplyr::rename(Offset = Exposure) %>% 
  mutate_at(vars(Offset), log) %>% 
  mutate_at(vars(ClaimNb), as.numeric)

test_prepped = test.GLM %>% 
  dplyr::select(-IDpol) %>% 
  as_tibble() %>% 
  dplyr::rename(Offset = Exposure) %>% 
  mutate_at(vars(Offset), log) %>% 
  mutate_at(vars(ClaimNb), as.numeric)

#############################
### dataset for gbm #########
#############################

data <- freMTPL2freq %>% 
  as_tibble() %>% 
  mutate_at(vars(ClaimNb), as.numeric)  %>% 
  mutate_at(vars(VehPower, VehGas), factor) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure),
         ClaimNb = if_else(ClaimNb > 4, 4, ClaimNb)) %>% 
  dplyr::rename(Offset = Exposure)

set.seed(100)
ll <- sample(1:nrow(data), round(0.9 * nrow(data)), replace = FALSE)
learn <- data[ll, -1] 
test <- data[-ll, -1]

rec_obj <- recipe(ClaimNb ~ ., data = learn) %>% 
  step_center(VehAge, DrivAge, BonusMalus, Density) %>% # Subtract column mean 
  step_scale(VehAge, DrivAge, BonusMalus, Density) %>%  # Divide columns by standard deviation
  step_log(Offset) 

scaled_obj <- prep(rec_obj, training = learn)

learn_gbm <- bake(scaled_obj, learn)  
test_gbm <- bake(scaled_obj, test) 

#######################################
### dataset for neural network ########
#######################################

PreProcess.Continuous <- function(var1, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  dat2$X <- as.numeric(dat2$V1)
  dat2$X <- 2*(dat2$X-min(dat2$X))/(max(dat2$X)-min(dat2$X))-1
  names(dat2)[names(dat2) == "V1"]  <- var1
  names(dat2)[names(dat2) == "X"]  <- paste(var1,"X", sep="")
  dat2
}

PreProcess.CatDummy <- function(var1, short, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  n2 <- ncol(dat2)
  dat2$X <- as.integer(dat2$V1)
  n0 <- length(unique(dat2$X))
  for (n1 in 2:n0){dat2[, paste(short, n1, sep="")] <- as.integer(dat2$X==n1)}
  names(dat2)[names(dat2) == "V1"]  <- var1
  dat2[, c(1:n2,(n2+2):ncol(dat2))]
}

Features.PreProcess <- function(dat2){
  dat2 <- PreProcess.Continuous("Area", dat2)   
  dat2 <- PreProcess.Continuous("VehPower", dat2)   
  dat2$VehAge <- pmin(dat2$VehAge,35)
  dat2 <- PreProcess.Continuous("VehAge", dat2)   
  dat2$DrivAge <- pmin(dat2$DrivAge,90)
  dat2 <- PreProcess.Continuous("DrivAge", dat2)   
  dat2$BonusMalus <- pmin(dat2$BonusMalus,150)
  dat2 <- PreProcess.Continuous("BonusMalus", dat2)   
  dat2 <- PreProcess.CatDummy("VehBrand", "Br", dat2)
  dat2$VehGasX <- as.integer(dat2$VehGas)-1.5
  dat2$Density <- round(log(dat2$Density),2)
  dat2 <- PreProcess.Continuous("Density", dat2)   
  dat2 <- PreProcess.CatDummy("Region", "R", dat2)
  dat2
}

dat_nn <- Features.PreProcess(dat) 

