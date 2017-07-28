library(foreign)
library(devtools)
source(file = "cvsource.R")

install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
library(FDboost)


# Import one of the following three datasets for modelling


# dataset MedicalImages
# Dataset is available at http://www.timeseriesclassification.com/description.php?Dataset=MedicalImages
# MedicalImages dataset for multinomial classification. The dataset records the densitly of brightness
# of a medical image, which presents one of 10 body organs. The dataset has scalar target as the name of 
# body organ presented on a medical image, named as class1 to class 10; the dataset has a functional 
# predictor, which records the brightness density of the mdedical image. Totally there are 1141 samples for modelling.
#
# medgf1 <- read.arff("MedicalImages_TRAIN.arff") 
# medgf2 <- read.arff("MedicalImages_TEST.arff")
# medgf3 <- rbind(medgf1, medgf2)
# medgf4 <- list(response = medgf3$target, func_x = as.matrix(medgf3[,-100]), x_index = as.numeric(1:99))
# medgf4$rspdummy <- factor(levels(medgf4$response)[levels(medgf4$response) != 10])
# mydata <- medgf4
# myresponse = "Multinomial"


# dataset earthquake
# data available at http://www.timeseriesclassification.com/description.php?Dataset=Earthquakes
# The earthquake classification problem involves predicting whether a major event is about to 
# occur based on the most recent reading data in the surrounding area. The data is taken from 
# Northern California Earthquake Data Center and each data is an averaged reading for one hour.
# A major event is defined as any reading of over 5 on the Rictor scale. To better distinguish aftershock
# and a major earthquake, a positive case is considered to be one where a major event is not 
# preceded by another major event for at least 512 hours. A negative case refers to instances 
# where there is a reading below 4 that is preceded by at least 20 readings in the previous 512 hours
# that are non-zero (to avoid trivial negative cases). 
# Of the 86,066 hourly readings, 368 negative cases and 93 positive are available.
#
# earthquakes1 <- read.arff("Earthquakes_TRAIN.arff")
# earthquakes2 <- read.arff("Earthquakes_TEST.arff")
# earthquakes3 <- rbind(earthquakes1, earthquakes2)
# earthquakes4 <- list(response = earthquakes3$target, func_x = as.matrix(earthquakes3[,which(colnames(earthquakes3)!="target")]), x_index = 1:512)
# mydata <- earthquakes4
# myresponse = "Numeric" 
# 
#
# Dataset fuelSubset
# Dataset fuelSubset consists of 129 laboratory samples of spectral Data of Fossil Fuels, and is provided in package FDboost.
# We use h2o variable as scalar target, which records the humidity. For functional predictor, we use near infrared spectrum (NIR).  
# measured at 231 wavelengths.
# 
# data("fuelSubset", package = "FDboost")
# fuel1 <- list(response = fuelSubset$h2o, func_x = fuelSubset$NIR, x_index= fuelSubset$nir.lambda)
# mydata <- fuel1
# myresponse = "Numeric" 


# set paramters according to the type of target(you can change the value of parameters here!)
if(myresponse == "Multinomial"){
  set_splitvclass = TRUE # if TRUE, CVdata splits each class of response proportionally 
  set_nosplitvars = c("x_index", "rspdummy") # the name of no-to-split variables for CVdata function
  set_mstop_grid = c(100,1000,5000,10000,15000) # search grids of mstop parameter
  set_ACC = TRUE;set_MSE = FALSE # if set_ACC = TRUE, compute accuracy, if set_MSE = TRUE, compute MSE
  set_family = Multinomial();# distribution family of response, called by FDboost function 
  set_timeformula = formula(~ bols(1)) # timeformula for scalar response, called by FDboost 
  set_rspformula = "%O%bols(rspdummy, df = 4, contrasts.arg = 'contr.dummy')" # special formular for multinoimal response, called by FDboost
}else{
  set_splitvclass = FALSE; set_nosplitvars = c("x_index")
  set_mstop_grid = c(1000,5000,10000,15000);set_ACC = FALSE;set_MSE = TRUE
  set_family = Gaussian();set_timeformula = formula(~ bols(1))
  set_rspformula = ""
}

# set hyper-paramters for cross validation (you can change the value of parameters here!)
set_disttype = c("Minkowski","DTW") # set the distance types for cross validation
set_window.type = c("none", "itakura", "sakoechiba") # set the dtw window types for cross validation
set_window.size =  seq(30, 55, by = 5) # set the dtw window sizes for cross validation
set_p = c(1,2,20) # set the Minkowski orders for cross validation
set_penalty = c("identity", "inverse", "no") # set the penalty types for cross validation
set_df = 4 # set the dfs for cross validation


######Start from here, you can change the value of parameters if required ######
# generate cross validation dataset
set.seed(8384)
cvdata <- CVdata(mydata, nfold = 10, frac = 0.8, splitvclass = set_splitvclass, 
                 response = "response", nosplitvars =  set_nosplitvars)


# set cv parameters for bfpco based FDboost
## save fixed hyperparameters of CVmodel in a list
cvpartargs <- list(funname = "FDboost",cvdata = cvdata, smstop = TRUE, 
                   mstop_grid = set_mstop_grid, B = 3, 
                   response = "response", ACC = set_ACC, MSE = set_MSE) 

## save to be selected FDboost/bfpco hyperparameters in a big dataframe
fmdf <- expand.grid(baselearner = "bfpco", distType = set_disttype,
                    window.type = set_window.type, window.size = set_window.size, 
                    p = set_p, penalty = set_penalty, df = set_df)  
fmdf[fmdf$distType == "Minkowski", c("window.type","window.size")] = NA
fmdf[fmdf$distType == "DTW", "p"] = NA 
fmdf <- unique(fmdf)
fmdf <- fmdf[order(fmdf$baselearner, fmdf$distType, fmdf$window.type, 
                   fmdf$window.size, fmdf$p, fmdf$penalty,fmdf$df), ]

# set cv parameters for bfpc, bols, bsignal based FDboost
temp1 <- expand.grid(baselearner = c("bfpc","bols"),distType = NA,
                     window.type = NA, window.size = NA, p = NA,
                     penalty = c("identity", "inverse", "no"), df = 4)
temp1[temp1$baselearner == "bols", "penalty"] <- NA
temp1 <- unique(temp1[order(temp1$baselearner, temp1$penalty, temp1$df), ])

temp2 <- expand.grid(baselearner = c("bsignal"),distType = NA,
                     window.type = NA, window.size = NA, 
                     p = NA,penalty = c("ps", "pss"), df = 4)
temp2 <- temp2[order(temp2$baselearner, temp2$penalty, temp2$df), ]

# combine cv parameters for all models
fmdf <- rbind(fmdf, temp1, temp2)
rownames(fmdf) <- 1:nrow(fmdf)


#####Start from here, you should only make changes if indeedly necessary!#######
## a big list of FDboost/mboost args
FDboostargs <- list() 
## set formula for Minkowski distance type 
for( i in (index1 = which(fmdf$baselearner == "bfpco" & fmdf$distType == "Minkowski"))){
  fm <- as.formula(paste("response ~ bfpco(x = func_x, s = x_index, pve = 0.95, 
                         df = ", fmdf[i,]$df, 
                         ", penalty = '", fmdf[i,]$penalty, 
                         "', distType = '", fmdf[i,]$distType,
                         "', p = ", fmdf[i,]$p, ")", set_rspformula,
                         sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = set_timeformula, family = set_family)
}   

## set formula for dtw distance type
for( i in (index2 = which(fmdf$baselearner == "bfpco" & fmdf$distType == "DTW"))){
  fm <- as.formula(paste("response ~ bfpco(x = func_x, s = x_index, pve = 0.95, 
                         df = ", fmdf[i,]$df, 
                         ", penalty = '", fmdf[i,]$penalty, 
                         "', distType = '", fmdf[i,]$distType,
                         "', window.type = '", fmdf[i,]$window.type,
                         "', window.size = ", fmdf[i,]$window.size,
                         ")", set_rspformula, sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = set_timeformula, family = set_family)
} 

## set formula of bfpc based FDboost
for( i in (index3 = which(fmdf$baselearner == "bfpc"))){
  fm <- as.formula(paste("response ~ bfpc(x = func_x, s = x_index, pve = 0.95, 
                         df = ", fmdf[i,]$df, 
                         ", penalty = '", fmdf[i,]$penalty, "')", set_rspformula,
                         sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = set_timeformula, family = set_family)
}               

## set formula of bols based FDboost
xname = paste("att", 1:ncol(mydata$func_x), sep = "")
for(i in (index4 = which(fmdf$baselearner == "bols"))){
  fm <- as.formula(paste("response ~ ", 
                         paste("bols(", xname, ", lambda = 0)",set_rspformula,
                               collapse = " + ", sep = "")))
  FDboostargs[[i]] <- list(formula = fm, timeformula = NA, family = set_family)
}

## formula of bsignal based FDboost  
for( i in (index5 = which(fmdf$baselearner == "bsignal"))){
  fm <- as.formula(paste("response ~ bsignal(x = func_x, s = x_index, knots = 10,
                         penalty = '", fmdf[i,]$penalty, "', center = TRUE)", set_rspformula, 
                         sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = set_timeformula, family = set_family)
}

## set name of FDboostargs
names(FDboostargs) <- paste(fmdf$baselearner, 1:length(fmdf$baselearner), sep = "_")


# cv of bfpco based FDboost
## minkowski model
cvmink<- lapply(FDboostargs[index1], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvmink) = paste("mod", index1, sep = "")

## dtw model
cvdtw <- lapply(FDboostargs[index2], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvdtw) = paste("mod", index2, sep = "")

## bfpc models
cvbfpc <- lapply(FDboostargs[index3], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvbfpc) = paste("mod", index3, sep = "")

## bols models
cvdata_bols <- lapply(cvdata, FUN = function(x){
  lapply(x, FUN = function(y){ colnames(y$func_x) = xname;
    y = c(y[names(y) != "func_x"], as.list(data.frame(y$func_x)))})})
cvpartargs_bols <- cvpartargs;cvpartargs_bols$cvdata <- cvdata_bols;cvpartargs_bols$funname = "mboost"
cvbols <- lapply(FDboostargs[index4], FUN = function(x){
  do.call(CVmodel, args = c(cvpartargs_bols, mdlargs = list(x)))
})
names(cvbols) = names(cvbfpc) = paste("mod", index5, sep = "")

## bsignal models
cvbsignal <- lapply(FDboostargs[index5], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvbfpc) = paste("mod", index5, sep = "")