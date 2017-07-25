library(foreign)
library(FDboost)
load("cvsource.R")

# read data
medgf1 <- read.arff("MedicalImages_TRAIN.arff")
medgf2 <- read.arff("MedicalImages_TEST.arff")
medgf3 <- rbind(medgf1, medgf2)
set.seed(338)
medgf <- medgf3[sample(nrow(medgf3), round(nrow(medgf3)*0.7), replace = FALSE), ]

# preprocess data
mymedgf <- list(response = medgf$target,intensity = as.matrix(medgf[,-100]), pixel = as.numeric(1:99))
mymedgf$rspdummy <- factor(levels(mymedgf$response)[levels(mymedgf$response) != 10])

# generate cross validation dataset
set.seed(8384)
cvdata <- CVdata(mymedgf, nfold = 10, frac = 0.8, splitvclass = TRUE, 
                 response = "response", nosplitvars = c("pixel", "rspdummy"))

# set cv parameters for bfpco based FDboost
cvpartargs <- list(funname = "FDboost",cvdata = cvdata, smstop = TRUE, 
                   mstop_grid = c(100,1000,5000,10000,15000),B = 3, 
                   response = "response", ACC = TRUE, pred_type = "class") # save fixed hyperparameter of CVmodel in a list

fmdf <- expand.grid(baselearner = "bfpco",
                    distType = c("Minkowski", "DTW"),
                    window.type = c("none", "itakura", "sakoechiba"), 
                    window.size = seq(30, 55, by = 5), 
                    p = c(1,2,20),
                    penalty = c("identity", "inverse", "no"), 
                    df = 4)  ## save to be selected FDboost/bfpco hyperparameters in a big dataframe
fmdf[fmdf$distType == "Minkowski", c("window.type","window.size")] = NA
fmdf[fmdf$distType == "DTW", "p"] = NA 
fmdf <- unique(fmdf)
fmdf <- fmdf[order(fmdf$baselearner, fmdf$distType, fmdf$window.type, fmdf$window.size, fmdf$p, fmdf$penalty,fmdf$df), ]
rownames(fmdf)  <- 1:nrow(fmdf)

FDboostargs <- list() ## a big list of FDboost/mboost args
## set formula for Minkowski distance type 
for( i in 1:9){
  fm <- as.formula(paste("response ~ bfpco(x = intensity, s = pixel, pve = 0.95, 
                         df = ", fmdf[i,]$df, 
                         ", penalty = '", fmdf[i,]$penalty, 
                         "', distType = '", fmdf[i,]$distType,
                         "', p = ", fmdf[i,]$p, 
                         ")%O%bols(rspdummy, df = 4, contrasts.arg = 'contr.dummy')", 
                         sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = formula(~ bols(1)), family = Multinomial())
}               
## set formula for dtw distance type
for( i in 10:nrow(fmdf)){
  fm <- as.formula(paste("response ~ bfpco(x = intensity, s = pixel, pve = 0.95, 
                         df = ", fmdf[i,]$df, 
                         ", penalty = ' ", fmdf[i,]$penalty, 
                         "', distType = '", fmdf[i,]$distType,
                         "', window.type = '", fmdf[i,]$window.type,
                         "', window.size = ", fmdf[i,]$window.size,
                         ")%O%bols(rspdummy, df = 4, contrasts.arg = 'contr.dummy')", 
                         sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = formula(~ bols(1)), family = Multinomial())
} 

names(FDboostargs) <- paste("bfpco_", 1:length(FDboostargs))

# cv of bfpco based FDboost
## minkowski model
cvmink<- lapply(FDboostargs[4], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvmink) = paste("mod", 1:9, sep = "")

## dtw model
cvdtw <- cvdtw <- lapply(FDboostargs[10:63], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvdtw) = paste("mod", 10:63, sep = "")


# cv of bfpc, bols, bsignal based FDboost
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

fmdf <- rbind(fmdf, temp1, temp2)
rownames(fmdf) <- 1:nrow(fmdf)

## set formula of bfpc based FDboost
for( i in 64:66){
  fm <- as.formula(paste("response ~ bfpc(x = intensity, s = pixel, pve = 0.95, 
                         df = ", fmdf[i,]$df, 
                         ", penalty = '", fmdf[i,]$penalty, 
                         "')%O%bols(rspdummy, df = 4, contrasts.arg = 'contr.dummy')", 
                         sep = ""))
  FDboostargs[[i]] <- list(formula = fm, timeformula = formula(~ bols(1)), family = Multinomial())
  names(FDboostargs)[[i]] <- paste("bfpc_", i)
}               

## set formula of bols based FDboost
for(i in 67){
  fm <- as.formula(paste("response ~ ", 
                         paste("bols(", xname, ", lambda = 0) %O% 
                               bols(rspdummy,contrasts.arg = 'contr.dummy', lambda = 0)",
                               collapse = " + ", sep = "")))
  FDboostargs[[i]] <- list(formula = fm, timeformula = NA, family = Multinomial())
  names(FDboostargs)[[i]] <- paste("bols_", i)
}

## formula of bsignal based FDboost  
for( i in 68:69){
  fm <- as.formula(paste("response ~ bsignal(x = intensity, s = pixel, knots = 10,
                         penalty = '", fmdf[i,]$penalty, "', center = TRUE) %O% 
                         bols(rspdummy, df = 4,  contrasts.arg = 'contr.dummy')"))
  FDboostargs[[i]] <- list(formula = fm, timeformula = formula(~ bols(1)), family = Multinomial())
  names(FDboostargs)[[i]] <- paste("bsignal_", i)
}

# cv models
## bfpc models
cvbfpc <- lapply(FDboostargs[64:66], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvbfpc) = paste("mod", 64:66, sep = "")

## bols models
xname = names(medgf[,-100])
cvdata_bols <- lapply(cvdata, FUN = function(x){
  lapply(x, FUN = function(y){ colnames(y$intensity) = xname;
      y = c(y[names(y) != "intensity"], as.list(data.frame(y$intensity)))})})
cvpartargs_bols <- cvpartargs;cvpartargs_bols$cvdata <- cvdata_bols;cvpartargs_bols$funname = "mboost"
cvbols <- lapply(FDboostargs[67], FUN = function(x){
  do.call(CVmodel, args = c(cvpartargs_bols, mdlargs = list(x)))
})

## bsignal models
cvbsignal <- lapply(FDboostargs[68:69], FUN = function(x) {
  do.call(CVmodel, args = c(cvpartargs, mdlargs = list(x)))})
names(cvbfpc) = paste("mod", 68:69, sep = "")


















