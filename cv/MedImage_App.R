library(devtools)
install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
library(FDboost)
library(dtw)
library(proxy)
library(classiFunc)
library(gamm4)
library(refund)
library(foreign)

setwd("/Users/WeiliLin/Documents/Statistics/master_thesis/algorithm_code/FDboost_bfpco_github/FDboost/cv")
source(file = "library.R")

doOneSet <- function(useParallel = TRUE, cores = 4, seed = 8323, 
                     data, use.method, cvparam, mparam, ptrain, nfold){
  
  if(useParallel) {
    stopifnot(require(parallel))
    options(mc.cores=cores)
    this.lapply <- function(X, FUN){
      mclapply(X=X, FUN=FUN, mc.preschedule = FALSE, mc.allow.recursive = FALSE)
    }
  } else this.lapply <- lapply
  
  set.seed(seed = seed)
  train_index <- lapply(1:length(1:nfold), FUN = function(k){
    get_index(data = data, response = "y", frac = ptrain , splitvclass = TRUE)$train_index})
  
  res <- this.lapply(1:length(use.method), function(i){
    
    time.start = proc.time()
    
    temp <- doOneRep(data = data, train_index = train_index, use.method = use.method[i], 
                     cvparam = cvparam[i], mparam = mparam[i])
    
    time.stop = proc.time()
    
    #     res <- cbind(tmp, name=name, rep=i, seed=seed.i, ntrain=ntrain, 
    #                  benchmark.oos = mean((target[-train] - mean(target[train]))^2),
    #                  benchmark.is = mean((target[-train] - mean(target[-train]))^2),
    #                  time = (time.stop - time.start)[3])
    #     cat("\n", name,": replication ", i, " done.\n")
    return(temp)
  })
  names(res) <- use.method
  return(res)
}

doOneRep <- function(data, train_index, use.method, cvparam, mparam){
  temp <- cv.wrap.func(data = data, train_index = train_index, use.method = use.method, cvparam = cvparam, mparam = mparam)
  res <- temp$res
  cvparam.new <- temp$cvparam.new
  
  return(list(res = res, cvparam.new = cvparam.new))
}


# Prepare data #################################################################
setwd("/Users/WeiliLin/Documents/Statistics/master_thesis/data_application/MedicalImages")
medgf <- read.arff("MedicalImages_TRAIN.arff")

## data preprocessing
range(rowSums(medgf[,-100])) #data is rowwise centered
range(apply(medgf[,-100], MARGIN = 1, FUN = sd)) # data is rowwise standardized
prop.table(table(medgf$target)) #over 50% of observations are of class 10

###  plot data 
plotdata <- medgf
plotdata$target = as.numeric(levels(medgf$target))[medgf$target]

par(mfrow = c(4,1))
matplot(x = 1:99, y = t(as.matrix(plotdata[plotdata$target < 4,-100])), 
        type = "l", col= plotdata[plotdata$target < 4,100], 
        ylab = "intensity of class 1,2,3", xlab = "pixel")

matplot(x = 1:99, y = t(as.matrix(plotdata[plotdata$target > 3 & plotdata$target < 7 ,-100])), 
        type = "l", col= plotdata[plotdata$target > 3 & plotdata$target < 7, 100],
        ylab = "intensity of calss 4,5,6", xlab = "pixel")

matplot(x = 1:99, y = t(as.matrix(plotdata[plotdata$target > 6 & plotdata$target < 10 ,-100])), 
        type = "l", col = plotdata[plotdata$target > 6 & plotdata$target < 10, 100]-2, 
        ylab = "intensity of class 7,8,9", xlab = "pixel")

matplot(x = 1:99, y = t(as.matrix(plotdata[plotdata$target > 9,-100])), 
        type = "l", col = plotdata[plotdata$target > 9,100]-2, 
        ylab = "intensity of class 10", xlab = "pixel")

par(mfrow = c(3,3))
for( i in 1:9){
  yval <- t(as.matrix(plotdata[plotdata$target == i | plotdata$target == 10, -100]))
  yclass <- plotdata[plotdata$target == i | plotdata$target == 10, 100]
  matplot(x = 1:99, y = yval, 
          type = "l", col= yclass, 
          ylab = "", xlab = "pixel",
          main = paste("intensity of class", i, sep = " "))
  legend("topright", legend = c("class10", paste("class", i, sep = "")), col = yclass)
}

### group mean plot
gmean <- aggregate(plotdata[, -100], list(plotdata$target), mean)

par(mfrow = c(2,1))
matplot(x = 1:99, y = t(as.matrix(gmean[,-1])), col = gmean[,1], type = "o",pch = gmean[,1],
        cex = 0.8, xlab = "grid", ylab = "", main = "Intensity averaged over class")
legend( "topright", legend = paste("class", gmean[,1], sep = ""), col = gmean[,1],
        lty = 1, cex = 0.6, pt.cex = 0.5, ncol = 2, pch = gmean[,1])  # move the legend to exterior space
### curve of class 9 and class 10 is very similar

### random guess accuracy 
### accuracy = p1^2 + p2^2 + .....p10^2
racy <- sum(prop.table(table(medgf$target))^2)  # racy = 0.318

## reconstruct data
mymedgf <- list(y = medgf$target, x = as.matrix(medgf[,-100]), s = as.numeric(1:99))
mymedgf$rspdummy <- factor(levels(mymedgf$y)[levels(mymedgf$y) != 10])


# set parameter ##############################################################
use.method = c("wrap.FDboost.fpco.minkowski", "wrap.FDboost.fpco.elasticMetric",
               "wrap.FDboost.fpco.correlation", "wrap.FDboost.fpco.dtw",
               "wrap.FDboost.fpc", "wrap.FDboost.bsignal",
               "wrap.gam.fpco")

cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = 1),
                wrap.FDboost.fpco.elastic = list(distType = "elasticMetric"),
                wrap.FDboost.fpco.correlation = list(distType = "correlation"),
                wrap.FDboost.fpco.dtw = list(distType = "dtw"),
                wrap.FDboost.fpc = list(npc = 5),
                wrap.FDboost.bsignal = list(penalty = "ps"),
                wrap.gam.fpco = list(distType = "dtw")
                )

mparam <- list(wrap.FDboost.fpco.minkowski = list(rspformula = "bols(rspdummy, df = 2, contrasts.arg = 'contr.dummy')", family = Multinomial()),
               wrap.FDboost.fpco.elastic = list(rspformula = "bols(rspdummy, df = 2, contrasts.arg = 'contr.dummy')", family = Multinomial()),
               wrap.FDboost.fpco.correlation = list(rspformula = "bols(rspdummy, df = 2, contrasts.arg = 'contr.dummy')", family = Multinomial()),
               wrap.FDboost.fpco.dtw = list(rspformula = "bols(rspdummy, df = 2, contrasts.arg = 'contr.dummy')", family = Multinomial()),
               wrap.FDboost.fpc = list(rspformula = "bols(rspdummy, df = 2, contrasts.arg = 'contr.dummy')", family = Multinomial()),
               wrap.FDboost.bsignal = list(rspformula = "bols(rspdummy, df = 2, contrasts.arg = 'contr.dummy')", family = Multinomial()),
               wrap.gam.fpco = list(family = multinom(K = 9))
               )

# Estimate model 
res <- doOneSet(data = mymedgf, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 2)


