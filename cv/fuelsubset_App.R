# library(devtools)
# install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
# library(FDboost)
# library(dtw)
# library(proxy)
# library(classiFunc)
# library(gamm4)
# library(refund)
# library(parallel)

libdir = "/zpool1/s11226758/master_thesis/data_application"
library(stabs, lib.loc = libdir)
library(nlme,lib.loc = libdir)
library(lme4, lib.loc = libdir)
library(mgcv,lib.loc = libdir)
library(gamm4, lib.loc = libdir)
library(mboost, lib.loc = libdir)
library(backports, lib.loc = libdir)
library(FDboost, lib.loc = libdir)
library(proxy, lib.loc = libdir)
library(dtw,lib.loc = libdir)
library(refund, lib.loc = libdir)
library(parallel, lib.loc = libdir)
library(classiFunc, lib.loc = libdir)

# setwd("/Users/WeiliLin/Documents/Statistics/master_thesis/algorithm_code/FDboost_bfpco_github/FDboost/cv")
# source(file = "library.R")
source(file = "/zpool1/s11226758/master_thesis/data_application/CodeRes/library.R")


##################help function#################################################
doOneSet <- function(useParallel = TRUE, cores = 4, seed = 8323, 
                     data, use.method, cvparam, mparam, ptrain, nfold, splitvclass = FALSE){
  
  if(useParallel) {
    stopifnot(require(parallel))
    options(mc.cores=cores)
    this.lapply <- function(X, FUN){
      mclapply(X=X, FUN=FUN, mc.preschedule = FALSE, mc.allow.recursive = FALSE)
    }
  } else this.lapply <- lapply
  
  if(is.null(seed)){ seed <- runif(1, 0, 1e9) } 
  set.seed(seed = seed)
  train_index <- lapply(1:length(1:nfold), FUN = function(k){
    get_index(data = data, response = "y", frac = ptrain , splitvclass = splitvclass)$train_index})
  
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
  res$train_index <- train_index
  return(res)
}

doOneRep <- function(data, train_index, use.method, cvparam, mparam){
  temp <- cv.wrap.func(data = data, train_index = train_index, use.method = use.method, cvparam = cvparam, mparam = mparam)
  res <- temp$res
  cvparam.new <- temp$cvparam.new
  mod <- temp$mod
  
  return(list(res = res, cvparam.new = cvparam.new, mod = mod))
}
################################################################################

# data("fuelSubset")
# mydata <- list(y = fuelSubset$h2o, x = fuelSubset$UVVIS, s = fuelSubset$uvvis.lambda)
# 
# # set parameter 
# use.method = c("wrap.FDboost.fpco.minkowski", "wrap.FDboost.fpco.elasticMetric",
#                "wrap.FDboost.fpco.correlation", "wrap.FDboost.fpco.dtw",
#                "wrap.FDboost.fpc", "wrap.FDboost.bsignal")
# 
# cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = c(1,2,5,10), pve = c(0.95, 0.7), add = c(TRUE, FALSE), fastcmd = c(TRUE, FALSE)),
#                 wrap.FDboost.fpco.elastic = list(distType = "elasticMetric", add = c(TRUE, FALSE), pve = c(0.95, 0.7), fastcmd = c(TRUE, FALSE)),
#                 wrap.FDboost.fpco.correlation = list(distType = "correlation", pve = c(0.95, 0.7), add = c(TRUE, FALSE), fastcmd = c(TRUE, FALSE)),
#                 wrap.FDboost.fpco.dtw = list(distType = "dtw", window.type = c("sakoechiba", "itakura","none"), window.size = c(5,10,20,30)), 
#                 wrap.FDboost.fpc = list(pve = c(0.95, 0.7)),
#                 wrap.FDboost.bsignal = list(knots = c(10,20,30), differences = c(1,2))
# )
# mparam <- NULL
# 
# # Estimate model 
# res <- doOneSet(data = mydata, cores = 10, seed = 508, use.method = use.method, cvparam = cvparam, mparam = mparam, 
#                 ptrain = 0.7, nfold = 10, splitvclass = FALSE)
# 
# # save
# savedir = "/zpool1/s11226758/master_thesis/data_application/CodeRes/results"
# save(res, file = paste(savedir, "/res_fuelsubset.RData", sep = ""))

# second round -informal
################################################################################
data("fuelSubset")
myfuel <- list(y = fuelSubset$h2o, x = fuelSubset$NIR, s = fuelSubset$nir.lambda)
mydiff.data <- list(y = myfuel$y, s = myfuel$s[-1])
mydiff.data$x <- t(diff(t(myfuel$x), 1))
mydiff.data$x <- unclass(mydiff.data$x)
mydata <- mydiff.data
#plot(mydiff.data$s, colMeans(mydiff.data$x), main = "mean of differentiated NIR")

# set parameter 
use.method = c("wrap.FDboost.fpco.minkowski", "wrap.FDboost.fpco.elasticMetric",
               "wrap.FDboost.fpco.correlation", #"wrap.FDboost.fpco.dtw",
               "wrap.FDboost.fpc", "wrap.FDboost.bsignal", "wrap.gam.pfr")

cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = c(1,2),add = TRUE), 
                wrap.FDboost.fpco.elastic = list(distType = "elasticMetric", add = TRUE),
                wrap.FDboost.fpco.correlation = list(distType = "correlation", add = TRUE),
                wrap.FDboost.fpco.dtw = list(distType = "dtw", window.type = c("sakoechiba", "itakura","none"), window.size = 5), 
                wrap.FDboost.fpc = list(pve = 0.95),
                wrap.FDboost.bsignal = list(knots = 10, differences = 1),
                wrap.gam.pfr = list(k1 = 7, k2 = 7)
)
mparam <- NULL

# Estimate model 
res <- doOneSet(data = mydata, cores = 10, seed = 508, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 10, splitvclass = FALSE)

# save
savedir = "/zpool1/s11226758/master_thesis/data_application/CodeRes/results"
save(res, file = paste(savedir, "/res2inf_fuelsubset.RData", sep = ""))


