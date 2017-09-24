# Load library
library(devtools)
install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
library(FDboost)
library(dtw)
library(proxy)
library(classiFunc)
library(gamm4)
library(refund)

rm(list = ls())

# Load library on server
# libdir = ""
# library(stabs, lib.loc = libdir)
# library(MASS, lib.loc = libdir)
# library(lattice, lib.loc = libdir)
# library(Matrix, lib.loc = libdir)
# library(nlme,lib.loc = libdir)
# library(boot,lib.loc = libdir)
# library(lme4, lib.loc = libdir)
# library(mgcv,lib.loc = libdir)
# library(gamm4, lib.loc = libdir)
# library(mboost, lib.loc = libdir)
# library(backports, lib.loc = libdir)
# library(FDboost, lib.loc = libdir)
# library(proxy, lib.loc = libdir)
# library(dtw,lib.loc = libdir)
# library(refund, lib.loc = libdir)
# require(parallel)
# library(classiFunc, lib.loc = libdir)
# library(foreign, lib.loc = libdir)
# library(plyr, lib.loc = libdir)

options(expressions = 10000)

# Load functions
source(file = "library.R")

# help function
doOneSet <- function(useParallel = TRUE, cores = 4, seed = 8323, 
                     data, use.method, cvparam, mparam, ptrain, nfold, splitvclass = splitvclass){
  
  if(useParallel) {
    stopifnot(require(parallel))
    options(mc.cores=cores)
    this.lapply <- function(X, FUN){
      mclapply(X=X, FUN=FUN, mc.preschedule = FALSE, mc.allow.recursive = FALSE)
    }
  } else this.lapply <- lapply
  
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

# Prepare data
data("DTI2")
DIT2s <- DTI2[,c("cca", "pasat")]
DTI2c <- DIT2s[complete.cases(DIT2s),]
s = as.numeric(1:93)
mydata = list(x = DTI2c$cca, y = DTI2c$pasat, s = 1:93)

# set parameter 
use.method = c("wrap.FDboost.fpco.minkowski", "wrap.FDboost.fpco.elasticMetric",
               "wrap.FDboost.fpco.correlation", "wrap.FDboost.fpco.dtw",
               "wrap.FDboost.fpco.dtw","wrap.FDboost.fpco.dtw",
               "wrap.FDboost.fpc", "wrap.FDboost.bsignal", "wrap.gam.pfr","wrap.gam.fpco")

cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = c(1,2,3,4,5), pve = c(0.95, 0.85), add = c(TRUE, FALSE)),
                wrap.FDboost.fpco.elastic = list(distType = "elasticMetric", add = c(TRUE, FALSE), pve = c(0.95, 0.85)),
                wrap.FDboost.fpco.correlation = list(distType = "correlation", pve = c(0.95, 0.85), add = c(TRUE, FALSE)),
                wrap.FDboost.fpco.dtw = list(distType = "dtw", window.type = c("sakoechiba"), window.size = c(4,8,10), pve = c(0.85,0.95), add = c(TRUE, FALSE)),
                wrap.FDboost.fpco.dtw = list(distType = "dtw", window.type = c("itakura"), window.size = c(4,8,10), pve = c(0.85,0.95), add = c(TRUE, FALSE)), 
                wrap.FDboost.fpco.dtw = list(distType = "dtw", window.type = "none", pve = c(0.85,0.95), add = c(TRUE, FALSE)),
                wrap.FDboost.fpc = list(pve = c(0.95, 0.85)),
                wrap.FDboost.bsignal = list(knots = c(10,20,30), differences = c(1,2)),
                wrap.gam.pfr = list(k1 = c(5,10), k2 = c(5,10)),
                wrap.gam.fpco = list(distType = "dtw", window.type = "sakoechiba", window.size = c(8), k = c(15), add = TRUE)
                )

mparam <- NULL


# Estimate model 
res <- doOneSet(data = mydata, cores = 10, seed = 1000, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 10, splitvclass = FALSE)


# save
savedir = ""
save(res, file = paste(savedir, "/res_DTI2.RData", sep = ""))

