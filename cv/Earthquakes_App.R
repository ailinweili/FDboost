# library(devtools)
# install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
# library(FDboost)
# library(dtw)
# library(proxy)
# library(classiFunc)
# library(gamm4)
# library(refund)
# library(foreign)

libdir = "/zpool1/s11226758/master_thesis/data_application"
library(stabs, lib.loc = libdir)
library(mboost, lib.loc = libdir)
library(backports, lib.loc = libdir)
library(FDboost, lib.loc = libdir)
library(proxy, lib.loc = libdir)
library(dtw,lib.loc = libdir)
library(lme4, lib.loc = libdir)
library(gamm4, lib.loc = libdir)
library(refund, lib.loc = libdir)
library(parallel, lib.loc = libdir)
library(foreign, lib.loc = libdir)
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

# prepare data
# setwd("/Users/WeiliLin/Documents/Statistics/master_thesis/data_application/Earthquakes")
# Earthquakes1 <- read.arff("Earthquakes_TRAIN.arff")
# Earthquakes2 <- read.arff("Earthquakes_TEST.arff")
# Earthquakes <- rbind(Earthquakes1, Earthquakes2)

Earthquakes <- read.arff("/zpool1/s11226758/master_thesis/data_application/data/Earthquakes.arff")

mydata <- list(y = Earthquakes$target, x = I(Earthquakes[, colnames(Earthquakes) != "target"]), s = 1:512)
mydata$x <- as.matrix(mydata$x)
# set parameter 
use.method = c("wrap.FDboost.fpco.minkowski", "wrap.FDboost.fpco.elasticMetric",
               "wrap.FDboost.fpco.correlation", "wrap.FDboost.fpco.dtw",
               "wrap.FDboost.fpc", "wrap.FDboost.bsignal")

cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = c(1,2,5,10), pve = c(0.95, 0.7), add = c(TRUE, FALSE), fastcmd = c(TRUE, FALSE)),
                wrap.FDboost.fpco.elastic = list(distType = "elasticMetric", add = c(TRUE, FALSE), pve = c(0.95, 0.7), fastcmd = c(TRUE, FALSE)),
                wrap.FDboost.fpco.correlation = list(distType = "correlation", pve = c(0.95, 0.7), add = c(TRUE, FALSE), fastcmd = c(TRUE, FALSE)),
                wrap.FDboost.fpco.dtw = list(distType = "dtw", window.type = c("sakoechiba", "itakura", "none"), window.size = c(5,10,20,30,50)), 
                wrap.FDboost.fpc = list(pve = c(0.95, 0.7)),
                wrap.FDboost.bsignal = list(knots = c(10,20,30), differences = c(1,2))
)

mparam <- list(wrap.FDboost.fpco.minkowski = list(family = Binomial()),
               wrap.FDboost.fpco.elastic = list(family = Binomial()),
               wrap.FDboost.fpco.correlation = list(family = Binomial()),
               wrap.FDboost.fpco.dtw = list(family = Binomial()),
               wrap.FDboost.fpc = list(family = Binomial()),
               wrap.FDboost.bsignal = list(family = Binomial())
               #wrap.gam.fpco = list(family = binomial(link = "logit"))
)

# Estimate model 
res <- doOneSet(data = mydata, cores = 10, seed = 2003, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 10, splitvclass = TRUE)

# save res
savedir = "/zpool1/s11226758/master_thesis/data_application/CodeRes/results"
save(res, file = paste(savedir, "/res_Earthquakes.RData", sep = ""))
     