library(devtools)
install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
library(FDboost)
library(dtw)
library(proxy)
library(classiFunc)
library(gamm4)
library(refund)

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
    get_index(data = data, response = "y", frac = ptrain , splitvclass = FALSE)$train_index})
  
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



# gnerate data
data(DTI2)
DIT2s <- DTI2[,c("cca", "pasat")]
DTI2c <- DIT2s[complete.cases(DIT2s),]
s = as.numeric(1:93)
mydata = list(x = DTI2c$cca, y = DTI2c$pasat, s = 1:93)

# set parameter 
use.method = c("wrap.FDboost.fpco.minkowski", "wrap.FDboost.fpco.elasticMetric",
               "wrap.FDboost.fpco.correlation", "wrap.FDboost.fpco.dtw",
               "wrap.FDboost.fpc", "wrap.FDboost.bsignal",
               "wrap.gam.fpco","wrap.gam.pfr")

cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = 1),
                wrap.FDboost.fpco.elastic = list(distType = "elasticMetric"),
                wrap.FDboost.fpco.correlation = list(distType = "correlation"),
                wrap.FDboost.fpco.dtw = list(distType = "dtw"),
                wrap.FDboost.fpc = list(npc = 5),
                wrap.FDboost.bsignal = list(penalty = "ps"),
                wrap.gam.fpco = list(distType = "dtw"),
                wrap.gam.pfr = list(k = -1)
                )
mparam <- NULL
ptrain = 0.7
nfold = 5

# Estimate model 
res <- doOneSet(data = data, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 5)





