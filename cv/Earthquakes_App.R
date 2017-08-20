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



##################help function#################################################
doOneSet <- function(useParallel = TRUE, cores = 4, seed = 8323, 
                     data, use.method, cvparam, mparam, ptrain, nfold){
  
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
################################################################################

# prepare data
setwd("/Users/WeiliLin/Documents/Statistics/master_thesis/data_application/Earthquakes")
Earthquakes1 <- read.arff("Earthquakes_TRAIN.arff")
Earthquakes2 <- read.arff("Earthquakes_TEST.arff")
Earthquakes <- rbind(Earthquakes1, Earthquakes2)

mydata <- list(y = Earthquakes$target, x = I(Earthquakes[, colnames(Earthquakes) != "target"]), s = 1:512)
mydata$x <- as.matrix(mydata$x)
# set parameter 
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
mparam <- list(wrap.FDboost.fpco.minkowski = list(family = Binomial()),
               wrap.FDboost.fpco.elastic = list(family = Binomial()),
               wrap.FDboost.fpco.correlation = list(family = Binomial()),
               wrap.FDboost.fpco.dtw = list(family = Binomial()),
               wrap.FDboost.fpc = list(family = Binomial()),
               wrap.FDboost.bsignal = list(family = Binomial()),
               wrap.gam.fpco = list(family = binomial(link = "logit")))
ptrain = 0.7
nfold = 5

# Estimate model 
res <- doOneSet(data = mydata, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 2)

