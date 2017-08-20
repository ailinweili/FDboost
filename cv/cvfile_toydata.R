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
################################################################################


####################toydata application with gaussian response##################
# gnerate dataset1 with y of gaussian distribution
Xnl <- matrix(0, 30, 101)
set.seed(813)
tt <- sort(sample(1:90, 30))
for(i in 1:30){
  Xnl[i, tt[i]:(tt[i]+4)] <- -1
  Xnl[i, (tt[i]+5):(tt[i]+9)] <- 1
}
X.toy <- Xnl + matrix(rnorm(30*101, ,0.05), 30)
colnames(X.toy) <- paste("X", 1:101, sep = "")
y.toy <- tt + rnorm(30, 0.05)

data <- list(y = y.toy, x = X.toy, s = 1:101)

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
mparam <- NULL
ptrain = 0.7
nfold = 5

# Estimate model 
res <- doOneSet(data = data, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 2)
################################################################################


##################toydata applicaton with mulitnomial response #################

# gnerate dataset2 with y of multinom distribution
Xnl <- matrix(0, 30, 101)
set.seed(813)
tt <- sort(sample(1:90, 30))
for(i in 1:30){
  Xnl[i, tt[i]:(tt[i]+4)] <- -1
  Xnl[i, (tt[i]+5):(tt[i]+9)] <- 1
}
X.toy <- Xnl + matrix(rnorm(30*101, ,0.05), 30)
colnames(X.toy) <- paste("X", 1:101, sep = "")
y.toy <- tt + rnorm(30, 0.05)

multilabels <- vector("numeric", length = length(y.toy))
multilabels[y.toy < 30] <- 0
multilabels[y.toy < 60 & y.toy> 30] <- 1
multilabels[y.toy > 60] <- 2
multilabels <- factor(multilabels)

data2 <- list(y = multilabels, x = X.toy, s = 1:101, rspdummy = c(1,2))

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
               wrap.gam.fpco = list(family = multinom(K = 2))
)

# Estimate model 
res <- doOneSet(data = data2, seed = 100, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.8, nfold = 2)




