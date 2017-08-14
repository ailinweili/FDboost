library(devtools)
install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
library(FDboost)

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

# gnerate data
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

cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = 1),
                wrap.FDboost.fpco.elastic = list(distType = "elasticMetric"),
                wrap.FDboost.fpco.correlation = list(distType = "correlation"),
                wrap.FDboost.fpco.dtw = list(distType = "dtw"))
mparam <- NULL
ptrain = 0.7
nfold = 5

# Estimate model 
res <- doOneSet(data = data, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 5)

x <- list(a = matrix(1:99 + rnorm(99,0,1), ncol = 3), b = matrix(1:6000 + rnorm(6000, 0 ,1), ncol = 40), 
          d = matrix(1:6000 + rnorm(6000,0,1), ncol = 40), e = matrix(1:6000+ rnorm(6000, 0 ,1), ncol= 40),
          f = matrix(1:99+ rnorm(198, 0 ,1), ncol = 3))
proc.time()
mclapply(1:5, FUN = function(i){
  computeDistMat(x[[i]], method = "amplitudeDistance")
  print(dim(x[[i]]))}, mc.preschedule = TRUE)
proc.time()

dist_dtw <- proxy::pr_DB$get_entry("dtw")$FUN

proc.time()
res5 <- as.matrix(proxy::dist(x, method = "dtw")) #28 seconds
proc.time()
res6 <- computeDistMat(x, method = "custom.metric", custom.metric = proxy::pr_DB$get_entry("dtw")$FUN) #29seconds
proc.time()
res7 <- computeDistMat(x, method = "custom.metric", custom.metric = dist_dtw) #28seconds
proc.time()

