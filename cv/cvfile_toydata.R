library(devtools)
install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
library(FDboost)

source(file = "library.R")

doOneSet <- function(data, use.method, cvparam, mparam, ptrain, nfold){
  
  set.seed(8323)
  train_index <- lapply(1:length(1:nfold), FUN = function(k){
    get_index(data = data, response = "y", frac = ptrain , splitvclass = FALSE)$train_index})
  
  temp <- doOneRep(data, train_index, use.method, cvparam, mparam)
  res <- c(temp, train_index = list(train_index))
  return(res)
}

doOneRep <- function(data, train_index, use.method, cvparam, mparam){
  
  # use parallel here
  
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
use.method = "wrap.FDboost.fpco.minkowski"
cvparam <- list(wrap.FDboost.fpco.minkowski = list(distType = "Minkowski", p = 1))
mparam <- NULL
ptrain = 0.7
nfold = 5

# Estimate model 
res <- doOneSet(data = data, use.method = use.method, cvparam = cvparam, mparam = mparam, 
                ptrain = 0.7, nfold = 5)


