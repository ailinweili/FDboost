# library(devtools)
# install_git("git://github.com/ailinweili/FDboost.git", branch = "bfpco")
# library(FDboost)
# library(refund)
# library(plyr)

# function to perform cross validation for all models used
cv.wrap.func <- function(data, train_index, use.method, cvparam, mparam){
  
  if(is.null(data$rspdummy)){
  traindata <- lapply(train_index, FUN = function(m) list(y = data$y[m], x = data$x[m,], s = data$s))
  validdata <- lapply(train_index, FUN = function(m) list(y = data$y[-m], x = data$x[-m,], s = data$s))
  }else{
    traindata <- lapply(train_index, FUN = function(m) list(y = data$y[m], x = data$x[m,], s = data$s, rspdummy = data$rspdummy))
    validdata <- lapply(train_index, FUN = function(m) list(y = data$y[-m], x = data$x[-m,], s = data$s, rspdummy = data$rspdummy))
  }
  
  # generate a dataframe of cv hyperparameters for each use.method
  # deal with cvparam = NULL
  cvparam.df <- lapply(cvparam, FUN = function(x) unique(expand.grid(x, stringsAsFactors = FALSE)))
  names(cvparam.df)  <- paste(use.method, ".cvparam", sep = "")
  print(cvparam.df)
  
  res <- list()
  cvparam.new <- list()
  for( i in 1:length(cvparam.df)){
    
    if(use.method[i] %in% c("wrap.gam.fpco", paste("wrap.", "FDboost.", "fpco.", c("minkowski","dtw","correlation","elasticMetric"), sep = ""))){
      
      if(use.method[i] == "wrap.FDboost.fpco.minkowski") cvparam.1 <- list(distType = cvparam.df[[i]]$distType, p = as.numeric(cvparam.df[[i]]$p))
      if(use.method[i] == "wrap.FDboost.fpco.dtw") cvparam.1 <- list(distType = cvparam.df[[i]]$distType,  window.type = cvparam.df[[i]]$window.type, window.size = cvparam[[i]]$window.size)
      if(use.method[i] == "wrap.FDboost.fpco.correlation") cvparam.1 <- list(distType = cvparam.df[[i]]$distType)
      if(use.method[i] == "wrap.FDboost.fpco.elasticMetric") cvparam.1 <- list(distType = cvparam.df[[i]]$distType)
      if(use.method[i] == "wrap.gam.fpco") cvparam.1 <- list(distType = cvparam.df[[i]]$distType)
      cvparam.1 <- cvparam.1[sapply(1:length(cvparam.1), FUN = function(k){!is.null(cvparam.1[[k]])})]
      
      temp <- cv.wrap.fpco(traindata = traindata, validdata = validdata, 
                           cvparam = cvparam.df[[i]], distparam = cvparam.1, 
                           mparam = mparam[[i]], use.method = use.method[i])
      res[[i]] <- temp$res
      cvparam.new[[i]] <- temp$cvparam.new
      names(res)[i] <- use.method[i]
      names(res[[i]]) <- paste("cvparam.new.row", 1:nrow(cvparam.new[[i]]), sep = "")
      names(cvparam.new)[i] <- use.method[i]
    }
    
    
    if(use.method[i] %in% paste("wrap.", "FDboost.", c("fpc","bsignal"), sep = "")){
      for( j in 1:nrow(cvparam.df[[i]])){
        bstop <- select_mstop(smodel = do.call(use.method[i], args = list(data = traindata[[1]], 
               cvparam = if(is.data.frame(cvparam.df)) {as.list(cvparam.df[[i]][j,])} else {as.list(cvparam.df[[i]])},
               mparam = mparam[[i]], select = TRUE)))
        
        res[[i]] <- list()
        res[[i]][[j]] <- lapply(1:length(traindata), FUN = function(k) {do.call(use.method[i], 
            args = list(data = traindata[[k]], newdata = validdata[[k]], 
                        cvparam = if(is.data.frame(cvparam.df[[i]][j,])) {as.list(cvparam.df[[i]][j,])} else {as.list(cvparam.df[[i]])}, 
                        mparam = c(mparam[[i]], list(mstop = bstop)), 
                        select = FALSE))})
        names(res[[i]][[j]]) <- paste("cvdata", 1:length(traindata), sep = "")
      }
    
      cvparam.new[[i]] <- cvparam.df[[i]]
      names(cvparam.new)[i] <- use.method[i]
      
      names(res)[i] <- use.method[i]
      names(res[[i]]) <- paste("cvparam.new.row", 1:nrow(cvparam.new[[i]]), sep = "")
    }

    if(use.method[i] %in% paste("wrap.","gam.", c("pfr"), sep = "")){ # to be checked
      for( j in 1:nrow(cvparam.df[[i]])){
        res[[i]] <- list()
        res[[i]][[j]] <- lapply(1:length(traindata), FUN = function(k) {
          do.call(use.method[i], 
            args = list(data = traindata[[k]], newdata = validdata[[k]], 
                        cvparam = if(is.data.frame(cvparam.df[[i]][j,])) {as.list(cvparam.df[[i]][j,])} else {as.list(cvparam.df[[i]])}, 
                        mparam = mparam, 
                        select = FALSE))})
        names(res[[i]][[j]]) <- paste("cvdata", 1:length(traindata), sep = "")
      }
      
      cvparam.new[[i]] <- cvparam.df[[i]]
      names(cvparam.new)[i] <- use.method[i]
      
      names(res)[i] <- use.method[i]
      names(res[[i]]) <- paste("cvparam.new.row", 1:nrow(cvparam.new[[i]]), sep = "")
    }
    
  }    
  
  return(list(res = res, cvparam.new = cvparam.new))
}

# function to perform cross validation for fpco-based models, called by cv.wrap.func  
cv.wrap.fpco <- function(traindata, validdata, cvparam, distparam, mparam, use.method){
  
  # get unique distance measure
  dist.combi <- unique(expand.grid(distparam, stringsAsFactors = FALSE))
  
  # compute distance matrix for each distance measure and each training data
  d <- list(list())
  for(i in 1:nrow(dist.combi)){
    # if distType = "dtw", metric rather than method should be used!!!
    d[[i]] <- lapply(traindata, FUN = function(dd) {
      do.call(computeDistMat, args = c(list(x = dd$x, 
                  method = dist.combi$distType[i]),
                as.list(dist.combi[i, ][names(dist.combi) != "distType"])))})
    names(d[[i]]) <- paste("traindata", 1:length(traindata), sep = "")
  }
  
  # compute distance matrix of prediction dta for each distance measure and each train/validdataset
  if(use.method %in% c("wrap.gam.fpco")){
    dnew <- list(list()) 
    for(i in 1:nrow(dist.combi)){
      dnew[[i]] <- lapply(1:length(validdata), FUN = function(k) {
        do.call(computeDistMat, args = c(list(x = rbind(traindata[[k]]$x, validdata[[k]]$x), 
                      method = dist.combi$distType[i]), 
                 as.list(dist.combi[i, ][names(dist.combi) != "distType"]))
        )[1:nrow(traindata[[k]]$x), (nrow(traindata[[k]]$x) + 1) : (nrow(traindata[[k]]$x) + nrow(validdata[[k]]$x))]})
      names(dnew[[i]]) <- paste("newdistance", 1:length(validdata), sep = "")
    }
  }
  
  # unique cvparameter which are unrelated to distance measure
  cvparam.2 <- cvparam[!(names(cvparam) %in% names(distparam))]
  combi <- expand.grid(cvparam.2) # what if combi has 0 length?
  
  # cross validataion over each cvparam(distparam and cvparam.2)
  ntime <- ifelse(nrow(combi) == 0, 1,nrow(combi))
  ccombi <- dist.combi[rep(1:nrow(dist.combi), times = ntime), ] #ccombi must be a dataframe
  if(!is.data.frame(ccombi)) {
    ccombi <- data.frame(ccombi, stringsAsFactors = FALSE); names(ccombi) <- names(dist.combi)}
  ccombi$dnr <- rep(1:nrow(dist.combi), times = ntime)
  ccombi <- cbind(ccombi, combi[rep(1:nrow(combi), each = nrow(dist.combi)),])
  rownames(ccombi) <- NULL
  ccombi$mstop <- rep(100, times = nrow(ccombi))
  print(ccombi)
  
  # select number of iteration and estimate model 
  res <- list()
  if(use.method %in% paste("wrap.", "FDboost.", "fpco.", c("minkowski","dtw","correlation","elasticMetric"), sep = "")){
    for(i in 1:nrow(ccombi)){ 
      ccombi$mstop[i] <- select_mstop(smodel = do.call(use.method, args = list(data = traindata[[1]], 
                                                cvparam = as.list(ccombi[i,]), 
                                                mparam = c(mparam, list(d = d[[ccombi[i,"dnr"]]][[1]])), 
                                                select = TRUE)))
      res[[i]]<- lapply(1:length(traindata), FUN = function(k){
         do.call(use.method, args = list(data = traindata[[k]], newdata = validdata[[k]], 
                                         cvparam = as.list(ccombi[i,]), 
                                         mparam= c(mparam, list(d = d[[ccombi[i,"dnr"]]][[k]], 
                                                 dnew = if(exists("dnew")) {dnew[[ccombi[i, "dnr"]]][[k]]} else{NULL}, 
                                                 mstop = ccombi$mstop[i])),
                                         select = FALSE))})
      names(res[[i]]) <- paste("cvdata", 1:length(traindata), sep = "")
    }
  }
  
  if(use.method %in% c("wrap.gam.fpco")){
    for(i in 1:nrow(ccombi)){ 
      res[[i]]<- lapply(1:length(traindata), FUN = function(k){
        do.call(use.method, args = list(data = traindata[[k]], newdata = validdata[[k]], 
                                        cvparam = as.list(ccombi[i,]),
                                        mparam= c(mparam, list(d = d[[ccombi[i,"dnr"]]][[k]], 
                                                  dnew = dnew[[ccombi[i, "dnr"]]][[k]]))))})   # dnew is empty
      names(res[[i]]) <- paste("cvdata", 1:length(traindata), sep = "")
    }
  }
  
  return(list(res = res, cvparam.new = ccombi))
}


# wrap function of fpco-FDbooost (checked)
wrap.FDboost.fpco.minkowski <- wrap.FDboost.fpco.dtw <- 
wrap.FDboost.fpco.correlation <- wrap.FDboost.fpco.elasticMetric <- 
function(data, newdata = NULL, cvparam = NULL, mparam= NULL, select = FALSE){
  
  if((is.character(data$y) | is.factor(data$y)) & nlevels(factor(data$y)) > 2) {
    if(is.null(data$rspdummy)){
    rspdummy <- levels(factor(data$y))[-(nlevels(factor(data$y)))]
    } else {rspdummy = data$rspdummy}
  }
    
  # set parameter value, if not explicitly given set parameter to default value
  d = if(is.null(mparam$d)) {NULL} else {mparam$d}
  df = if(is.null(cvparam$df)) {4} else {cvparam$df}
  lambda = if(is.null(cvparam$lambda)) {NULL} else {cvparam$lambda}
  penalty =  if(is.null(cvparam$penalty)) {"identity"} else {cvparam$penalty}
  pve = if(is.null(cvparam$pve)) {0.95} else {cvparam$pve}
  npc = if(is.null(cvparam$npc)) {NULL} else {cvparam$npc}
  npc.max = if(is.null(cvparam$npc.max)) {15} else {cvparam$npc.max}
  add = if(is.null(cvparam$add)) {FALSE} else {cvparam$add}
  
  if(match.call()[[1]] == "wrap.FDboost.fpco.minkowski"){
    print("wrap.FDboost.fpco.minkowski")
    distType = if(is.null(cvparam$distType)) {'Minkowski'} else {cvparam$distType}
    p = if(is.null(cvparam$p)) {1} else {cvparam$p}
    
    fm <- formula(bquote(y ~ bfpco(x = x, s = s, index = NULL,
                     d = d, df = .(df), lambda = .(lambda), penalty = .(penalty),
                     pve = .(pve), npc = .(npc), npc.max = .(npc.max),
                     add = .(add), distType = .(distType), p = .(p))))
  }

  if(match.call()[[1]] == "wrap.FDboost.fpco.dtw"){
    print("wrap.FDboost.fpco.dtw")
    distType = if(is.null(cvparam$distType)) {"dtw"} else {cvparam$distType}
    window.type = if(is.null(cvparam$window.type)) {"none"} else {cvparam$window.type}
    window.size = if(is.null(cvparam$window.size)) {1} else {cvparam$window.size}
    
    fm <- formula(bquote(y ~ bfpco(x = x, s = s, index = NULL,
                                   d = d, df = .(df), lambda = .(lambda), penalty = .(penalty),
                                   pve = .(pve), npc = .(npc), npc.max = .(npc.max),
                                   add = .(add), distType = .(distType), window.type = .(window.type),
                                   window.size = .(window.size))))
  }

  if(match.call()[[1]] == "wrap.FDboost.fpco.correlation"){
    print("wrap.FDboost.fpco.correlation")
    distType = if(is.null(cvparam$distType)) {'correlation'} else {cvparam$distType}
    
    fm <- formula(bquote(y ~ bfpco(x = x, s = s, index = NULL,
                                   d = d, df = .(df), lambda = .(lambda), penalty = .(penalty),
                                   pve = .(pve), npc = .(npc), npc.max = .(npc.max),
                                   add = .(add), distType = .(distType))))
  }

  if(match.call()[[1]] == "wrap.FDboost.fpco.elasticMetric"){
    print("wrap.FDboost.fpco.elasticMetric")
    distType = if(is.null(cvparam$distType)) {'elasticMetric'} else {cvparam$distType}
    
    fm <- formula(bquote(y ~ bfpco(x = x, s = s, index = NULL,
                                   d = d, df = .(df), lambda = .(lambda), penalty = .(penalty),
                                   pve = .(pve), npc = .(npc), npc.max = .(npc.max),
                                   add = .(add), distType = .(distType)))) 
  }

  # change formula if multinoimal response
  if(!is.null(mparam$rspformula))
    fm <- as.formula(paste(deparse(fm, width.cutoff = 500), paste("%O%",mparam$rspformula, sep = ""), sep = ""))                   
  
  # run model 
  mod <- FDboost(fm, 
               timeformula = if(is.null(mparam$timeformula)) {NULL} else {mparam$timeformula},
               family = if(is.null(mparam$family)) {Gaussian()} else {mparam$family}, 
               control = boost_control(mstop = if(is.null(mparam$mstop)) {100} else {mparam$mstop}),
               data = data)
  
  # return model if select is TRUE
  if(select == TRUE)  return(mod)
  
  # make prediction
  if(exists("rspdummy") | nlevels(factor(data$y)) == 2){
   yhat.test <- predict(mod, newdata = newdata, type = "class")
   yhat.train <- fitted(mod, type = "class")
  }else {
   yhat.test <- predict(mod, newdata = newdata)
   yhat.train <- mod$fitted()
  }

  # return prediction value
  return(list(yhat.train = yhat.train, yhat.test = yhat.test))
}

# wrap function of fpc-FDbooost(checked) 
wrap.FDboost.fpc <- function(data, newdata = NULL, cvparam = NULL, mparam= NULL, select = FALSE){
  if((is.character(data$y) | is.factor(data$y)) & nlevels(factor(data$y)) > 2){
    rspdummy <- levels(factor(data$y)[-nlevels(factor(data$y))])}
  
  #set parameter value, if not explicitly given set parameter to default value
  df = if(is.null(cvparam$df)) {4} else {cvparam$df}
  lambda = if(is.null(cvparam$lambda)) {NULL} else {cvparam$lambda}
  penalty =  if(is.null(cvparam$penalty)) {"identity"} else {cvparam$penalty}
  pve = if(is.null(cvparam$pve)) {0.95} else {cvparam$pve}
  npc = if(is.null(cvparam$npc)) {NULL} else {cvparam$npc}
  npc.max = if(is.null(cvparam$npc.max)) {15} else {cvparam$npc.max}
  getEigen = if(is.null(cvparam$getEigen)) {TRUE} else {cvparam$getEigen}
  
  fm <- formula(bquote(y ~ bfpc(x = x, s = s, index = NULL, df = .(df), lambda = .(lambda), 
                         penalty = .(penalty), pve = .(pve), npc = .(npc), npc.max = .(npc.max),
                         getEigen = .(getEigen)))) 
  
  if(!is.null(mparam$rspformula))
    fm <- as.formula(paste(deparse(fm, width.cutoff = 500), paste("%O%",mparam$rspformula, sep = ""), sep = ""))                   
  
  mod <- FDboost(fm, 
                 timeformula = if(is.null(mparam$timeformula)) {NULL} else {mparam$timeformula},
                 family = if(is.null(mparam$family)) {Gaussian()} else {mparam$family}, 
                 control = boost_control(mstop = if(is.null(mparam$mstop)) {100} else {mparam$mstop}),
                 data = data)
  
  if(select == TRUE)  return(mod)
  
  # make prediction
  if(exists("rspdummy") | nlevels(factor(data$y)) == 2){
    yhat.test <- predict(mod, newdata = newdata, type = "class")
    yhat.train <- fitted(mod, type = "class")
  }else {
    yhat.test <- predict(mod, newdata = newdata)
    yhat.train <- mod$fitted()
  }
  
  # return predicted value
  return(list(yhat.train = yhat.train, yhat.test = yhat.test))
}

# wrap function of bsignal-FDbooost(checked: not real error x is not centered per column, inducing a non-centered effect) 
wrap.FDboost.bsignal <- function(data, newdata = NULL, cvparam = NULL, mparam= NULL, select = FALSE){
  if((is.character(data$y) | is.factor(data$y)) & nlevels(factor(data$y)) > 2){
    rspdummy <- levels(factor(data$y)[-nlevels(factor(data$y))])}
  
  inS = if(is.null(cvparam$inS)) {'smooth'} else {cvparam$inS} # Check default value for inS
  knots =  if(is.null(cvparam$knots)) {10} else {cvparam$knots}
  boundary.knots =  if(is.null(cvparam$boundary.knots)) {NULL} else {cvparam$boundary.knots}
  degree =  if(is.null(cvparam$degree)) {3} else {cvparam$degree}
  differences =  if(is.null(cvparam$differences)) {1} else {cvparam$differences}
  df =  if(is.null(cvparam$df)) {4} else {cvparam$df}
  lambda =  if(is.null(cvparam$lambda)) {NULL} else {cvparam$lambda}
  center =  if(is.null(cvparam$center)) {FALSE} else {cvparam$center}
  cyclic =  if(is.null(cvparam$cyclic)) {FALSE} else {cvparam$cyclic}
  Z =   if(is.null(cvparam$Z)) {NULL} else {cvparam$Z}
  penalty = if(is.null(cvparam$penalty)) {'ps'} else {cvparam$penalty}
  check.ident =  if(is.null(cvparam$check.ident)) {FALSE} else {cvparam$check.ident}
  
  # set formula, if y is multinomial, specific form is taken
  fm <- formula(bquote(y ~ bsignal(x = x, s = s, index = NULL, inS = .(inS), knots = .(knots),
                             boundary.knots = .(boundary.knots), degree = .(degree),
                             differences = .(differences), df = .(df), lambda = .(lambda),
                             center = .(center), cyclic = .(cyclic), Z = Z, penalty = .(penalty),
                             check.ident = .(check.ident))))
  
  if(!is.null(mparam$rspformula))
    fm <- as.formula(paste(deparse(fm, width.cutoff = 500), paste("%O%",mparam$rspformula, sep = ""), sep = ""))                   
  
  
  mod <- FDboost(fm, 
                 timeformula = if(is.null(mparam$timeformula)) {NULL} else {mparam$timeformula},
                 family = if(is.null(mparam$family)) {Gaussian()} else {mparam$family}, 
                 control = boost_control(mstop = if(is.null(mparam$mstop)) {100} else {mparam$mstop}),
                 data = data)
  
  if(select == TRUE)  return(mod)
  
  # make prediction
  if(exists("rspdummy") | nlevels(factor(data$y)) == 2){
    yhat.test <- predict(mod, newdata = newdata, type = "class")
    yhat.train <- fitted(mod, type = "class")
  }else {
    yhat.test <- predict(mod, newdata = newdata)
    yhat.train <- mod$fitted()
  }
  
  return(list(yhat.train = yhat.train, yhat.test = yhat.test))
}

# wrap function of fpco-gam(checked)
wrap.gam.fpco <- function(data, newdata = NULL, cvparam = NULL, mparam = NULL, select = FALSE){
  
  #set parameter value, if not explicitly given set parameter to default value
  y <- data$y
  dummy <- rep(1, times = length(data$y))
  bs <- 'pco'
  k <- if(is.null(cvparam$k)) {2} else {cvparam$k}
  D <- if(is.null(mparam$d)) {NULL} else {mparam$d}
  add <- if(is.null(cvparam$add)) {FALSE} else {cvparam$add}
  fastcmd <- if(is.null(cvparam$fastcmd)){FALSE} else {cvparam$cmdscale}
  Dnew <- if(is.null(mparam$dnew)) {NULL} else{mparam$dnew}
  
  if(is.null(mparam$family)) mparam$family = gaussian(link = "identity")
  if(mparam$family$family == "multinom"){
    # set formula
    ylevels <- levels(factor(y))
    ynlevels <- nlevels(factor(y))

    ynum <- mapvalues(y, from = ylevels, to = (0:(ynlevels-1)))
    if(is.factor(ynum)) ynum <- as.numeric(levels(ynum))[ynum]
    if(!is.numeric(ynum)) stop("response of gam.fpco model is not a numeric vector!")
    
    fm <- list(formula(ynum ~ s(dummy, bs = 'pco', k = k,  
                        xt = list(D = D, add = add, fastcmd = fastcmd))))
    for ( i in 1:(ynlevels-2)) 
      fm[[i+1]] <- formula(~ s(dummy, bs = 'pco', k = k, xt = list(D = D, add = add, fastcmd = fastcmd)))
    
    # estimate model
    mdata = list(ynum = ynum, dummy = dummy)
    mod <- gam(formula = fm, 
               method = if(is.null(mparam$method)) {"REML"} else {mparam$method},
               family = mparam$family,
               data = mdata)
    
    # if called for select mstop, return mod 
    if(select == TRUE) return(mod)
    
    # prediction
    dist_list <- list(dummy = D)
    pred_data <- pco_predict_preprocess(mod, newdata=NULL, dist_list)
    temp <- predict(mod, pred_data, type = "response")
    yhat.train <- apply(temp, MARGIN = 1, FUN = function(x) {which(x == max(x)) - 1})
    yhat.train <- mapvalues(yhat.train, from = (0:(ynlevels-1)), to = ylevels, warn_missing = FALSE)
    
    dist_list2 <- list(dummy = Dnew)
    pred_data2 <- pco_predict_preprocess(mod, newdata=NULL, dist_list2)
    temp2 <- predict(mod, pred_data2, type = "response")
    yhat.test <- apply(temp2, MARGIN = 1, FUN = function(x) {which(x == max(x)) - 1}) 
    yhat.test <- mapvalues(yhat.test, from = (0:(ynlevels-1)), to = ylevels, warn_missing = FALSE)
  }else 
    { # mparam$family != "multinom"
    # set formula
    fm <- formula(y ~ s(dummy, bs = 'pco', k = k,  
                        xt = list(D = D, add = add, fastcmd = fastcmd)))
    
    # estimate model
    mod <- gam(formula = fm, 
               method = if(is.null(mparam$method)) {"REML"} else {mparam$method},
               family = mparam$family)
    
    # if called for select mstop, return mod 
    if(select == TRUE) return(mod)
    
    # prediction
    yhat.train <- mod$fitted.values
    
    dist_list <- list(dummy = Dnew)
    pred_data <- pco_predict_preprocess(mod, newdata=NULL, dist_list)
    yhat.test <- predict(mod, pred_data)
  }
  
  
  return(list(yhat.train = yhat.train, yhat.test = yhat.test))
}  

# wrap function of af-pfr(checked)
wrap.gam.pfr <- function(data, newdata = NULL, cvparam = NULL, mparam = NULL, select = FALSE){
  basistype <- if(is.null(cvparam$basistype)) {"te"} else {cvparam$basistype}
  integration <- if(is.null(cvparam$integration)) {"simpson"} else {cvparam$basistype}
  L <- if(is.null(cvparam$L)) {NULL} else {cvparam$L}
  presmooth <- if(is.null(cvparam$presmooth)) {NULL} else {cvparam$presmooth}
  presmooth.opts <- if(is.null(cvparam$presmooth.opts)) {NULL} else {cvparam$presmooth.opts}
  Xrange <- if(is.null(cvparam$Xrange)) {range(data$x, na.rm = T)} else {cvparam$Xrange}
  Qtransform <- if(is.null(cvparam$Qtransform)) {FALSE} else {cvparam$Qtransform}
  bs <- if(is.null(cvparam$bs)) {"tp"} else {cvparam$bs}
  m <- if(is.null(cvparam$m)) {NA} else {cvparam$m}
  k <- if(is.null(cvparam$K)) {-1} else {cvparam$K}
  
  fm <- formula(bquote(y ~ af(x, basistype = .(basistype), integration = .(integration),
                              L = .(L), presmooth = presmooth, presmooth.opts = presmooth.opts,
                              Xrange = Xrange, Qtransform = .(Qtransform),
                              k =.(k), m =.(m), bs = .(bs))))
  
  mod <- pfr(formula = fm, 
             method = if(is.null(mparam$method)) {"GCV.Cp"} else{mparam$method}, 
             gamma = if(is.null(mparam$gamma)) {1.2} else{mparam$gamma}, 
             data = data)
  
  if(select == TRUE) return(mod)
  
  yhat.train <- mod$fitted.values
  yhat.test <- predict(mod, newdata = newdata)
  return(list(yhat.train = yhat.train, yhat.test = yhat.test))
}
  
###############################################################################
# Function to get training data index
get_index <- function(data, response = "y", 
                      frac = 0.7, splitvclass = FALSE){
  # check response
  if(is.null(data[[response]]))
    stop(paste(response, "is not a valid variable name of data", sep = " "))
  if(!(is.numeric(data[[response]]) | is.logical(data[[response]]) | is.factor(data[[response]])))
    stop(paste(response, "is not scalar", sep = " "))
  
  # compute index for training set and test set according to the response data type
#   if(is.numeric(data[[response]])){
#     # split numeric response proportionally
#     index = which(!is.na(data[[response]]))
#     train_index = sample(index, size = round(length(index)*frac), replace = FALSE)
#     test_index = setdiff(index, train_index)
#   }
  
#  if(is.factor(data[[response]]) | is.character(data[[response]]) | is.logical(data[[response]])){
    # split nominial response 
    if(splitvclass == FALSE){
      # split nominial response like splitting numeric response
      index = which(!is.na(data[[response]]))
      train_index = sample(index, size = round(length(index)*frac), replace = FALSE)
      test_index = setdiff(index, train_index)
    }else{
      #  split each class of response proportionally 
      tempresponse <- factor(data[[response]])
      train_index <- vector("list", length = nlevels(tempresponse))
      test_index <- vector("list", length = nlevels(tempresponse))
      
      for(j in 1:nlevels(tempresponse)){
        index = which(tempresponse == levels(tempresponse)[j] & !is.na(tempresponse)) 
        train_index[[j]] = sample(index, size = round(length(index)*frac), replace = FALSE)
        test_index[[j]] = setdiff(index, train_index[[j]])
      }
      train_index = unlist(train_index)
      test_index = unlist(test_index)
    }
#  }  
  
  return(list(train_index = train_index, test_index = test_index))
}  

# Function to select best number of iteration for FDboost
select_mstop <- function(smodel, mstop_grid = c(100,1000,5000,10000), B = 5){
  
  # set the initial value of bstop(best stop)
  bstop = mstop_grid[1]
  
  # set cv folds
  cvf <- cv(model.weights(smodel), type = "kfold", B = B)
  
  # increase model iterations until the best iteration does not exceed model iterations.
  for (i in 1:length(mstop_grid)){
    bstop <- mstop(cvrisk(smodel[mstop_grid[i]], grid = 1:mstop_grid[i]))
    print(i)
    if (bstop < mstop_grid[i]) break
  }
  
  return(bstop)
}