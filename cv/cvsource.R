# get_index function gets the index for training set and test set. If response is 
# multinomial, each class of response can be perfectly proportionally split by setting
# splitvclass = TRUE. mydata is either a list or a dataframe.
get_index <- function(mydata, response = "response", 
                      frac = 0.7, splitvclass = TRUE){
  # check response
  if(is.null(mydata[[response]]))
    stop(paste(response, "is not a variable in mydata", sep = " "))
  if(!(is.numeric(mydata[[response]]) | is.factor(mydata[[response]]) | is.character(mydata[[response]])))
    stop(paste(response, "is not of type numeric, factor or character!", sep = " "))
  
  # compute index for training set and test set according to the response data type
  if(is.numeric(mydata[[response]])){
    # split numeric response proportionally
    index = which(!is.na(mydata[[response]]))
    train_index = sample(index, size = round(length(index)*frac), replace = FALSE)
    test_index = setdiff(index, train_index)
  }
  
  if(is.factor(mydata[[response]]) | is.character(mydata[[response]])){
    # split nominial response 
    if(splitvclass == FALSE){
      # split nominial response like splitting numeric response
      index = which(!is.na(mydata[[response]]))
      train_index = sample(index, size = round(length(index)*frac), replace = FALSE)
      test_index = setdiff(index, train_index)
    }else{
      #  split each class of response proportionally 
      tempresponse <- factor(mydata[[response]])
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
  }  
    
  return(list(train_index = train_index, test_index = test_index))
}  

# CVdata accepts mydata(data in form of a list or a dataframe) and returns a list of two sublists: cvtrain, cvtest.
# cvtrain(cvtest) contains nfold dataset, each dataset is a training set(test set). For each of the nfold 
# iteration, get_index function is called to compute index of training set and index of test set. Variables in mydata are
# subsequently split according to their attributes(nosplitvars, matrix, or vector).
CVdata <- function(mydata, nfold, frac = 0.7, splitvclass = TRUE, 
                   response = "response", nosplitvars = c("pixel", "rspdummy")){
  # set structure of training list and test list
  cvtrain = vector("list", length = nfold)
  cvtest = vector("list", length = nfold)
  
  # distinguish 1 or 2 dimensional features
  typ <- unlist(lapply(mydata, FUN = class))
  if(sum(names(mydata) == "") > 0) stop("Unnamed variables are not allowed in mydata!")
  matnames <- names(mydata[which(typ %in% c("matrix"))])
  matnames <- matnames[!(matnames %in% nosplitvars)]
  vecnames <- names(mydata[which(typ %in% c("character", "numeric","factor"))])
  vecnames <- vecnames[!(vecnames %in% nosplitvars)]
  if(length(which(!(typ %in% c("matrix", "character", "numeric", "factor")))) > 0){ 
    stop("there are variables in 'mydata' which are not of the type 'matrix',
         'character', 'numeric' or 'factor', such features can not be correctly 
         splitted into training and test data!")
  }
  
  # perform nfold cross validation on mydata, split 1 dimensional, 2 dimensional
  # variables separately, nosplitvars are not split
  for( i in 1:nfold){
    
    cvtrain[[i]] <- list()
    cvtest[[i]] <- list()
    cvtrain_unord <- list()
    cvtest_unord <- list()
    
    train_index = get_index(mydata, response, frac, splitvclass)$train_index
    test_index = get_index(mydata, response, frac, splitvclass)$test_index
    
    if(!is.null(matnames)){
      for( nms in matnames) {
        cvtrain_unord[[nms]] <- mydata[[nms]][train_index, ]
        cvtest_unord[[nms]] <- mydata[[nms]][test_index, ]
      }
    }
    
    if(!is.null(vecnames)){
      for( nms in vecnames) {
        cvtrain_unord[[nms]] <- mydata[[nms]][train_index]
        cvtest_unord[[nms]] <- mydata[[nms]][test_index]  
      }
    }
    
    for(nms in nosplitvars){
      cvtrain_unord[[nms]] <- mydata[[nms]]
      cvtest_unord[[nms]] <- mydata[[nms]]
    }
    
    for(nms in names(mydata)){
      cvtrain[[i]][[nms]] <- cvtrain_unord[[nms]]
      cvtest[[i]][[nms]] <- cvtest_unord[[nms]]
    }
  }
  
  # return data 
  return(list(cvtrain = cvtrain, cvtest = cvtest))
}

# select_mstop computes the best value for the number of iteration(mstop). mdlargs 
# and addargs are arguments to fit a single initiall model.The iteration of the model 
# is then gridly increased until the best value for the number of iteration is found.
select_mstop <- function(funname = "FDboost", mdlargs, mstop_grid, B = 3, 
                         addargs = list(data = cvdata$cvtrain[[1]],
                                        control = boost_control(mstop = 100))) {
  # build a single initial model
  smodel <- do.call(funname, args = c(mdlargs, addargs))
  
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

# CVmodel performs iteration number selection(if desired) and cross validation on cvdata. 
# If mstop is not given or smstop is TRUE, selection of iteration number is 
# completed by calling select_mstop function. Models using the selected iteration 
# number are fitted on cross validation data(cvdata), accuracy is computed if ACC = TRUE.
# Parameter 'cvdata' is designly obtained by callling CVdata function. CVmodel function 
# returns fitted cross validation models, cvdata, selected iteration number and 
# returns conditionally also accuracy(if ACC = TRUE).
CVmodel <- function(funname = "FDboost", cvdata = NULL, mstop = NULL, smstop = FALSE, mstop_grid = NULL, B = NULL,
                    mdlargs = NULL, response = "response", ACC = TRUE, pred_type = "class"){
  if(!all.equal(names(cvdata), c("cvtrain", "cvtest")))
    stop("cvdata should contain a 'cvtrain' and a 'cvtest' list!")
  
  # compute the value of mstop if not given
  if(smstop | is.null(mstop)){
    mstop <- select_mstop(funname = funname, mdlargs = mdlargs, mstop_grid = mstop_grid, B = B)
  }
  
  # build models
  mdls <- lapply(cvdata$cvtrain, FUN = function(x) { 
    do.call(funname, args = c(mdlargs, data = list(x), control = list(boost_control(mstop = mstop))))
  })
  
  # compute accuracy if ACC is TRUE
  if(ACC == TRUE){
    accuracy <- unlist(lapply(seq_along(mdls), FUN = function(i){
      pred <- predict(mdls[[i]], 
                      newdata = cvdata$cvtest[[i]][which(names(cvdata$cvtest[[i]]) != response)], 
                      type = pred_type)
      sum(pred == cvdata$cvtest[[i]][[response]])/length(cvdata$cvtest[[i]][[response]])
    }))
  }
  
  return(list(cvmdls = mdls, accuracy = if(ACC) accuracy, cvdata = cvdata, mstop = mstop))
}

