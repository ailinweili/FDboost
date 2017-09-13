load("/Users/WeiliLin/Documents/Statistics/master_thesis/algorithm_code/FDboost_bfpco_github/FDboost/cv/res_MedImage/res_MedImage_singlegam.RData")
library(ggplot2)
library(reshape2)
library(gridExtra)
library(stringr)
library(foreign)
library(FDboost)

# get data 
medgf <- read.arff("/Users/WeiliLin/Documents/Statistics/master_thesis/data_application/MedicalImages/MedicalImages.arff")
index19 <- which(medgf$target %in% c(1,9))
set.seed(512)
index10 <- sample(which(medgf$target == 10), size = 100, replace = FALSE)
mymedgf <- medgf[c(index19,index10),]
mymedgf$target <- droplevels(mymedgf$target)

## reconstruct data
mydata <- list(y = mymedgf$target, x = as.matrix(mymedgf[,-100]), s = as.numeric(1:99))
mydata$rspdummy <- factor(levels(mymedgf$target)[levels(mymedgf$target) != 10])

# generate training data and validation data
y.train <- lapply(res$train_index, FUN = function(x) mydata$y[x])
y.valid <- lapply(res$train_index, FUN = function(x) {
  valid_index <- setdiff((1:length(mydata$y)),x)
  y.valid <- mydata$y[valid_index]
  return(y.valid)})
nfold = 10
names(y.train)  <- paste("y.train", 1:nfold, sep = "")
names(y.valid)  <- paste("y.valid", 1:nfold, sep = "")
all.equal(length(mydata$y), sum(length(y.train[[1]]), length(y.valid[[1]])))


# help function
compute_msr<- function(predlist, nfold, msrtype = "valid"){
  msr <- lapply(predlist, FUN = function(x){
    lapply(1:nfold, FUN = function(i) {
      msr.train <- sum(x[[i]]$yhat.train != y.train[[i]])/length(y.train[[i]])
      msr.valid <- sum(x[[i]]$yhat.test != y.valid[[i]])/length(y.valid[[i]])
      if(msrtype == "both") return(list(msr.train = msr.train, msr.valid = msr.valid))
      if(msrtype == "train") return(list(msr.train = msr.train))
      if(msrtype == "valid") return(list(msr.valid = msr.valid))})})
  return(msr)
}

get_gpdata <- function(msrdata, cvparam, subname = c("p", "pve", "add")){
  temp <- lapply(1:length(msrdata), FUN = function(i) unlist(msrdata[[i]]))
  plotdata <- t(do.call(cbind, lapply(temp, data.frame, stringsAsFactors=FALSE)))
  rownames(plotdata) <- NULL; colnames(plotdata) <- paste("msr.cv", 1:nfold, sep = "")
  plotdata <- cbind(plotdata, cvparam[, subname, drop = FALSE])
  
  gpdata <- melt(plotdata, id = subname, value.name = "msr", variable.name = "CVnumber")
  for(i in 1:length(subname)) gpdata[,subname[i]] <- as.factor(gpdata[,subname[i]])
  return(gpdata)
}



################compute msr for all models#######################################
temp <- lapply(1:(length(res)-1), function(i){compute_msr(predlist = res[[i]]$res[[1]], nfold = 10)})
names(temp) <- names(res)[1:(length(res)-1)]
summary_msr <- lapply(temp, FUN = function(x){lapply(x, FUN = function(y) {y = unlist(y); return(c(average = mean(y), quantile = quantile(y, probs = c(0.5)), sd = sd(y)))})})
print(t(as.data.frame(summary_msr)))

msr.minkowski <- temp$wrap.FDboost.fpco.minkowski
msr.elasticMetric <- temp$wrap.FDboost.fpco.elasticMetric
msr.correlation <- temp$wrap.FDboost.fpco.correlation
msr.dtw.sakoechiba <- temp[[4]]
msr.dtw.itakura <- temp[[5]]
msr.dtw.none <- temp[[6]]
msr.fpc <- temp$wrap.FDboost.fpc
msr.bsignal <- temp$wrap.FDboost.bsignal
msr.gam <- temp$wrap.gam.fpco

## best model of each methods
best_ave_msr <-lapply(summary_msr, function(x){
  msr_ave = as.data.frame(t(as.data.frame(x)))$average
  best_msr = min(msr_ave)
  index = which(msr_ave == best_msr)
  sd = as.data.frame(t(as.data.frame(x)))$sd[index]
  return(c(best_msr = best_msr, index = index, sd = sd) )})

best_msr<-lapply(1:(length(res)-1), function(i){temp[[i]][[best_ave_msr[[i]][2]]]}) 
names(best_msr) <-  names(res)[1:(length(res)-1)]
best_msr[[5]] <- best_msr[[5]]  <- NULL # the best dtw model reamians others are discarded

##### print cvparam.new for all models #########################################
cvparam_all <- lapply(res[1:(length(res)-1)], FUN = function(x) x$cvparam.new)
best_cvparam <- lapply(1:length(cvparam_all), FUN = function(i) {
  index = best_ave_msr[[i]][2]
  y = cvparam_all[[i]][[1]][index, , drop = FALSE]
  return(y)
})
names(best_cvparam) <- names(cvparam_all)
print(best_cvparam)

############## plot for each method separately ###################################################
gpdata.minkowski <- get_gpdata(msr.minkowski, res$wrap.FDboost.fpco.minkowski$cvparam.new$wrap.FDboost.fpco.minkowski, subname = c("p", "pve","add"))
plot2 <- ggplot(gpdata.minkowski, aes(x = pve, y = msr, fill = add)) + geom_boxplot() + facet_grid(~p, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("FDboost-fpco-minkowski") + theme(legend.position="bottom")


gpdata.elasticMetric <- get_gpdata(msr.elasticMetric, res$wrap.FDboost.fpco.elasticMetric$cvparam.new$wrap.FDboost.fpco.elasticMetric, subname = c("pve","add"))
plot3 <- ggplot(gpdata.elasticMetric, aes(x = pve, y = msr, fill = add)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("FDboost-fpco-elasticMetric") + theme(legend.position="bottom")


gpdata.correlation <- get_gpdata(msr.correlation, res$wrap.FDboost.fpco.correlation$cvparam.new$wrap.FDboost.fpco.correlation, subname = c("pve","add"))
plot4 <- ggplot(gpdata.correlation, aes(x = pve, y = msr, fill = add)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("FDboost-fpco-correlation") + theme(legend.position="bottom")


gpdata.dtw.sakoechiba<- get_gpdata(msr.dtw.sakoechiba, res[[4]]$cvparam.new$wrap.FDboost.fpco.dtw, subname = c("window.size","add","pve"))
plot5 <- ggplot(gpdata.dtw.sakoechiba, aes(x = pve, y = msr, fill = add)) + geom_boxplot() + facet_grid(~window.size, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("FDboost-fpco-dtw(sak)") + theme(legend.position="bottom")


gpdata.dtw.itakura<- get_gpdata(msr.dtw.itakura, res[[5]]$cvparam.new$wrap.FDboost.fpco.dtw, subname = c("window.size","add","pve"))
plot6 <- ggplot(gpdata.dtw.itakura, aes(x = pve, y = msr, fill = add)) + geom_boxplot() + facet_grid(~window.size, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) +
  ggtitle("FDboost-fpco-dtw(ita)") + theme(legend.position="bottom")


gpdata.dtw.none<- get_gpdata(msr.dtw.none, res[[6]]$cvparam.new$wrap.FDboost.fpco.dtw, subname = c("pve","add","window.type"))
plot7 <- ggplot(gpdata.dtw.none, aes(x = pve, y = msr, fill = add)) + geom_boxplot() + 
  facet_grid(~window.type, labeller = label_both) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") +  ylim(0.2,0.6) +
  ggtitle("FDboost-fpco-dtw(no)") + theme(legend.position="bottom")


gpdata.fpc <- get_gpdata(msr.fpc, res$wrap.FDboost.fpc$cvparam.new$wrap.FDboost.fpc, subname = c("pve"))
plot8 <- ggplot(gpdata.fpc, aes(x = pve, y = msr)) + geom_boxplot(fill = "lightblue") + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("FDboost-fpc") + theme(legend.position="bottom")


gpdata.bsignal <- get_gpdata(msr.bsignal, res$wrap.FDboost.bsignal$cvparam.new$wrap.FDboost.bsignal, subname = c("knots", "differences"))
plot9 <- ggplot(gpdata.bsignal, aes(x = differences, y = msr, fill = knots)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("FDboost-bsignal") + theme(legend.position="bottom")

gpdata.gam <- get_gpdata(msr.gam, res$wrap.gam.fpco$cvparam.new$wrap.gam.fpco, subname = c("p", "add", "k"))
plot10 <- ggplot(gpdata.gam, aes(x = k, y = msr, fill = add)) + geom_boxplot() + 
  facet_grid(~p, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0.2,0.6) + 
  ggtitle("gam-fpco-mink") + theme(legend.position="bottom")


#### plot best model of each methods####################################
tp <- lapply(1:length(best_msr), FUN = function(i) unlist(best_msr[[i]]))
plotdata <- t(do.call(cbind, lapply(tp, data.frame, stringsAsFactors=FALSE)))
rownames(plotdata) <- c("FDb.fpco.mink", "FDb.fpco.ela","FDb.fpco.corr",
                        "FDb.fpco.dtw","FDb.fpc","FDb.bsig","gam.fpco.mink")
colnames(plotdata) <- paste("msr.cv", 1:nfold, sep = "")
gpdata <- melt(plotdata, id = subname, value.name = "msr", varnames = c("method", "CVnumber"))
plot <- ggplot(gpdata, aes(x = method, y = msr)) + geom_boxplot(fill = "lightblue") + 
  stat_summary(fun.y = mean, geom = "point", shape=23, size=4) + labs( x = "")




