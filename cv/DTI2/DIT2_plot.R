load("/Users/WeiliLin/Documents/Statistics/master_thesis/algorithm_code/FDboost_bfpco_github/FDboost/cv/res_DTI2/res_DTI2.RData")

data("DTI2")
DIT2s <- DTI2[,c("cca", "pasat")]
DTI2c <- DIT2s[complete.cases(DIT2s),]
s = as.numeric(1:93)
mydata = list(x = DTI2c$cca, y = DTI2c$pasat, s = 1:93)

# DIT2 res does not contain train_index
y.train <- lapply(res$train_index, FUN = function(x) mydata$y[x])
y.valid <- lapply(res$train_index, FUN = function(x) {
  valid_index <- setdiff((1:length(mydata$y)),x)
  y.valid <- mydata$y[valid_index]
  return(y.valid)})
nfold = 10
names(y.train)  <- paste("y.train", 1:nfold, sep = "")
names(y.valid)  <- paste("y.valid", 1:nfold, sep = "")
all.equal(length(mydata$y), sum(length(y.train[[1]]), length(y.valid[[1]])))

#help function
compute_mse <- function(predlist, nfold, msetype = "valid"){
  mse <- lapply(predlist, FUN = function(x){
    lapply(1:nfold, FUN = function(i) {
      mse.train <- mean((x[[i]]$yhat.train - y.train[[i]])^2)
      mse.valid <- mean((x[[i]]$yhat.test - y.valid[[i]])^2)
      if(msetype == "both") return(list(mse.train = mse.train, mse.valid = mse.valid))
      if(msetype == "train") return(list(mse.train = mse.train))
      if(msetype == "valid") return(list(mse.valid = mse.valid))
    })})
  return(mse)
}

get_gpdata <- function(msedata, cvparam, subname = c("p", "pve", "add")){
  temp <- lapply(1:length(msedata), FUN = function(i) unlist(msedata[[i]]))
  plotdata <- t(do.call(cbind, lapply(temp, data.frame, stringsAsFactors=FALSE)))
  rownames(plotdata) <- NULL; colnames(plotdata) <- paste("mse.cv", 1:nfold, sep = "")
  plotdata <- cbind(plotdata, cvparam[, subname, drop = FALSE])
  
  gpdata <- melt(plotdata, id = subname, value.name = "mse", variable.name = "CVnumber")
  for(i in 1:length(subname)) gpdata[,subname[i]] <- as.factor(gpdata[,subname[i]])
  return(gpdata)
}

###############compute mse for all models#######################################
mse.train <- unlist(lapply(1:nfold, FUN = function(i) mean((y.train[[i]] - mean(y.train[[i]]))^2)))
mse.test <- unlist(lapply(1:nfold, FUN = function(i) mean((y.valid[[i]] - mean(y.train[[i]]))^2)))
temp <- lapply(1:(length(res)-1), function(i){compute_mse(predlist = res[[i]]$res[[1]], nfold = 10)})
names(temp) <- names(res)[1:(length(res)-1)]
temp$intercept.model <- list(train = mse.train, test = mse.test)
summary_mse <- lapply(temp, FUN = function(x){lapply(x, FUN = function(y) {y = unlist(y); return(c(average = mean(y), quantile = quantile(y, probs = c(0.5)), sd = sd(y)))})})
print(t(as.data.frame(summary_mse)))

mse.Minkowski <- temp$wrap.FDboost.fpco.minkowski
mse.elasticMetric <- temp$wrap.FDboost.fpco.elasticMetric
mse.correlation <- temp$wrap.FDboost.fpco.correlation
mse.dtw.sakoechiba <- temp[[4]]
mse.dtw.itakura <- temp[[5]]
mse.dtw.none <- temp[[6]]
mse.fpc <- temp$wrap.FDboost.fpc
mse.bsignal <- temp$wrap.FDboost.bsignal
mse.pfr <- temp$wrap.gam.pfr
mse.intercept <- temp$intercept.model
mse.gam <- temp$wrap.gam.fpco

## best model of each methods
best_ave_mse <-lapply(summary_mse, function(x){
  mse_ave = as.data.frame(t(as.data.frame(x)))$average
  best_mse = min(mse_ave)
  index = which(mse_ave == best_mse)
  sd = as.data.frame(t(as.data.frame(x)))$sd[index]
  return(c(best_mse = best_mse, index = index, sd = sd) )})

best_mse<-lapply(1:(length(res)-1), function(i){temp[[i]][[best_ave_mse[[i]][2]]]}) 
names(best_mse) <-  names(res)[1:(length(res)-1)]
best_mse[[5]] <- best_mse[[5]]  <- NULL # the best dtw model reamians others are discarded


##### print cvparam.new for all models #########################################
cvparam_all <- lapply(res[1:(length(res)-1)], FUN = function(x) x$cvparam.new)
best_cvparam <- lapply(1:length(cvparam_all), FUN = function(i) {
  index = best_ave_mse[[i]][2]
  y = cvparam_all[[i]][[1]][index, , drop = FALSE]
  return(y)
})
names(best_cvparam) <- names(cvparam_all)
print(best_cvparam)

############## plot for all models ###################################################
gpdata.intercept <- data.frame(mse = c(mse.train, mse.test), type = rep(c('mse.train','mse.test'), each = length(mse.train)))
plot1 <- ggplot(gpdata.intercept, aes(x = type, y = mse, fill = type)) + geom_boxplot(fill = "lightblue") + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  labs(title = "Intercept-Model", x = "") + theme(legend.position="bottom")


gpdata.Minkowski <- get_gpdata(mse.Minkowski, res$wrap.FDboost.fpco.minkowski$cvparam.new$wrap.FDboost.fpco.minkowski, subname = c("p", "pve","add"))
plot2 <- ggplot(gpdata.Minkowski, aes(x = pve, y = mse, fill = add)) + geom_boxplot() + facet_grid(~p, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("FDboost-fpco-minkowski") + theme(legend.position="bottom")


gpdata.elasticMetric <- get_gpdata(mse.elasticMetric, res$wrap.FDboost.fpco.elasticMetric$cvparam.new$wrap.FDboost.fpco.elasticMetric, subname = c("pve","add"))
plot3 <- ggplot(gpdata.elasticMetric, aes(x = pve, y = mse, fill = add)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("FDboost-fpco-elasticMetric") + theme(legend.position="bottom")


gpdata.correlation <- get_gpdata(mse.correlation, res$wrap.FDboost.fpco.correlation$cvparam.new$wrap.FDboost.fpco.correlation, subname = c("pve","add"))
plot4 <- ggplot(gpdata.correlation, aes(x = pve, y = mse, fill = add)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("FDboost-fpco-correlation") + theme(legend.position="bottom")


gpdata.dtw.sakoechiba<- get_gpdata(mse.dtw.sakoechiba, res[[4]]$cvparam.new$wrap.FDboost.fpco.dtw, subname = c("window.size","pve","add"))
plot5 <- ggplot(gpdata.dtw.sakoechiba, aes(x = pve, y = mse, fill = add)) + geom_boxplot() + facet_grid(~window.size, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("FDboost-fpco-dtw(sakoechiba)") + theme(legend.position="bottom")


gpdata.dtw.itakura<- get_gpdata(mse.dtw.itakura, res[[5]]$cvparam.new$wrap.FDboost.fpco.dtw, subname = c("window.size","pve","add"))
plot6 <- ggplot(gpdata.dtw.itakura, aes(x = pve, y = mse, fill = add)) + geom_boxplot() + facet_grid(~window.size, labeller = label_both) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) +
  ggtitle("FDboost-fpco-dtw(itakura)") + theme(legend.position="bottom")


gpdata.dtw.none<- get_gpdata(mse.dtw.none, res[[6]]$cvparam.new$wrap.FDboost.fpco.dtw, subname = c("pve","add"))
plot7 <- ggplot(gpdata.dtw.none, aes(x = pve, y = mse, fill = add)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") +  ylim(0,270) +
  ggtitle("FDboost-fpco-dtw(none)") + theme(legend.position="bottom")


gpdata.fpc <- get_gpdata(mse.fpc, res$wrap.FDboost.fpc$cvparam.new$wrap.FDboost.fpc, subname = c("pve"))
plot8 <- ggplot(gpdata.fpc, aes(x = pve, y = mse, fill = pve)) + geom_boxplot(fill = "lightblue") + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("FDboost-fpc") + theme(legend.position="bottom")


gpdata.bsignal <- get_gpdata(mse.bsignal, res$wrap.FDboost.bsignal$cvparam.new$wrap.FDboost.bsignal, subname = c("knots", "differences"))
plot9 <- ggplot(gpdata.bsignal, aes(x = differences, y = mse, fill = knots)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("FDboost-bsignal") + theme(legend.position="bottom")


gpdata.pfr <- get_gpdata(mse.pfr, res$wrap.gam.pfr$cvparam.new$wrap.gam.pfr, subname = c("k1", "k2"))
plot10 <- ggplot(gpdata.pfr, aes(x = k1, y = mse, fill = k2)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("pfr-af") + theme(legend.position="bottom")

gpdata.gam <- get_gpdata(mse.gam, res$wrap.gam.fpco$cvparam.new$wrap.gam.fpco, subname = c("window.size", "k"))
plot11 <- ggplot(gpdata.gam, aes(x = k, y = mse, fill = window.size)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + 
  scale_fill_brewer(palette="Blues") + ylim(0,270) + 
  ggtitle("gam-fpco-dtw(sak)") + theme(legend.position="bottom")

#### plot best model of each methods####################################
tp <- lapply(1:length(best_mse), FUN = function(i) unlist(best_mse[[i]]))
plotdata <- t(do.call(cbind, lapply(tp, data.frame, stringsAsFactors=FALSE)))
rownames(plotdata) <- c("FDb.fpco.mink", "FDb.fpco.ela","FDb.fpco.corr",
                        "FDb.fpco.dtw","FDb.fpc","FDb.bsig","pfr.af", "gam.fpco.dtw")
colnames(plotdata) <- paste("mse.cv", 1:nfold, sep = "")
# add intercept model for comparison
plotdata = rbind(plotdata, intercept.mod = mse.test)
gpdata <- melt(plotdata, id = subname, value.name = "mse", varnames = c("method", "CVnumber"))
plot <- ggplot(gpdata, aes(x = method, y = mse)) + geom_boxplot(fill = "lightblue") + 
  stat_summary(fun.y = mean, geom = "point", shape=23, size=4) + labs( x = "")

