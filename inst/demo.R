library(mgcv)
library(refund)
library(FDboost)
library(classiFunc)
library(dtw)



### check X_fpco function #####################################################
## test computation of new PCOs 
data("fuelSubset")

mf = data.frame(fuelSubset$UVVIS)
names(mf) <- NULL
vary = ""
args = hyper_fpco(mf = mf, vary = vary, npc = 5, npc.max = 15, s = fuelSubset$uvvis.lambda)

# compute PCOs
res1 <- X_fpco(mf, vary, args)
# compute PCOs for new points ()
res2 <- X_fpco(mf, vary, res1$args)

# new PCOs should be the same(in terms of absolute value) as the old one
all.equal(abs(res1$args$klX$points), abs(res2$args$klX$points))


## compute PCOs for new data with identical signal index
# compute PCOs
res3 <- X_fpco(mf, vary, args)
# compute PCOs for new data points
newdata = as.matrix(mf[1:50,]) + matrix(rnorm(50*134,0,1), nrow = 50, ncol = 134)
newdata <- as.data.frame(newdata)
names(newdata) <- NULL
res4 <- X_fpco(mf = newdata, vary, res3$args) # ncol(res4$X) = npc

## compute new PCOs with different signal index
# generat new data by take the first 100 rows of mf and the rowmeans of every 3 columns
newdata = data.frame(matrix(NA, ncol = 40, nrow = 100))
for( i in 1:40 ) 
newdata[,i] = rowMeans(mf[1:100, (3*i):(3*i+2)])
newindex = vector()
for(i in 1:40) 
newindex[i] = mean(fuelSubset$uvvis.lambda[(3*i):(3*i+2)])
attr(newdata[[1]], "signalIndex") <- newindex 
# <Fix me > why use first term of a dataframe
names(newdata) <- NULL
res5 <- X_fpco(mf = newdata, vary, res3$args)
newpcos <- res5$X # ncol(res5$X) = npc



### check cmdscale_lanczos_new function#########################################
## comparison of cmdscale_lanczos and cmdscale_lanczos_new
data(fuelSubset)
x = fuelSubset$UVVIS
d = computeDistMat(x, method = "dtw")

i = 5
res1 <- cmdscale_lanczos_new(d, npc = i, pve = 0.99, npc.max = 15, eig = TRUE)
res2 <- refund:::cmdscale_lanczos(d, k = i, eig = TRUE)
all.equal(abs(res1$points),abs(res2$points)) #TRUE
all.equal(abs(res1$evalues),abs(res2$eig[1:length(res1$evalues)])) #TRUE
all.equal(res1$x, res2$x) # TRUE
all.equal(res1$ac, res2$ac) #TRUE

res3 <- cmdscale_new(d, npc = i, pve = 0.99, npc.max = 15, eig = TRUE)
res4 <- cmdscale(d, k = i, eig = TRUE)
all.equal(abs(res3$points),abs(res4$points)) #TRUE
all.equal(abs(res3$evalues),abs(res4$eig[1:length(res3$evalues)])) #TRUE
all.equal(res3$x, res4$x) #TRUE
all.equal(res3$ac, res4$ac) #TRUE



#### check bfpco function ######################################################

## check the return object of bfpco
library(mgcv)
library(dtw)
data(fuelSubset)

x = fuelSubset$UVVIS
s = fuelSubset$uvvis.lambda

bs1 <- bfpco(x, s, distType = "Euclidean") # class blg
bs2 <- bfpc(x, s)

bs1$model.frame() # 129*134
bs2$model.frame()
all.equal(bs1$model.frame(), bs2$model.frame())
bs1$get_call()
bs2$get_call() 
bs1$get_data() # 129*1, practically 129*134
bs2$get_data() # 129*1, practically 129*134
bs1$get_index() # NULL
bs2$get_index() # NULL
bs1$get_vary() # ""
bs2$get_vary() # ""
bs1$get_names() 
bs2$get_names()
bs1$dpp(weights = rep(1, nrow(x))) 
bs2$dpp(weights = rep(1, nrow(x)))
 
# look into dpp 
temp = bs1$dpp(weights = rep(1, nrow(x))) 
# PCO coeffcient
temp$fit(y = fuelSubset$heatan)$model    # 15*1
# fitted value
temp$fit(y = fuelSubset$heatan)$fitted()   #129*1
# Hat matrix
temp$hatvalues()   #129*129
# prediction 
temp$predict(bm = list(temp$fit(y = fuelSubset$heatan)))  #129*1
newdata = vector("list", length = length(names(bs1)))
names(newdata) = names(bs1)
newdata[["get_data"]] = x[1:50,] # somehow this does not work, because newdata is not set properly
temp$predict(bm = list(temp$fit(y = fuelSubset$heatan)), newdata = newdata)
# degree of freedom
temp$df()
# the names of PCOs
temp$Xnames


### Comparison of bfpco based FDboost, bfpc based FDboost and pco based gam 
# Generate data, the toy dataset analyzed by Phillip(2017) is used
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
y.rainbow <- rainbow(30, end=0.9)[(y.toy-min(y.toy))/
                                    diff(range(y.toy))*29+1]

dummy <- rep(1,30) # dummy response variable


# Display data
par(mfrow=c(2, 2))
matplot((0:100)/100, t(Xnl[c(4, 25), ]), type="l", xlab="t", ylab="",
        ylim=range(X.toy), main="Noiseless functions")
matplot((0:100)/100, t(X.toy[c(4, 25), ]), type="l", xlab="t", ylab="",
        ylim=range(X.toy), main="Observed functions")
matplot((0:100)/100, t(X.toy), type="l", lty=1, col=y.rainbow, xlab="t",
        ylab="", main="Rainbow plot")

# Obtain DTW distances
D.dtw <- computeDistMat(X.toy, method="dtw", window.type="sakoechiba", window.size=5)

# Model data
toydata <- list(y.toy = y.toy, X.toy = X.toy, s = 1:101)

# Fit pco-based gam model(m1), fpco-based boosting model(m2), fpc-based boosting model(m3)
m1 <- gam(y.toy ~ s(dummy, bs="pco", k=15, xt=list(D=D.dtw)), method="REML")

m2 <- FDboost(y.toy ~ bfpco(X.toy, s = s, distType = "dtw", pve = 0.99,
                           window.type="sakoechiba", window.size=5), 
             timeformula = NULL, data = toydata, control = boost_control(mstop = 200))

m3 <- FDboost(y.toy ~ bfpc(X.toy, s = s), timeformula = NULL, data = toydata)  
 
# Model fitted values
fitteds = data.frame(y.toy = toydata$y.toy, fitted_pco = m1$fitted.values, fitted_bfpco = m2$fitted(), fitted_bfpc = m3$fitted())

par(mfrow=c(2, 2))
obs_id = 1:length(toydata$y.toy)
matplot(obs_id, fitteds, type = c("l"), lwd = 1.5, col = 1:4 , main = "model fitted value")
legend("topleft", legend = c("y.toy", "fitted_pco", "fitted_fpco", "fitted_fpc"), col=1:4, lwd = 1.5, cex = 0.6)

# Average of square residual
c(resid_pco = mean(m1$residuals^2), resid_fpco = mean(m2$resid()^2), resid_fpc = mean(m3$resid()^2))


# Prediction for new data
# Case when new data have the same time index
newdd = list(X.toy = Xnl + matrix(rnorm(30*101, 0, 0.05), 30), s = 1:101)

newpred_m2 = predict(m2, newdata = newdd)
newpred_m3 = predict(m3, newdata = newdd)
newpreds = data.frame(y.toy = toydata$y.toy, pred_fpco = newpred_m2, pred_fpc = newpred_m3)

matplot(obs_id, newpreds, type = c("l"), lwd = 1.5, col = 1:4, main = "predicton for new data at identical time grids")
legend("topleft", legend = c("y.toy","pred_fpco", "pred_fpc"), col=1:4, lwd = 1.5, cex = 0.7)

# Case when new data have different time index
new_xtoy = data.frame()
for( i in 1:32 ) 
  new_xtoy[1:nrow(X.toy),i] = rowMeans(X.toy[1:nrow(X.toy), (3*i):(3*i+2)])

new_s = vector()
for(i in 1:32) 
  new_s[i] = mean(newdd$s[(3*i):(3*i+2)])

newdd_2 = list(X.toy = as.matrix(new_xtoy), s = as.integer(new_s))

newpred2_m2 = predict(m2, newdata = newdd_2)
newpred2_m3 = predict(m3, newdata = newdd_2) 
newpreds2 = data.frame(y.toy = toydata$y.toy, pred_fpco = newpred2_m2, newpred_fpc = newpred2_m3)

matplot(obs_id, newpreds, type = c("l"), pch = 1, col = 1:4, main = "prediciton for new data at differnt time grids")
legend("topleft", legend = c("y.toy","pred_fpco", "pred_fpc"), col=1:4, lwd = 1.5 , cex = 0.7)



## Classfication application: Binomial
toydata$labels <-  cut(toydata$y.toy, breaks = c(0, 55, 100), labels = LETTERS[1:2])

binm1 <- FDboost(labels ~ bfpco(X.toy, s = s, distType = "dtw", pve = 0.9,
                               window.type="sakoechiba", window.size=5), 
                 timeformula = ~bols(1), 
                 data = toydata, 
                 family = Binomial(),
                 control = boost_control(mstop = 200)
                )

# Use newdd as newdata 
newdd = list(X.toy = Xnl + matrix(rnorm(30*101, 0, 0.05), 30), s = 1:101)

# prediction
pred_binm1 <- predict(binm1, newdata = newdd, type = "class")



# Multinomial classification
toydata$multilabels <-  cut(toydata$y.toy, breaks = c(0, 25,50,75,100), labels = LETTERS[1:4])
toydata$ydummy <- factor(levels(toydata$multilabels)[levels(toydata$multilabels) != "A"])

mulm1 <- FDboost(multilabels ~ bfpco(X.toy, s = s, distType = "dtw", pve = 0.9,
                                    window.type="sakoechiba", window.size=5)%O%
                               bols(ydummy, df = 4, contrasts.arg = "contr.dummy"), 
                timeformula = ~bols(1), 
                data = toydata, 
                family = Multinomial(),
                control = boost_control(mstop = 200)
                )

# use newdd as newdata
newdd2 = list(X.toy = Xnl + matrix(rnorm(30*101, 0, 0.05), 30), s = 1:101, ydummy = toydata$ydummy)
pred_mulm1 <- predict(mulm1, newdata = newdd2, type = "class")


# methods of bfpco-based FDboost class
# <Fixed me > coef(m2) does not work because 'd' was not correctly computed by makeGrid function
# <Fixed me > plot(m2) does not work because the call of coef() function

## Model performance over number of principal coordinates
aveperf_fpco = sapply(1:15, FUN = function(i) {mean(
  FDboost(y.toy ~ bfpco(X.toy, s = s, distType = "dtw", npc = i,
                        window.type="sakoechiba", window.size=5), 
          timeformula = ~ bols(1), data = toydata, 
          control = boost_control(mstop = 1000))$resid()^2 ) })
  
aveperf_fpc = sapply(1:15, FUN = function(i) {mean(
  FDboost(y.toy ~ bfpc(X.toy, s = s, npc = i), timeformula = NULL, data = toydata,
          control = boost_control(mstop = 1000))$resid()^2)})

aveperf_eucl = sapply(1:15, FUN = function(i) {mean(
  FDboost(y.toy ~ bfpco(X.toy, s = s, distType = "Euclidean", npc = i),
          timeformula = ~ bols(1), data = toydata, 
          control = boost_control(mstop = 1000))$resid()^2 ) } )

gcvdata <- data.frame(aveperf_fpco, aveperf_fpc, aveperf_eucl)
matplot(1:15, gcvdata, type = "b", main = "model performance over number of pc/pco",
        xlab = "number of pc/pco", ylab = "GCV", pch = c(17,15,16), col = c("red", "black", "green"))
legend("topright", legend = c("dtw_fpco", "fpc", "euclidean_fpco"), c(17,15,16), c("red", "black", "green"), cex = 0.5)
