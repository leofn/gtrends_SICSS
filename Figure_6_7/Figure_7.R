rm(list=ls())

library(gbm)
library(ggplot2)
library(DataCombine)
library(plyr, dplyr)
library(arm)
library(ROCR)
library(caret)
library(pROC)
library(mlbench)


load("../Data/all_protests.RData")
load("../Data/coef_var_daily.RData")

types <- ddply(data, ~city, summarize, mean(type,na.rm=T))[,2]
cities <- unique(data$city)
d <- data.frame(cbind(types, cities))
d <- d[d$cities %in% unique(d2$city),]
types_cities <-data.frame(as.numeric(as.character(rep(d[,1], each=60))), d2$city)

d <- data.frame(d2, types_cities[,1])
colnames(d)[4] <- c("type")
d$protest <- ifelse(d$type==0, 0, ifelse(d$t==60, 1, 0))

d$labels_group <- ifelse(d$type==1, "is_protest", "is_noprotest")
d$label <- ifelse(d$protest==1, "protest", "noprotest")

d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day1', slideBy = -1)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day2', slideBy = -2)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day3', slideBy = -3)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day4', slideBy = -4)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day5', slideBy = -5)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day6', slideBy = -6)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day7', slideBy = -7)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day14', slideBy = -14)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day21', slideBy = -21)
d <- slide(d, 'lcoef_var_day', TimeVar = 't', GroupVar = 'city', NewVar = 'lcoef_var_day28', slideBy = -28)


#############################################
#########     "MANUAL" K-FOLD CV    #########
#############################################

set.seed(90)
folds <- 10
n = 10000
cut = c(train = .8, test=.2)
accuracy_train <- NULL
auc_train <- NULL
cm_train <- list()
accuracy_test <- NULL
auc_test <- NULL
cm_test <- list()
values_roc <- list()

for (i in 1:folds) {
  train_cities <- sample(unique(d$city), round(cut["train"]*length(unique(d$city))))
  test_cities <- sample(unique(d$city), round(cut["test"]*length(unique(d$city))))
  train <- d[d$city %in% train_cities,]
  test <- d[d$city %in% test_cities,]
  
  mb <- gbm(protest ~ lcoef_var_day1 + lcoef_var_day2 + lcoef_var_day3 + lcoef_var_day4 +
              lcoef_var_day5 + lcoef_var_day6 + lcoef_var_day7, 
            data = train, distribution = "gaussian",n.trees = n,
            shrinkage = 0.2, interaction.depth = 12)
  
  #### TRAINING SET
  n.trees = seq(from=100 ,to=n, by=100)
  predmatrix <- predict(mb,train, n.trees = n.trees, type="link")
  pred_train <- apply(predmatrix, 1, mean)
  train$pred_train <- as.numeric(as.character(pred_train))
  prediction_train <- prediction(train$pred_train, train$protest)
  perf <- performance(prediction_train,"tpr","fpr")
  acc.perf = performance(prediction_train, measure = "acc")
  index = which.max( slot(acc.perf, "y.values")[[1]])
  acc_train = slot(acc.perf, "y.values")[[1]][index]
  cutoff = slot(acc.perf, "x.values")[[1]][index]
  accuracy_train[i] <- acc_train
  # Area under ROC
  auc.perf_train = performance(prediction_train, measure = "auc")
  auc_train[i] <- unlist(auc.perf_train@y.values)
  #CM
  y_hat_train <- ifelse(pred_train >cutoff, 1, 0)
  cm_train_t <- confusionMatrix(as.factor(y_hat_train), as.factor(train$protest))[[2]]
  cm_train[[i]] <- cm_train_t
  
  #### TEST SET
  n.trees = seq(from=100 ,to=n, by=100)
  predmatrix <- predict(mb,test, n.trees = n.trees, type="link")
  pred_test <- apply(predmatrix, 1, mean)
  test$pred_test <- as.numeric(as.character(pred_test))
  prediction_test <- prediction(test$pred_test, test$protest)
  perf <- performance(prediction_test,"tpr","fpr")
  values_roc[[i]] <- perf
  acc.perf = performance(prediction_test, measure = "acc")
  index = which.max( slot(acc.perf, "y.values")[[1]])
  acc_test = slot(acc.perf, "y.values")[[1]][index]
  cutoff = slot(acc.perf, "x.values")[[1]][index]
  accuracy_test[i] <- acc_test
  # Area under ROC
  auc.perf_test = performance(prediction_test, measure = "auc")
  auc_test[i] <- unlist(auc.perf_test@y.values)
  #CM
  y_hat_test <- ifelse(pred_test >cutoff, 1, 0)
  cm_test_t <- confusionMatrix(as.factor(y_hat_test), as.factor(test$protest))[[2]]
  cm_test[[i]] <- cm_test_t
  
}

## Get means of all folds
acc_train_mean <- mean(accuracy_train)
acc_train_mean
acc_test_mean <- mean(accuracy_test)
acc_test_mean
auc_train_mean <- mean(auc_train)
auc_train_mean
auc_test_mean <- mean(auc_test)
auc_test_mean

cm_train_means <- lapply(cm_train, mean)

cm_train_means <- apply(simplify2array(cm_train), 1:2, mean)
cm_train_means
cm_test_means <- apply(simplify2array(cm_test), 1:2, mean)
t(cm_test_means)
rec <- cm_test_means[4] / (sum(cm_test_means[,2]))
rec
prec <- cm_test_means[4] / (sum(cm_test_means[2,]))
prec
f_score = (2*prec*rec) / sum(prec, rec)
f_score

x <- lapply(values_roc, function(x) x@x.values)[-6]
y <- lapply(values_roc, function(x) x@y.values)[-6]
x.s <- lapply(x, function(i) i[[1]][1:min(sapply(lapply(x, "[[", 1), length))])
x1 <- NULL
for(i in 1:length(x.s)){ 
  x1 <- rbind(x1, x.s[[i]])
}
x2 <- apply(x1, 2, mean)
y.s <- lapply(y, function(i) i[[1]][1:min(sapply(lapply(y, "[[", 1), length))])
y1 <- NULL
for(i in 1:length(y.s)){ 
  y1 <- rbind(y1, y.s[[i]])
}
y2 <- apply(y1, 2, mean)

#save.image("../Data/results_fig7.RData") # Save and reload, as code takes a while to run

#load("../Data/results_fig7.RData")

t(cm_test_means)
auc_test_mean
rec
f_score

# ROC CURVE PLOT

pdf("../Figure_6_7/Figure_7.pdf", width = 13, height=12)
par(mar=c(5.5, 6, 3, 1))
plot(x2, y2, type='l', bty='l', xlab="", 
     ylab="", lwd=2.5, col="blue", main="ROC Curve (Model 2)", 
     cex.axis=2.5, cex.lab=2.5, cex.main=2.5)
mtext("False Positive Rate", side=1, line=4, cex=2.5)
mtext("True Positive Rate", side=2, line=4, cex=2.5)
abline(a=0, b= 1)
legend(x = "bottomright", legend = c("Coef. of Variation"), fill = c(4), cex=2)
dev.off()






