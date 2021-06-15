rm(list=ls())

library(ggplot2)
library(DataCombine)
library(plyr, dplyr)
library(arm)
library(ROCR)
library(caret)
library(pROC)
library(mlbench)

load("../Data/all_protests.Rdata")
load("../Data/coefvar_data.RData")

list <- coefvar_data[unlist(lapply(coefvar_data, function(x) nrow(x) > 1440))]
coefvar_data60d <- lapply(list, "[", c(1:1440))
d <- data.frame(t(do.call(rbind.data.frame, coefvar_data60d)))
colnames(d) <- names(coefvar_data60d)
rownames(d) <- c(1:1440)

## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##  LAST TWO WEEKS DATA
## ## ## ## ## ## ## ## ## ## ## ## ## ## 
w2c2 <- 1440
w2c1 <- w2c2-168
last1week_max <- apply(d[w2c1:w2c2,], 2, max)
last1week_min <- apply(d[w2c1:w2c2,], 2, min)
last1week = last1week_max - last1week_min

w4c2 <- 1440-168
w4c1 <- w4c2-168*2
last2weeks_max <- apply(d[w4c1:w4c2,], 2, max)
last2weeks_min <- apply(d[w4c1:w4c2,], 2, min)
last2weeks = last2weeks_max - last2weeks_min

types <- ddply(data, ~city, summarize, mean(type,na.rm=T))[,2]
cities <- unique(data$city)
data <- data.frame(cbind(types, cities))
data <- data[data$cities %in% colnames(d),]

data <- data.frame(cbind(data, last1week, last1week_max, last1week_min, last2weeks,last2weeks_max, last2weeks_min))
data$types <- as.numeric(as.character(data$types))
data$labels <- ifelse(data$types==1, "protest", "noprotest")


# Accuracy function for later
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])}, 
    perf@x.values, perf@y.values, pred@cutoffs)
}

# split sets
set.seed(90)
cut = c(train = .8, .2)
groups = sample(cut(seq(nrow(data)), nrow(data)*cumsum(c(0,cut)), labels = names(cut)))
res = split(data, groups)
train <- res[[1]]
test <- res[[2]]

###############
#### MODEL ####
###############
m <- glm(types ~ last1week_max + last2weeks_max, family = binomial(logit), data=train) #for b plot
summary(m)
m

### Training set:
pred_train <- predict(m)
train$pred_train <- as.numeric(as.character(pred_train))

prediction_train <- prediction(train$pred_train, train$types)
perf <- performance(prediction_train,"tpr","fpr")
#pdf("Figures/roc_curve_2w_1.pdf", width = 15, height=12)
plot(perf,colorize=TRUE)
abline(a=0, b= 1)
#dev.off()

# Accuracy:
print(opt.cut(perf, prediction_train)) ## 
acc.perf = performance(prediction_train, measure = "acc")
index = which.max( slot(acc.perf, "y.values")[[1]] )
acc_train = slot(acc.perf, "y.values")[[1]][index]
cutoff = slot(acc.perf, "x.values")[[1]][index]
print(c(accuracy= acc_train, cutoff = cutoff))
# Area under ROC
auc.perf_train = performance(prediction_train, measure = "auc")
auc.perf_train@y.values

# ####### ####### ####### ####### 
# ####### K fold crossvalidation
# ####### ####### ####### ####### 
train$labels <- ifelse(train$types==1, "protest", "noprotest")
train_control <- trainControl(method="cv", number=10, classProbs=T, savePredictions = T)
model <- train(labels~ last1week_max + last2weeks_max, data=train, trControl=train_control, method="bayesglm")
print(model)
roc <- roc(model$pred$obs, model$pred$protest)
roc
s <- plot.roc(roc, col="blue")
s
confusionMatrix(model)

### Plot:

pdf("../Figure_6_7/Figure_6.pdf", width = 13, height=12)
par(mar=c(5.5, 6, 3, 1))
plot((1-roc[[3]]), roc[[2]], type='l', bty='l', xlab="", 
     ylab="", lwd=2.5, col="blue", main="ROC Curve (Model 1)", 
     cex.axis=2.5, cex.lab=2.5, cex.main=2.5)
mtext("False Positive Rate", side=1, line=4, cex=2.5)
mtext("True Positive Rate", side=2, line=4, cex=2.5)
abline(a=0, b= 1)
text(.845,.2,"Out of Sample AUC: 0.93", cex=2)
legend(x = "bottomright", legend = c("Coef. of Variation"), fill = c(4), cex=1.5)
dev.off()

# Make table using:
summary(model)


