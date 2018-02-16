library("ISLR")
data(Auto)

str(Auto)
head(Auto)

attach(Auto)

par(mar=c(3,3,1,1), mgp=c(2,0.7,0), lwd=2)
plot(mpg~horsepower)

fit.1 <- lm(mpg~horsepower, data=Auto)
fit.2 <- lm(mpg~poly(horsepower,2), data=Auto)
fit.3 <- lm(mpg~poly(horsepower,3), data=Auto)
summary(fit.3)


par(mar=c(3,3,1,1), mgp=c(2,0.7,0), lwd=2)
plot(mpg~horsepower)
hp <- seq(min(horsepower), max(horsepower), length.out=100)

lines(hp, predict(fit.1, data.frame("horsepower"=hp)), col="red")
lines(hp, predict(fit.2, data.frame("horsepower"=hp)), col="green")
lines(hp, predict(fit.3, data.frame("horsepower"=hp)), col="blue")

library(boot)
# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=Auto, statistic=bs, 
                R=1000, formula=mpg~horsepower)

# view results
results
confint(results)
boot.ci(results, type="bca")
plot(results, index=1) # intercept 
plot(results, index=2) # horsepower

library("caret")

train.ctrl = trainControl(method="cv", number=10,
                          savePredictions=T, verboseIter=F, allowParallel=T)


set.seed(1234)
fit.glm <- train(mpg~., data=Auto[,1:6],  method="glm", trControl=train.ctrl)
fit.glmnet <- train(Auto[,2:6], Auto[,1],  method="glmnet", trControl=train.ctrl)
fit.knn <- train(Auto[,2:6], Auto[,1],  method="knn", trControl=train.ctrl)
fit.svm <- train(Auto[,2:6], Auto[,1],  method="svmLinear", trControl=train.ctrl)

fit.resamples <- resamples(list("GLM"=fit.glm, "GLMNET"=fit.glmnet, "KNN"=fit.knn, "SVM"=fit.svm))
summary(fit.resamples)
bwplot(fit.resamples, metric="RMSE")
bwplot(fit.resamples, metric="Rsquared")
ggplot(fit.knn)


