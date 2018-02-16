
# Model building
# install.packages(c("caret", "ggplot2", "pROC", "data.table", "glmnet", "randomForest", "mice"))
library("caret")
library("ggplot2")
library("pROC")
library("data.table")
library("glmnet")
library("randomForest")

# Final prediction (for scoring the output):
final = T

# Set it to the directory with the data
setwd("~/Projects/UW/data/Titanic/")
d <- fread("train.csv", stringsAsFactors=F)

# This is to ensure that we have the same pipeline regardless whether 
# we build final model or 
if (final) {
  d.test <- fread("test.csv", stringsAsFactors=F)
  d.test$Survived <- NA
  d.test <- d.test[,names(d), with=F]
  d <- rbind(d, d.test)
  # y will be the output variable that will also denote 
  # Train / test split
  # Train has label, Test is 'NA'
  d$y.orig = d$Survived
  d$y = d$y.orig
} else {
  # Set seed to make sure that results are reproducible
  set.seed(123)       
  ratio.train = 0.7  # 70% in train
  d$y.orig = d$Survived
  d$y <- ifelse(runif(nrow(d))<ratio.train, d$y.orig, NA)
}

# Basic feature fixing
d[,Embarked:=factor(Embarked)]
# Above is (almost) the same as:
# d$Embarked <- factor(Embarked)
d[,Pclass:=factor(Pclass)]
d[,IsFemale:=as.integer(d$Sex=="female")]

# Check who still has na's
sapply(d, function(x) {sum(is.na(x))})

# Do feature engineering 
# Usually do it in the same data.frame, to reduce errors associated 
# with forgeting to perform the engineering in the same way on test set
#

# Missing value treatment:

# Median substitution for missing Embarked
d$Embarked[d$Embarked==""] <- "S"
d[,Embarked:=factor(d$Embarked)]

# Fare is na in test set
d[,Fare:=ifelse(is.na(Fare), median(Fare, na.rm=T), Fare)]

# MICE for Age
library(mice)
imp.dt <- d[,c("y", "Age", "Fare","Embarked", "IsFemale", "Pclass"), with=F]
imp.m <- matrix(0, 6,6); imp.m[2,2:6] <- 1
Age.imp <- mice(imp.dt, print=F, seed=123, predictorMatrix=imp.m)
summary(Age.imp)

fit <- glm.mids(y~., data=Age.imp, family = binomial)
#summary(fit)
summary(pool(fit))

d[,Age.Mice:=complete(Age.imp)$Age]

# Slide: Basic Model 

# NOTE: This is just 'for fun', before we start doing real modelling
if(!final){
  # Train / test split
  X.col <- c("y", "Pclass", "Age.Mice", "SibSp", "Parch", "Fare", "Embarked", "IsFemale")
  X.train <- d[!is.na(y),X.col, with=F]
  y.train <- d[!is.na(y),y]
  X.test <- d[is.na(y),X.col, with=F]
  y.test <- d[is.na(y),y.orig]
  
  fit <- glm(y~., family=binomial, data=X.train)
  summary(fit)
  
  fit.test.prob <- predict(fit, X.test, type="response")
  fit.test.roc <- roc(y.test, fit.test.prob)
  fit.test.roc
  plot(fit.test.roc)
  table(fit.test.prob>0.5, y.test)
  confusionMatrix(as.integer(fit.test.prob>0.5), y.test, positive="1")
  rm(X.train, X.test, y.train, y.test, X.col, fit.test.roc, fit.test.prob)
}

# Handling categorical variables:

# One-hot encoding of categorical variables and their DRACULA count
# https://blogs.technet.microsoft.com/machinelearning/2015/02/17/big-learning-made-easy-with-counts/
# https://docs.microsoft.com/en-us/azure/machine-learning/studio-module-reference/data-transformation-learning-with-counts
#' @param d - data.table with categorical features
#' @param feature - feature name 
#' @param output - name / position of output column
#' @param onehot.min - minimum number of elements for onehot encoding
#' @param smooth - laplace smoothing value for log-odds
#' @param count - show count column
#' @param logodds - show logodds column
#' @param na.action - what to do, if a feature has NAs
Categorical2Numerical <- function(d, feature, output, onehot.min=10, smooth=10, count=T, logodds=T, na.action=na.fail) {
  stopifnot(length(unique(na.omit(d[[output]])))==2) # Not a binary classifer
  stopifnot(na.omit(unique(d[[output]])) %in% c(0,1)) # Not the right format of the output 
  
  # One-hot encoding
  categories.tbl <- table(d[[feature]])
  categories <- names(categories.tbl)[categories.tbl>=onehot.min]
  for(cat in categories) {
    col=paste0("Is", feature, ".", cat)
    d[,(col):=na.action(as.integer(d[[feature]]==cat))]
  }
  
  d[,OUTPUT.VAL:=d[[output]]]
  # Counts of pos / neg
  if(count) {
    col.count.pos <- paste0(feature,".N1")
    col.count.neg <- paste0(feature,".N0")
    
    count.pos <- d[,sum(OUTPUT.VAL, na.rm=T)]
    count.neg <- d[,sum(1 - OUTPUT.VAL, na.rm=T)]
    
    d[,(col.count.pos):=sum(OUTPUT.VAL, na.rm=T), by=feature]
    d[,(col.count.neg):=sum(1 - OUTPUT.VAL, na.rm=T), by=feature]
  }
  
  # Log Odds
  if (logodds) {
    output.prior <- d[,mean(OUTPUT.VAL, na.rm=T)]
    col <- paste0(feature, ".LogOdds")
    d[,(col):=log(sum(OUTPUT.VAL, na.rm=T) + smooth*output.prior) - log(sum(1 - OUTPUT.VAL, na.rm=T) + smooth*(1 - output.prior)), by=feature]
  }
  
  # Remove temporary column
  d[,OUTPUT.VAL:=NULL]
  
  # Note that in data.tables, the values will be updated without reassignment, because data.table is updated by reference
  invisible(d)
}

Categorical2Numerical(d, "Pclass", "y")
Categorical2Numerical(d, "SibSp", "y")
Categorical2Numerical(d, "Parch", "y")
Categorical2Numerical(d, "Embarked", "y")

# Text processing for Name

d$Name.Title <- gsub("^.*\\s+([A-Z][a-z]+)\\.\\s+.*$", "\\1", d$Name)
Categorical2Numerical(d, "Name.Title", "y")

d$Name.Surname <- gsub("^(.*?),.*", "\\1", d$Name)
d[,Name.Surname.Count:=.N, by=Name.Surname]

d$Name.HasBrackets <- as.integer(grepl("\\(", d$Name))
d$Name.HasQuotes <- as.integer(grepl('"', d$Name))

# Further split Age, also taking care of the missing value
d$AgeCategory <- with(d, ifelse(is.na(Age), "Missing", 
                                ifelse(Age<5, "Toddler",
                                       ifelse(Age<12, "Child", 
                                              ifelse(Age<18, "Teen",
                                                     ifelse(Age<35, "YoungAdult",
                                                            ifelse(Age<60, "Adult", "Senior")))))))
d$Age.IsEstimated <- ifelse(is.na(d$Age), 0, as.integer(d$Age)==d$Age-0.5)
Categorical2Numerical(d, "AgeCategory", "y")


d[,Ticket.Count:=.N, by=Ticket]
# Note: Not creating features from Category, but only FirstLetter.
# This is because Category seems to be too fragmented.
d[,Ticket.Category:=gsub("[^A-Z]", "", Ticket)]
d[,Ticket.FirstLetter:=gsub("^(.).*$", "\\1", Ticket.Category)]
Categorical2Numerical(d, "Ticket.FirstLetter", "y")
d[,Ticket.NChar:=nchar(Ticket)]
Categorical2Numerical(d, "Ticket.NChar", "y", onehot.min=20)


d$Cabin.Category <- factor(gsub("^(.).*$", "\\1", gsub("[^A-Z]", "", d$Cabin)))
Categorical2Numerical(d, "Cabin.Category", "y")
d$Cabin.NChar <- nchar(d$Cabin)

# Have a look at the the data
# But if you want to use freq.stats, copy it from the explore.R file
freq.stats(split(d$Survived, d$Name.Title), xlab="Title")
freq.stats(split(d$Survived, d$Name.Surname.Count), xlab="Surname")
freq.stats(split(d$Survived, d$Name.HasBrackets), xlab="HasBrackets")
freq.stats(split(d$Survived, d$Name.HasQuotes), xlab="HasQuotes")
freq.stats(split(d$Survived, d$Ticket.Count), xlab="Ticket.Count")
freq.stats(split(d$Survived, d$Ticket.NChar), xlab="Ticket.NChar")
freq.stats(split(d$Survived, d$Ticket.Category), xlab="Ticket.Category")
freq.stats(split(d$Survived, d$Ticket.FirstLetter), xlab="Ticket.FirstLetter")
freq.stats(split(d$Survived, d$Cabin.Category), xlab="Cabin.Category")
freq.stats(split(d$Survived, d$Cabin.NChar), xlab="Cabin.NChar")



# Finish splitting train / test
d.class <- sapply(d, class)
X.col <- intersect(names(d)[14:ncol(d)], names(d.class[d.class %in% c("integer", "numeric")]))
X.col <- unique(c(X.col, "Age.Mice", "SibSp", "Parch", "Fare"))

# Note that different data formats of the same dataset are generated
# To have standarized way of putting it into functions
X.train.dt <- d[!is.na(y),X.col, with=F]
X.train.m <- as.matrix(X.train.dt[,-"y"])
X.test.dt <- d[is.na(y),X.col, with=F]
X.test.m <- as.matrix(X.test.dt[,-"y"])

y.train <- d[!is.na(y), y]
y.train.f <- factor(y.train, levels=c(0, 1), labels=c("X0", "X1"))
y.test <- d[is.na(y), y.orig]
y.test.f <- factor(y.test, levels=c(0, 1), labels=c("X0", "X1"))

# Some sanity checks
stopifnot(sum(is.na(X.train.m))==0)
stopifnot(sum(is.na(X.test.m))==0)
stopifnot(sum(is.na(X.train.dt))==0)


#
# Model training using:
# 1) Logistic regression
# 2) Random Forest
train.ctrl = trainControl(method="cv", number=8,
                          classProbs=T, # summaryFunction=twoClassSummary,
                          savePredictions=T, verboseIter=T, allowParallel=T)

# GLM (Lasso / Ridge regression)
tune.grid <- expand.grid(.alpha=0:2/2,.lambda=exp(seq(-7, 2, length.out=10)))
set.seed(1234)
fit.glmnet <- train(X.train.m, y.train.f,  method="glmnet", preProc = c("center", "scale"), metric="Accuracy", trControl=train.ctrl, tuneGrid=tune.grid)
fit.glmnet
varImp(fit.glmnet)
fit.test.prob <- predict(fit.glmnet, X.test.m, type="prob")[,2]

# Random Forest
tune.grid <- expand.grid(.mtry=c(2,4,6,8))
set.seed(1234)
fit.rf <- train(X.train.m, y.train.f,  method="rf", metric="Accuracy", trControl=train.ctrl, tuneGrid=tune.grid)
fit.rf
varImp(fit.rf)
fit.test.prob <- predict(fit.rf, X.test.m, type="prob")[,2]

# Pick the right model for the final submission 
# (or better, submit results of all models)
if(final) {
  prob.threshold = 0.5
  final.dt <- data.table("PassengerId"=d[is.na(y),PassengerId], 
                         "Survived"=as.integer(fit.test.prob>prob.threshold))
  fwrite(final.dt, "final.csv")
} else {
  fit.test.roc <- roc(y.test, fit.test.prob)
  fit.test.roc
  plot(fit.test.roc)
  table(fit.test.prob>0.5, y.test)
  confusionMatrix(as.integer(fit.test.prob>0.5), y.test, positive="1")
}

# TODO: Ensembling
# TODO: Interactions
# TODO: Feature filtering
# TODO: Wrapping methods
# TRY: Check difference in accuracy between train / test
# TRY: Is difference between train / test in ROC as high?
# TRY: Try different split of train / test (e.g. different random seed). 
#      - Is Logistic Regression or Random Forest better?
#      - Is there still high difference in accuracy or ROC between train /test?
# TRY: Different models, like lda, boosted trees
#
