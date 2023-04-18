

library(readr)

cancer <- read.csv("data.csv")
cancer <- cancer[-33]

# *********************************************

# descriptive Analytic

# look at the structure of data and number of variables
str(cancer)
summary(cancer)
levels(cancer$diagnosis)


# change variable type/ tidy the data

cancer$diagnosis <- as.factor(cancer$diagnosis)


# look at the distributions of continuous variables

# install.packages("Hmisc")

cancer_1 <- cancer [3:11]
cancer_2 <- cancer [11:19]
cancer_3<- cancer [20:28]
cancer_4 <- cancer [28:32]

# Draw each column as histogram
library(Hmisc)
hist.data.frame(cancer_1)
hist.data.frame(cancer_2)
hist.data.frame(cancer_3)
hist.data.frame(cancer_4)


hist.data.frame <- function(x, ..., colors=rainbow(ncol(x))) {
  col<-1
  hist<-function(...) {
    graphics::hist(..., col=colors[col])
    col <<- col+1
  }
  f <- Hmisc:::hist.data.frame
  environment(f) <- environment()
  f(x,...)
}

hist(cancer_1)

# ****************************************************
hist.data.frame <- function(x, ..., colors=rainbow(ncol(x))) {
  col<-1
  hist<-function(...) {
    graphics::hist(..., col=colors[col])
    col <<- col+1
  }
  f <- Hmisc:::hist.data.frame
  environment(f) <- environment()
  f(x,...)
}

hist(cancer_2)
# *******************************************************************88

hist.data.frame <- function(x, ..., colors=rainbow(ncol(x))) {
  col<-1
  hist<-function(...) {
    graphics::hist(..., col=colors[col])
    col <<- col+1
  }
  f <- Hmisc:::hist.data.frame
  environment(f) <- environment()
  f(x,...)
}

hist(cancer_3)

# ***************************************************************************

hist.data.frame <- function(x, ..., colors=rainbow(ncol(x))) {
  col<-1
  hist<-function(...) {
    graphics::hist(..., col=colors[col])
    col <<- col+1
  }
  f <- Hmisc:::hist.data.frame
  environment(f) <- environment()
  f(x,...)
}

hist(cancer_4)

# ************************************************************************8

# check for outliers

# boxplot for each attribute on one image

# dev.off()

par(mfrow=c(2,4))
for(i in 1:8) {
  boxplot(cancer_1[,i], main=names(cancer_1)[i], 
          col = "#EE82EE")}

par(mfrow=c(2,4))
for(i in 1:8) {
  boxplot(cancer_2[,i], main=names(cancer_2)[i], 
          col = "#7FFF00")}

par(mfrow=c(2,4))
for(i in 1:8) {
  boxplot(cancer_3[,i], main=names(cancer_3)[i], 
          col = "#00CED1")}

par(mfrow=c(2,4))
for(i in 1:8) {
  boxplot(cancer_4[,i], main=names(cancer_4)[i], 
          col = "#FF8C00")}

# check for missing values

missing_row <- nrow(cancer)- sum(complete.cases(cancer))
missing_row 

missing_coulumn <- unlist(lapply(cancer, function(x) any(is.na(x))))
missing_coulumn



library(visdat)

vis_miss(cancer)


# install.packages("purrr")

library(purrr)

map_int(cancer, function(.x) sum(is.na(.x)))


# check for duplication

sum(duplicated(cancer))


# find the amount of M and B
round(prop.table(table(cancer$diagnosis)), 2)


# find  mutlicollinearity between variables

# dev.off()
df_corr <- cor(cancer %>% select(-id, -diagnosis))
corrplot::corrplot(df_corr, order = "hclust", tl.cex = 1, addrect = 8)

# data visulization for correlation

par(mfrow=c(2,4))
plot(cancer$texture_se,cancer$texture_worst , col = cancer$diagnosis , pch = 19)
plot(cancer$smoothness_mean , cancer$area_se , col = cancer$diagnosis,pch = 19)
plot(cancer$fractal_dimension_worst , cancer$compactness_mean , col = cancer$diagnosis,pch = 19)
plot(cancer$concave.points_se , cancer$concavity_mean , col = cancer$diagnosis,pch = 19)
plot(cancer$fractal_dimension_se,cancer$radius_se , col = cancer$diagnosis , pch = 19)

library(caret)
# The findcorrelation() function from caret package remove highly correlated predictors
# based on whose correlation is above 0.9. This function uses a heuristic algorithm 
# to determine which variable should be removed instead of selecting blindly

cancer_ <- cancer %>% select(findCorrelation(df_corr, cutoff = 0.9))
cancer_= cancer_[-10]

cancer <- cancer[!(cancer %in% cancer_)]


# Splitting dataset

library(caTools)
set.seed(12345)
split <- sample.split(cancer$diagnosis, SplitRatio = 0.8)


train_reg <- subset(cancer, split == "TRUE")
test_reg <- subset(cancer, split == "FALSE")


# feature selection

train_reg$diagnosis <- ifelse(train_reg$diagnosis == "M", 1,0 )
train_reg$diagnosis <- as.factor(train_reg$diagnosis)

test_reg$diagnosis <- ifelse(test_reg$diagnosis == "M", 1,0 )
test_reg$diagnosis <- as.factor(test_reg$diagnosis)


full.model <- glm(diagnosis ~., data = train_reg, family = binomial)
coef(full.model)

library(MASS)
library(magrittr)
library(dplyr)

step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)


# select pair of variables and Feature Scaling

train_set_svm1 = train_reg[c(1,5,6)]
train_set_svm1[-1] = scale(train_set_svm1[-1])


train_set_svm3 = train_reg[c(1,2:3)]
train_set_svm3[-1] = scale(train_set_svm3[-1])

train_set_svm4 = train_reg[c(1,9,10)]
train_set_svm4[-1] = scale(train_set_svm4[-1])


test_set_svm1 = test_reg[c(1,5,6)]
test_set_svm1[-1] = scale(test_set_svm1[-1])


test_set_svm3 = test_reg[c(1,2:3)]
test_set_svm3[-1] = scale(test_set_svm3[-1])

test_set_svm4 = test_reg[c(1,9,10)]
test_set_svm4[-1] = scale(test_set_svm4[-1])

# create SVM models
# install.packages("e1071")

#  visualize the pair features

par(mfrow=c(1,3))
plot(train_set_svm1$concavity_mean ,train_set_svm1$concave.points_mean  , col = train_set_svm1$diagnosis , pch = 19)
plot(train_set_svm3$texture_mean,train_set_svm3$perimeter_mean , col = train_set_svm3$diagnosis,pch = 19)
plot(train_set_svm4$perimeter_se ,train_set_svm4$area_se , col = train_set_svm4$diagnosis,pch = 19)

library(e1071)

# find best cost for model

tune.out1 <- tune(svm, diagnosis ~ concavity_mean + concave.points_mean, data = train_set_svm1 ,type = "C-classification", kernel="radial",
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tune.out2 <- tune(svm, diagnosis ~ texture_mean + perimeter_mean, data = train_set_svm3 ,type = "C-classification", kernel="radial",
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

tune.out3 <- tune(svm, diagnosis ~ perimeter_se + area_se, data = train_set_svm4 ,type = "C-classification", kernel="radial",
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# perform the model

model1 = svm(diagnosis ~ concavity_mean + concave.points_mean, data = train_set_svm1 ,type = "C-classification", kernel="radial",cost = "10")
model3 = svm(diagnosis ~ texture_mean + perimeter_mean, data = train_set_svm3 ,type = "C-classification", kernel="radial",cost = "0.1")
model4 = svm(diagnosis ~ perimeter_se + area_se, data = train_set_svm4 ,type = "C-classification", kernel="radial",cost = "10")



# Visualization

dev.off()
plot(model1 , train_set_svm1)
plot(model3 , train_set_svm3)
plot(model4 , train_set_svm4)

# perform prediction for model 1
p_pred = predict(model1 , newdata = train_set_svm1)
t1 <- table(train_set_svm1$diagnosis , p_pred)

p_pred1 = predict(model1 , newdata = test_set_svm1)
t2 <- table(test_set_svm1$diagnosis , p_pred1)

# perform Confusion Matrix to find accuracy for model 1
library(caret)
confusionMatrix(t1)
confusionMatrix(t2)

# perform prediction for model 2

p_pred3 = predict(model3 , newdata = train_set_svm3)
t5 <- table(train_set_svm3$diagnosis , p_pred3)

p_pred4 = predict(model3 , newdata = test_set_svm3)
t6 <- table(test_set_svm3$diagnosis , p_pred4)


# perform Confusion Matrix to find accuracy for model 2
library(caret)
confusionMatrix(t5)
confusionMatrix(t6)

# perform prediction for model 3
p_pred5 = predict(model4 , newdata = train_set_svm4)
t7 <- table(train_set_svm4$diagnosis , p_pred5)

p_pred6 = predict(model4 , newdata = test_set_svm4)
t8 <- table(test_set_svm4$diagnosis , p_pred6)

# perform Confusion Matrix to find accuracy for model 3

library(caret)
confusionMatrix(t7)
confusionMatrix(t8)
