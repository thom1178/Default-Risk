################################################################################
##################################### Packages #################################
################################################################################


library(ISLR)
library(ROCR)
library(boot) # for cross-validation
library(corrplot)
library(scatterplot3d)
library(tidyverse)
library(plotly)


################################################################################
#################################### Functions #################################
################################################################################
rocplot=function(pred, truth, ...){
  predob = prediction (pred, truth)
  perf = performance (predob , "tpr", "fpr") 
  plot(perf ,...)}


opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

# Function that will compute sensitivity and specificity at any given cutoff
se.sp <- function (cutoff, pred){
  sens <- performance(pred,"sens")
  spec <- performance(pred,"spec")
  num.cutoff <- which.min(abs(sens@x.values[[1]] - cutoff))
  return(list(Cutoff=sens@x.values[[1]][num.cutoff],
              Sensitivity=sens@y.values[[1]][num.cutoff], 
              Specificity=spec@y.values[[1]][num.cutoff]))
}

risk.score <- function(fit.logistic, data){ # 300 to 850
  pred <- predict(fit.logistic, newdata = data, type = "response") #prob default
  
  #Risk score of default: Use Credit score
  p.star <- 1-pred
  return((p.star*550) + 300)
} #Risk/ Credit score

#Odds ratio SE
get.or.se <- function(model) {
  broom::tidy(model) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}
################################################################################
#################################### Data #################################
################################################################################
# This assignment is based on ISLR Chapter 5, exercise 5
head(Default)
dim(Default)
n = dim(Default)[1]  # sample size
sum(is.na(Default))
names(Default)
attach(Default)
#########
# Task 1, EDA: response is 'default'
# Summarize response and relationship between response and the three explanatory variables.
#########

boxplot(balance~default, data = Default) #there is a difference in means
boxplot(income~default, data = Default) #There is not a difference in means
boxplot(balance~student, data = Default) #No  differences in mean
boxplot(income~student, data = Default) #differences in mean

bd1 <- ggplot(Default, aes(x = default, y = balance)) +
  geom_boxplot(alpha=0.7, fill = "forestgreen") +
  scale_y_continuous(name = "Balance") +
  scale_x_discrete(name = "Default") +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 13)) 

bd2 <- ggplot(Default, aes(x = default, y = balance, fill = student)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Balance") +
  scale_x_discrete(name = "Default") +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 13)) +
  scale_fill_brewer(palette = "Accent")


bd3 <- ggplot(Default, aes(x = default, y = income)) +
  geom_boxplot(alpha=0.7, fill = "forestgreen") +
  scale_y_continuous(name = "Income") +
  scale_x_discrete(name = "Default") +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 13)) 

bd4 <- ggplot(Default, aes(x = default, y = income, fill = student)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Income") +
  scale_x_discrete(name = "Default") +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 13)) +
  scale_fill_brewer(palette = "Accent")

bd1
bd2
bd3
bd4



h1 <- hist(balance[default == "Yes"], main = "", breaks = 20)
h2 <-hist(balance[default == "No"], main = "", breaks = 20)

plot(h1, ylim = c(0, 900), col = rgb(0, 0,1, .5), main = "", xlab = "Balance")
plot(h2, add = T, col = rgb(1, 0,0, .5))
legend("right", legend = c("Default", "No Default", "Overlap"),
       fill = c(rgb(0, 0,1, .5), rgb(1, 0,0, .5), rgb(1, 0,1, 1)), bty = "n")
#People who default tend to have a higher balance

h1 <- hist(income[default == "Yes"], main = "", breaks = 20)
h2 <-hist(income[default == "No"], main = "", breaks = 20)

plot(h1, ylim = c(0, 1550), col = rgb(0, 0,1,.5), main = "", xlab = "Income")
plot(h2, add = T, col = rgb(1, 0,0, .5))
legend("topright", legend = c("Default", "No Default", "Overlap"),
       fill = c(rgb(0, 0,1, .5), rgb(1, 0,0, .5), rgb(1, 0,1, 1)), bty = "n")

corrp <- cor(Default[,c(3, 4)])
corrplot.mixed(corrp, lower.col = 'red') #Weak negative correlation

table(default, student)

chisq.test(default, student) #No association/ may not include student in model
#########
# Task 2, Model 1: logistic regression model of default on income and balance
# Perform training/testing evaluation of Model 1
# Suggested measures: 
# - ROC curve
# - Sensitivity and specificity at 0.5 cutoff (prob of default > 0.5)
# - Sensitivity and specificity at an "optimal" cutoff from ROC curve
# Recommendation: Repeat 3 times at different seeds (training/testing split) and 
#   compare sensitivity and specificity at 0.5 cutoff.
#########
################################################################################
###################################### Model 1 #################################
################################################################################

# Split data into training and testing sets
p = 0.5
set.seed(1)  # set the random number generator seed
train = sample(n, p*n)  # random sample percentage out of n; this creates the index list

#Without student
fit_train = glm(default~balance+income, family=binomial(link=logit), 
                data=Default, subset = train)
test_probs = predict.glm(fit_train, Default, type="response")[-train]



#Without Student
rocplot(test_probs, Default[-train,"default"], main="Test Data", colorize = T,
        print.cutoffs.at = seq(0,1, by = .1), text.adj = c(-.2, 1.7))
abline(a=0, b=1)


# Statistics off the ROC
pred = prediction(test_probs, Default[-train,"default"])
# calculating AUC
auc1 <- performance(pred,"auc")
# convert S4 class to vector
auc1 <- unlist(slot(auc1, "y.values"))
auc1 

#[1] 0.9426981 Pretty good
aic1 <- AIC(fit_train)

# Compute optimal cutoff
# Present sensitivity and specificity for that optimal cutoff
roc.perf = performance(pred, measure="tpr", x.measure="fpr")
print(c1 <-opt.cut(roc.perf, pred)) # 0.03517643


se.sp(.5, pred) # Sensitivity and specificity at 0.5 cutoff

table((test_probs >  0.03517643), default[-train]) #Use optimal cutoff

prop.table(table((test_probs >  0.03517643), default[-train]))
#With student
################################################################################
###################################### Model 2 #################################
################################################################################
fit_train.stud = glm(default~balance+income +student, family=binomial(link=logit), 
                data=Default, subset = train)
test_probs.stud = predict.glm(fit_train, Default, type="response")[-train]



#With Student with same seed
rocplot(test_probs.stud, Default[-train,"default"], main="Test Data", colorize = T,
        print.cutoffs.at = seq(0,1, by = .1), text.adj = c(-.2, 1.7))
abline(a=0, b=1)


# Statistics off the ROC
pred = prediction(test_probs.stud, Default[-train,"default"])
# calculating AUC
auc1stud <- performance(pred,"auc")
# convert S4 class to vector
auc1stud <- unlist(slot(auc1stud, "y.values"))
auc1stud 

aic1stud <- AIC(fit_train.stud)

#[1] 0.9426981 Pretty good

# Compute optimal cutoff
# Present sensitivity and specificity for that optimal cutoff
roc.perf = performance(pred, measure="tpr", x.measure="fpr")
print(c1stud <-opt.cut(roc.perf, pred)) # 0.03517643


se.sp(.5, pred) # Sensitivity and specificity at 0.5 cutoff

table((test_probs.stud >  0.03517643), default[-train]) #Use optimal cutoff
#They look very similar
prop.table(table((test_probs.stud >  0.03517643), default[-train])) 

#################################################################################
################################   Same models   ################################
################################     seed = 2    ################################
p = 0.5
set.seed(2)  # set the random number generator seed
train = sample(n, p*n)  # random sample percentage out of n; this creates the index list

#Without student
fit_train = glm(default~balance+income, family=binomial(link=logit), 
                data=Default, subset = train)
test_probs = predict.glm(fit_train, Default, type="response")[-train]



#Without Student
rocplot(test_probs, Default[-train,"default"], main="Test Data", colorize = T,
        print.cutoffs.at = seq(0,1, by = .1), text.adj = c(-.2, 1.7))
abline(a=0, b=1)


# Statistics off the ROC
pred = prediction(test_probs, Default[-train,"default"])
# calculating AUC
auc2 <- performance(pred,"auc")
# convert S4 class to vector
auc2 <- unlist(slot(auc2, "y.values"))
auc2 


#[1] 0.9477989 Almost the same value
aic2 <- AIC(fit_train)

# Compute optimal cutoff
# Present sensitivity and specificity for that optimal cutoff
roc.perf = performance(pred, measure="tpr", x.measure="fpr")
print(c2 <-opt.cut(roc.perf, pred)) 


se.sp(.5, pred) # Sensitivity and specificity at 0.5 cutoff

table((test_probs >   0.03198994), default[-train]) #Use optimal cutoff


#With student
################################################################################
###################################### Model 2 #################################
################################################################################
fit_train.stud = glm(default~balance+income +student, family=binomial(link=logit), 
                     data=Default, subset = train)
test_probs.stud = predict.glm(fit_train, Default, type="response")[-train]



#With Student with same seed
rocplot(test_probs.stud, Default[-train,"default"], main="Test Data", colorize = T,
        print.cutoffs.at = seq(0,1, by = .1), text.adj = c(-.2, 1.7))
abline(a=0, b=1)


# Statistics off the ROC
pred = prediction(test_probs.stud, Default[-train,"default"])
# calculating AUC
auc2stud <- performance(pred,"auc")
# convert S4 class to vector
auc2stud <- unlist(slot(auc2stud, "y.values"))
auc2stud 

#[1] 0.9477989 Pretty good
aic2stud <- AIC(fit_train.stud)

# Compute optimal cutoff
# Present sensitivity and specificity for that optimal cutoff
roc.perf = performance(pred, measure="tpr", x.measure="fpr")
print(c2stud <-opt.cut(roc.perf, pred))# 0.03198994


se.sp(.5, pred) # Sensitivity and specificity at 0.5 cutoff

table((test_probs.stud >  0.03198994), default[-train]) #Use optimal cutoff
#They look very similar to seed 1 



#################################################################################
################################   Same models   ################################
################################     seed = 3    ################################
p = 0.5
set.seed(3)  # set the random number generator seed
train = sample(n, p*n)  # random sample percentage out of n; this creates the index list

#Without student
fit_train = glm(default~balance+income, family=binomial(link=logit), 
                data=Default, subset = train)
test_probs = predict.glm(fit_train, Default, type="response")[-train]



#Without Student
rocplot(test_probs, Default[-train,"default"], main="Test Data", colorize = T,
        print.cutoffs.at = seq(0,1, by = .1), text.adj = c(-.2, 1.7))
abline(a=0, b=1)


# Statistics off the ROC
pred = prediction(test_probs, Default[-train,"default"])
# calculating AUC
auc3 <- performance(pred,"auc")
# convert S4 class to vector
auc3 <- unlist(slot(auc3, "y.values"))
auc3 

#[1] 0.9463478 Almost the same value
aic3 <- AIC(fit_train)

# Compute optimal cutoff
# Present sensitivity and specificity for that optimal cutoff
roc.perf = performance(pred, measure="tpr", x.measure="fpr")
print(c3 <-opt.cut(roc.perf, pred))


se.sp(.5, pred) # Sensitivity and specificity at 0.5 cutoff

table((test_probs >   0.03251589), default[-train]) #Use optimal cutoff


#With student
################################################################################
###################################### Model 2 #################################
################################################################################
fit_train.stud = glm(default~balance+income +student, family=binomial(link=logit), 
                     data=Default, subset = train)
test_probs.stud = predict.glm(fit_train, Default, type="response")[-train]



#With Student with same seed = 3
rocplot(test_probs.stud, Default[-train,"default"], main="Test Data", colorize = T,
        print.cutoffs.at = seq(0,1, by = .1), text.adj = c(-.2, 1.7))
abline(a=0, b=1)


# Statistics off the ROC
pred = prediction(test_probs.stud, Default[-train,"default"])
# calculating AUC
auc3stud <- performance(pred,"auc")
# convert S4 class to vector
auc3stud <- unlist(slot(auc3stud, "y.values"))
auc3stud 

#[1] 0.9463478 Pretty good
aic3stud <- AIC(fit_train.stud)

# Compute optimal cutoff
# Present sensitivity and specificity for that optimal cutoff
roc.perf = performance(pred, measure="tpr", x.measure="fpr")
print(c3stud <-opt.cut(roc.perf, pred)) # 0.03251589


se.sp(.5, pred) # Sensitivity and specificity at 0.5 cutoff

table((test_probs.stud >  0.03251589), default[-train]) #Use optimal cutoff
#They look very similar to seed 1 

###########
# Task 4, K-fold cross-validation evaluation of the two models
#  Recommend 10-fold cross-validation, as leave-one-out is slow on this size data set
###########

# First, 10-fold cv on Model 1 (default on balance and income)
fit1 = glm(default~balance+income, family=binomial(link=logit), data=Default)
summary(fit1)
set.seed(17) # set the random number generator seed
# 10-fold cv, compute misclassification rate
# Note that this R function also provides a second component with a bias adjustment
cv.error.10 = cv.glm(Default, fit1, K=10) 
cv.error.10$delta[1] 
#[1] 0.021442

set.seed(17) # set the random number generator seed
# Second, 10-fold cv on Model 2 (default on balance, income, AND student)
fit2 = glm(default~balance+income+student, family=binomial(link=logit), data=Default)
summary(fit2)
cv.error.10.stud = cv.glm(Default, fit2, K=10) 
cv.error.10.stud$delta[1] 
#[1]  0.02137601

# We choose ton
##########
# Task 5, compute a 'risk score' of default from the model you choose, Model 1 or Model 2
#  Consider the package scatterplot3d to present a static 3D plot and/or 
#  rgl with function plot3d for a spinning 3D plot (risk score against balance and income)
##########


set.seed(1)
fit.final <- glm(default~balance+income, family=binomial(link=logit), 
                                  data=Default, subset = train)
creditScore <- risk.score(fit.final, data = Default)

hist(creditScore, col = rgb(0,1,1,.5), xlim = c(300, 950))

scatterplot3d(x = Default$balance, y = Default$income, z = creditScore,
              angle = 30, highlight.3d  = T, type = "p")

p <- plot_ly(Default, x = ~Default$balance, y = ~Default$income, z = ~creditScore
             , color = ~creditScore, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Balance'),
                      yaxis = list(title = 'Income'),
                      zaxis = list(title = 'Credit Score')))
p

#
mean(auc1, auc2, auc3)
mean(auc1stud, auc2stud, auc2stud)

mean(aic1, aic2, aic3)
mean(aic1stud, aic2stud, aic3stud)

mean(c1[3], c2[3], c3[3])
mean(c1stud[3], c2stud[3], c3stud[3])

