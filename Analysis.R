library(readxl)
newdata <- read_excel("newdata.xlsx", sheet = "Sheet5")

library(DataExplorer)
plot_intro(newdata)

# remove NA
library(tidyverse)
newdata2 <- drop_na(newdata)
plot_intro(newdata2)
str(newdata2)

newdata2$art <- factor(newdata2$art)
newdata2$collectable <- factor(newdata2$collectable)
str(newdata2)

library(tidyverse)
newdata2 <- newdata2 %>% filter(price > 0)
library(skimr)
newdata2 %>%
  skim()
summary(newdata2)


library(ggplot2)
ggplot(newdata2, aes(y = price)) + 
  geom_boxplot() +
  theme_classic()
ggplot(newdata2, aes(x = price)) + 
  geom_histogram() +
  theme_classic()


# remove outliers
boxplot(newdata2$price)
boxplot(newdata2$price)$out
outliers <- boxplot(newdata2$price, plot = FALSE)$out
newdata2 <- newdata2[-which(newdata2$price %in% outliers),]
boxplot(newdata2$price)
hist(newdata2$price)

plot_correlation(newdata2)

newdata2 <- newdata2 %>% mutate(lprice = log(price))

newdata2 %>%
  skim()
summary(newdata2)

# newdata2 <- newdata2 %>% mutate(lprice = (price^0.1414 - 1)/0.1414)

library(GGally)
ggpairs(newdata2, columns = c(10, 1:4))

par(mfrow = c(1, 2))
plot(model1)

hist(newdata2$lprice)
boxplot(newdata2$lprice)

# normality test
library(RcmdrMisc)
normalityTest(newdata2$lprice)
ks.test(newdata2$lprice, mean(newdata2$lprice), sd(newdata2$lprice))


# split train-test data 80:20
newdata2 <- newdata2[, -1]
n <- nrow(newdata2)
n_train <- round(0.8*n)
set.seed(13)
newdata2.split <- sample(1:n, n_train)
newdata2.train <- newdata2[newdata2.split, ]
plot_bar(newdata2.train)
newdata2.test <- newdata2[-newdata2.split, ]
plot_bar(newdata2.test)

# frequency table
library(skimr)
newdata2.train %>% 
  skim()
newdata2.test %>% 
  skim()


library(caret)
# fit model
model1 <- lm(lprice ~ ., data = newdata2.train)
summary(model1)
postResample(obs = exp(newdata2.train$lprice), pred = exp(model1$fitted.values))
pred1 <- predict(model1, newdata2.test)
postResample(obs = exp(newdata2.test$lprice), pred = exp(pred1))
library(lmtest)
bptest(model1)
output <- data.frame(yhat = model1$fitted.values, e = model1$residuals)
new_e <- output[order(output$yhat),]
new_e$group <- c(rep('g1', 486), rep('g2', 486))
leveneTest(new_e$e ~ new_e$group)
var.test(new_e$e ~ new_e$group)
dwtest(model1)
ks.test(model1$residuals, mean(model1$residuals), sd(model1$residuals))
t.test(model1$residuals, mu= 0)
vif(model1)

par(mfrow = c(2, 2))
plot(model1)

model2 <- lm(lprice ~ view + favorite + rarity + hat, data = newdata2.train)
summary(model2)
postResample(obs = exp(newdata2.train$lprice), pred = exp(model2$fitted.values))
pred2 <- predict(model2, newdata2.test)
postResample(obs = exp(newdata2.test$lprice), pred = exp(pred2))
output2 <- data.frame(yhat = model2$fitted.values, e = model2$residuals)
new_e2 <- output[order(output2$yhat),]
new_e2$group <- c(rep('g1', 486), rep('g2', 486))
leveneTest(new_e2$e ~ new_e2$group)
var.test(new_e2$e ~ new_e2$group)
dwtest(model2)
ks.test(model2$residuals, mean(model2$residuals), sd(model2$residuals))
t.test(model2$residuals, mu= 0)
vif(model2)
par(mfrow = c(2, 2))
plot(model2)

model3 <- step(model1, direction = "both")
summary(model3)
postResample(obs = exp(newdata2.train$lprice), pred = exp(model3$fitted.values))
pred3 <- predict(model3, newdata2.test)
postResample(obs = exp(newdata2.test$lprice), pred = exp(pred3))
output3 <- data.frame(yhat = model3$fitted.values, e = model3$residuals)
new_e3 <- output[order(output3$yhat),]
new_e3$group <- c(rep('g1', 486), rep('g2', 486))
leveneTest(new_e3$e ~ new_e3$group)
var.test(new_e3$e ~ new_e3$group)
dwtest(model3)
ks.test(model3$residuals, mean(model3$residuals), sd(model3$residuals))
t.test(model3$residuals, mu= 0)
vif(model3)
par(mfrow = c(2, 2))
plot(model3)
#e_model3 = model3$residuals
#summary(e_model3)

'''
newdata2$background <- ifelse(newdata2$background == "None", 0, 1)
newdata2$accessory <- ifelse(newdata2$accessory == "None", 0, 1)
newdata2$hat <- ifelse(newdata2$hat == "None", 0, 1)
str(newdata2)
newdata2$art <- as.numeric(newdata2$art)
newdata2$collectable <- as.numeric(newdata2$collectable)
str(newdata2)

newdata2.scaled <- as.data.frame(scale(newdata2[,-10]))
min.lprice <- min(newdata2$lprice)
max.lprice <- max(newdata2$lprice)
newdata2.scaled$lprice <- scale(newdata2$lprice
                               , center = min.lprice
                               , scale = max.lprice - min.lprice)

# Train-test split
n_train2 <- round(0.8*nrow(newdata2.scaled))
set.seed(13)
newdata2.split2 <- sample(1:nrow(newdata2.scaled), n_train2)
newdata2.train2 <- newdata2.scaled[newdata2.split, ]
newdata2.test2 <- newdata2.scaled[-newdata2.split, ]

model5 <- lm(lprice ~ ., data = newdata2.train2)
summary(model5)
yhat_m5 <- exp(model5$fitted.values*(max.lprice-min.lprice) + min.lprice)
postResample(obs = exp(newdata2.train$lprice), pred = yhat_m5)
pred5 <- predict(model5, newdata2.test2)
yhat2_m5 <- exp(pred5*(max.lprice-min.lprice) + min.lprice)
postResample(obs = exp(newdata2.test$lprice), pred = yhat2_m5)

par(mfrow = c(2, 2))
plot(model4)
normalityTest(model4$residuals)
'''


# ANN

library(neuralnet)
# The predictor vars must be scaled data for the ANN fitting
newdata3 <- newdata
newdata3 <- drop_na(newdata3)
newdata3 <- newdata3 %>% filter(price > 0)
boxplot(newdata3$price)
boxplot(newdata3$price)$out
outliers <- boxplot(newdata3$price, plot = FALSE)$out
newdata3 <- newdata3[-which(newdata3$price %in% outliers),]
boxplot(newdata3$price)
hist(newdata3$price)

newdata3$background <- ifelse(newdata3$background == "None", 0,1)
newdata3$accessory <- ifelse(newdata3$accessory == "None", 0,1)
newdata3$hat <- ifelse(newdata3$hat == "None", 0,1)

#newdata3 <- newdata3 %>% mutate(lprice = log(price))

newdata3.scaled <- as.data.frame(scale(newdata3[, -1]))
min.price <- min(newdata3$price)
max.price <- max(newdata3$price)
# response var must be scaled to [0 < resp < 1]
newdata3.scaled$price <- scale(newdata3$price
                               , center = min.price
                               , scale = max.price - min.price)

# Train-test split
n_train3 <- round(0.8*nrow(newdata3.scaled))
set.seed(13)
newdata3.split <- sample(1:nrow(newdata3.scaled), n_train3)
newdata3.train <- newdata3.scaled[newdata3.split, ]
newdata3.test <- newdata3.scaled[-newdata3.split, ]

y.train <- newdata3[newdata3.split, ]$price
y.test <- newdata3[-newdata3.split, ]$price
data.frame(y.train, yhat_train1)

# neuralnet doesn't accept resp~. (dot) notation
# so a utility function to create a verbose formula is used
library(nnet)
n <- names(newdata3)
f <- as.formula(paste("price ~", paste(n[!n %in% "price"], collapse = " + ")))
set.seed(13)
nn1 <- neuralnet(f, data = newdata3.train, hidden = 9, stepmax = 10^6, rep = 25, learningrate = 0.01)
#plot(nn1)

pred_train1 <- predict(nn1, newdata3.train)
pred_test1 <- predict(nn1, newdata3.test)
yhat_train1 <- pred_train1*(max.price-min.price) + min.price
yhat_test1 <- pred_test1*(max.price-min.price) + min.price

nn_train1 = postResample(obs = y.train, pred = yhat_train1)
nn_train1
nn_test1 = postResample(obs = y.test, pred = yhat_test1)
nn_test1
round(postResample(obs = y.test, pred = yhat_test1), 4)

f2 <- price ~ view + favorite + rarity + hat
nn2 <- neuralnet(f2, data = newdata3.train, hidden = 1, stepmax = 10^6, rep = 25, learningrate = 0.01)
#plot(nn2)

pred_train2 <- predict(nn2, newdata3.train)
pred_test3 <- predict(nn2, newdata3.test)
yhat_train2 <- pred_train2*(max.price-min.price) + min.price
yhat_test3 <- pred_test3*(max.price-min.price) + min.price

yhat_ann431 <- yhat_test2

nn_train2 = postResample(obs = y.train, pred = yhat_train2)
nn_train2
nn_test2 = postResample(obs = y.test, pred = yhat_test2)
nn_test2


plot(yhat_test1, y.test)
plot(yhat_test2, y.test)





# grid search
tr <- as.matrix(newdata3.train)
ts <- as.matrix(newdata3.test)

library(h2o)
h2o.init(nthreads = -1)
trainHex = as.h2o(tr)
testHex = as.h2o(ts)

predictors <-colnames(tr)[(colnames(tr) %in% c("secondsale","view","favorite","rarity", "art", "collectable", "background", "accessory", "hat"))]

params1 <- list(activation = "Tanh",
                epochs = c(1, 10, 25, 50 ,100),
                rate = c(0.001, 0.01, 0.1),
                hidden = seq(1,15,1))

h2o.rm("dl_grid_random")

grid <- h2o.grid(algorithm = "deeplearning", 
                  x = predictors, 
                  y = "price",
                  grid_id = "dl_grid_random",
                  training_frame = trainHex,
                  validation_frame = testHex,
                  stopping_metric = "RMSE",
                  stopping_tolerance = 1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
                  stopping_rounds = 2,
                  score_validation_samples = 266, ## downsample validation set for faster scoring
                  score_duty_cycle = 0.025,         ## don't score more than 2.5% of the wall time
                  max_w2 = 10,                      ## can help improve stability for Rectifier
                  hyper_params = params1
)                           

grid1 <- h2o.getGrid("dl_grid_random", sort_by = "rmse", decreasing = FALSE)
grid1

grid@summary_table[,4]
grid@summary_table[1:20,]

best_model <- h2o.getModel(grid@model_ids[[1]])
best_model




#==========================================================================
m4 <- lm(price~., data = newdata3.train)
summary(m4)
m5 <- step(m4, direction = 'both')
summary(m5)
pred5 <- predict(m5, newdata3.train[,-ncol(newdata3.train)])
par(mfrow=c(2,2))
plot(m5)
res5 <- m5$residuals
ks.test(res5, mean(res5), sd(res5))

library(lmtest)
bptest(price ~ favorite + rarity + accessory, data = newdata3.train)
t.test(res5, mu= 0)
yhat_train.scale2 <- pred5*(max.price-min.price) + min.price
yhat_test.scale2 <- pred5_test*(max.price-min.price) + min.price

postResample(yhat_train.scale2, y.train)
pred5_test <- predict(m5, newdata3.test[,-ncol(newdata3.test)])
round(postResample(yhat_test.scale2, y.test), 3)

yhat_total <- exp(pred1)
yhat_enter <- exp(pred2)
yhat_stepwise <- exp(pred4)

yhat_ann991 <- yhat_test1
yhat_ann651 <- yhat_test2
yhat_ann411 <- yhat_test3


par(mfrow=c(2,3))
g1 <- plot(y.test, yhat_total)
g2 <- plot(y.test, yhat_enter)
g3 <- plot(y.test, yhat_stepwise)
g4 <- plot(y.test, yhat_ann991)
g5 <- plot(y.test, yhat_ann651)
g6 <- plot(y.test, yhat_ann411)






