# import necessary libary
library(MASS)
library(dplyr)
library(caret)
library (ggplot2)
library(rpart)
library(e1071)
library (leaps)

# download and extract dataset from source
link <- "http://www.dcc.fc.up.pt/~ltorgo/Regression/cal_housing.tgz"
download.file(link, destfile = "~/data/cal_housing.tgz")
untar("cal_housing.gz")
cal_housing <- read.csv("~data/CaliforniaHousing/cal_housing.data")

# explore and visualize data
head(cal_housing)
dim(cal_housing)
str(cal_housing)
summary(cal_housing) #very useful

levels(cal_housing$ocean_proximity) #categorical var

# plot numerical vars
cal_housing_num <- subset(cal_housing, 
                          select = -c(ocean_proximity))
par(mfrow= c(3,3))
invisible(lapply(names(cal_housing_num), 
                 function(col_name) truehist(cal_housing_num[,col_name], 
                                             main = paste("Histogram of ", col_name), 
                                             xlab = NA)))

# scatter plot with ggplots
g <- ggplot(cal_housing, aes(x = longitude, y = latitude, colour = median_income))
g + geom_point() + scale_color_distiller(palette = "Spectral") +
  labs(title = "Plot of data points by location and median_income") + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"))

# correlation
cor(subset(cal_housing, select = -c(ocean_proximity)),
    use = "pairwise.complete.obs")[,"median_house_value"]

#### Data Wrangling #####
#########################

# dealing wiht missing data: 3 options
cal_housing <- subset(cal_housing, 
                      select = -c(total_bedroom)) #delete column

cal_housing <- cal_housing[complete.cases(cal_housing),] #remove missing entries

cal_housing$total_bedrooms[is.na(cal_housing$total_bedrooms)] <- median(cal_housing$total_bedrooms, na.rm=TRUE)#impute NAs with a good statistics (eg: median)

# other cleaning tasks
cal_housing <- cal_housing %>%
  filter(median_house_value < 500000) %>%
  mutate(rooms_per_house = total_rooms / households) %>%
  mutate(population_per_house = population / households) %>%
  mutate(ocean_proximity = as.factor(ocean_proximity)) %>%
  mutate_at(vars(-ocean_proximity, -median_house_value, -median_income), funs(scale)) %>%
  data.matrix %>% data.frame


#### Split to training set and test set ####
############################################

# random sampling
set.seed(365)
train_id <- sample(nrow(cal_housing), size = 0.8*nrow(cal_housing))
train_set <- cal_housing[train_id,]
test_set <- cal_housing[-train_id,]
print (paste(nrow(train_set), "train +", nrow(test_set), "test"))

# stratified sampling
par(mfrow = c(1,2))
truehist(cal_housing[,"median_income"], main = paste("Histogram of median income"), xlab = NA)
cal_housing <- cal_housing %>% #categorize median income
  mutate(income_level = ceiling(median_income/2)) %>%
  mutate(income_level = factor(ifelse(income_level >= 5, 5, income_level))) %>%
  select(-median_income)
plot(cal_housing$income_level, main = paste("Bar plot of income level"), xlab = NA)

train_str_id <- createDataPartition(cal_housing$income_level, p =.8,
                                    list = FALSE, times = 1)
train_str <- cal_housing[train_str_id,]
test_str <- cal_housing[-train_str_id,]
# test to see if we achieve stratified sampling
table(cal_housing$income_level) / nrow(cal_housing)
table(train_str$income_level) / nrow(train_str)


#compare performance of 2 sampling method
overall<- as.vector(table(cal_housing$income_level) / nrow(cal_housing))
normal_sampling <- factor(sapply(ceiling(test_set$median_income/2), 
                                 function(value) ifelse(value >=5, 5, value))) #sapply automatically returns a list
normal_sampling <- as.vector(table(normal_sampling) / length(normal_sampling))
str_sampling <- as.vector(table(test_str$income_level) / nrow(test_str))
compare <- data.frame(overall, str_sampling, normal_sampling) %>%
  mutate(rand_error = 100*normal_sampling/overall - 100) %>%
  mutate(strat_error = 100*str_sampling/overall-100)

compare

#### Fit models ####
####################

# linear model
model_lm <- lm(median_house_value~., train_str)
summary(model_lm)
predict_lm_train <- predict(model_lm, train_str)
sqrt(mean((train_str$median_house_value - predict_lm_train)^2)) #RMSE


# Decision tree
model_decision_tree <- rpart(median_house_value~.,
                             data = train_str, method = "anova",
                             control = rpart.control(minsplit = 2, cp=0.001))
predict_decision_tree <- predict(model_decision_tree, train_str)
sqrt(mean((train_str$median_house_value - predict_decision_tree)^2)) #RMSE

#SVM
model_svm <- svm(median_house_value~.,
                 data = train_str, cost = 10)
predict_svm <- predict(model_svm, train_str)
sqrt(mean((train_str$median_house_value - predict_svm)^2)) #RMSE

#### 10-fold cross validation:
cal_housing_copy <- cal_housing[sample(nrow(cal_housing)),] # randomly shuffle your data


folds <- cut(seq(1,nrow(cal_housing_copy)),
             breaks=10,labels=FALSE) #Create 10 equally size folds

#Perform 10 fold cross validation
MSE_lm <- 0
MSE_tree <- 0
MSE_svm <- 0

for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- cal_housing_copy[testIndexes, ]
  trainData <- cal_housing_copy[-testIndexes, ]
  
  # fit in the models
  lm_model <- lm(median_house_value~., trainData)
  tree_model <- rpart(median_house_value~.,data = trainData, method = "anova",
                      control = rpart.control(minsplit = 2, cp = 0.001))
  svm_model <- svm(median_house_value~.,data = trainData, cost = 10)
  
  # make predictions
  predict1 <- predict(lm_model, testData)
  predict2 <- predict (tree_model, testData)
  predict3 <- predict(svm_model, testData)
  
  #update MSE
  MSE_lm <- MSE_lm + sum(folds == i)/nrow(cal_housing_copy) * mean((predict1 - testData$median_house_value)^2)
  MSE_tree <- MSE_tree + sum(folds == i)/nrow(cal_housing_copy) * mean((predict2 - testData$median_house_value)^2)
  MSE_svm <- MSE_svm + sum(folds == i)/nrow(cal_housing_copy) * mean((predict3 - testData$median_house_value)^2)
  
}

sqrt(MSE_lm)
sqrt(MSE_tree)
sqrt(MSE_svm)

#### Tuning parameters ####
###########################

# Decision tree:
tune_tree <- tune.rpart(median_house_value~., 
                        data = train_str, minsplit = c(5,10,15, 20), 
                        cp = c(0.1,0.01,0.001,0.0001))
summary(tune_tree)
plot(tune_tree)

best_tree <- tune_tree$best.model
predict_tree <- predict(best_tree, train_str)
sqrt(mean((train_str$median_house_value - predict_tree)^2)) #RMSE of best tree model

# SVM:
tune_svm <- tune.svm(median_house_value ~.,
                     data = train_str,
                     cost=10^(-1:2), gamma=c(0.1,0,1))
summary(tune_svm)
plot(tune_svm)
best_svm <- tune_svm$best.model
predict_svm <- predict (best_svm, train_str)
sqrt(mean((train_str$median_house_value - predict_svm)^2))


#### Applying on test set ####
##############################

predict_tree_final <- predict(best_tree, test_str)
sqrt(mean((test_str$median_house_value - predict_tree_final)^2))

predict_svm_final <- predict(best_svm, test_str)
sqrt(mean((test_str$median_house_value - predict_svm_final)^2))