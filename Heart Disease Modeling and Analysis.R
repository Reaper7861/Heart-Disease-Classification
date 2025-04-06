# Load and view dataset
HeartDisease = read.csv("heart_disease.csv")
View(HeartDisease)


# Preprocessing dataset
colSums(is.na(HeartDisease)) # Data is good 


# Converting categorical features to factors
HeartDisease$sex <- as.factor(HeartDisease$sex)
HeartDisease$cp <- as.factor(HeartDisease$cp)
HeartDisease$fbs <- as.factor(HeartDisease$fbs)
HeartDisease$restecg <- as.factor(HeartDisease$restecg)
HeartDisease$exang <- as.factor(HeartDisease$exang)
HeartDisease$slope <- as.factor(HeartDisease$slope)
HeartDisease$ca <- as.factor(HeartDisease$ca)
HeartDisease$thal <- as.factor(HeartDisease$thal)
HeartDisease$num <- ifelse(HeartDisease$num == 0, 0, 1) # Convert to binary
HeartDisease$num <- as.factor(HeartDisease$num)


# Logistic Regression 
heart.age.fit = glm(num ~ age, family = "binomial", data = HeartDisease) # Significant
summary(heart.age.fit)

heart.sex.fit = glm(num ~ sex, family = "binomial", data = HeartDisease) # Significant
summary(heart.sex.fit)

heart.cp.fit = glm(num ~ cp, family = "binomial", data = HeartDisease) # Significant
summary(heart.cp.fit)

heart.trestbps.fit = glm(num ~ trestbps, family = "binomial", data = HeartDisease) # Significant
summary(heart.trestbps.fit)

heart.chol.fit = glm(num ~ chol, family = "binomial", data = HeartDisease) # Not Significant
summary(heart.chol.fit)

heart.fbs.fit = glm(num ~ fbs, family = "binomial", data = HeartDisease) # Not Significant
summary(heart.fbs.fit)

heart.restecg.fit = glm(num ~ restecg, family = "binomial", data = HeartDisease) # Significant
summary(heart.restecg.fit)

heart.thalach.fit = glm(num ~ thalach, family = "binomial", data = HeartDisease) # Significant
summary(heart.thalach.fit)

heart.exang.fit = glm(num ~ exang, family = "binomial", data = HeartDisease) # Significant
summary(heart.exang.fit)

heart.oldpeak.fit = glm(num ~ oldpeak, family = "binomial", data = HeartDisease) # Significant
summary(heart.oldpeak.fit)

heart.slope.fit = glm(num ~ slope, family = "binomial", data = HeartDisease) # Significant
summary(heart.slope.fit)

heart.ca.fit = glm(num ~ ca, family = "binomial", data = HeartDisease) # Significant
summary(heart.ca.fit)

heart.thal.fit = glm(num ~ thal, family = "binomial", data = HeartDisease) # Not Significant
summary(heart.thal.fit)


heart.fit = glm(num ~ ., family = "binomial", data = HeartDisease) # Sex, cp, trestbps, thalach, oldpeak, and slope are significant
summary(heart.fit)


# Implement step function to get predictors that result in lowest AIC
step(heart.fit)


# Fit a test model without thalach 
heart.fit.test1 = glm(num ~ sex + cp + trestbps + exang
                     + slope + ca + thal, family = "binomial", 
                     data = HeartDisease)
summary(heart.fit.test1)

# Fit a test model without thal 
heart.fit.test2 = glm(num ~ sex + cp + trestbps + exang
                      + slope + ca + thalach, family = "binomial", 
                      data = HeartDisease)
summary(heart.fit.test2)

# Fit a test model without thalach and thal 
heart.fit.test3 = glm(num ~ sex + cp + trestbps + exang
                      + slope + ca, family = "binomial", 
                      data = HeartDisease)
summary(heart.fit.test3)


# Fit model based on predictors chosen by step function
heart.fit.optimal = glm(num ~ sex + cp + trestbps + thalach + exang
                        + slope + ca + thal, family = "binomial", 
                        data = HeartDisease)
summary(heart.fit.optimal)


# Logistic Regression Cross Validation - 80/20 Split
store.errorRate = rep(0, 10)

for(i in 1:10){
  set.seed(i + 100)
  sample = sample.int(n = nrow(HeartDisease), size = floor(.8 * nrow(HeartDisease)), replace = F)
  
  train = HeartDisease[sample,]
  test = HeartDisease[-sample,]
  
  heart.glm.cv = glm(num ~ sex + cp + trestbps + thalach + exang
                     + slope + ca + thal, family = "binomial", 
                     data = train)
  
  heart.glm.pred = predict(heart.glm.cv, newdata = test, type = "response")
  heart.binary = ifelse(heart.glm.pred < 0.5, "0", "1")
  
  conf.mat = table(Predicted = heart.binary, Actual = test$num)
  store.errorRate[i] = (conf.mat[1, 2] + conf.mat[2, 1])/sum(conf.mat)
}

conf.mat # 0 does not have heart disease, 1 has heart disease
store.errorRate
mean(store.errorRate)


# Role of age
heart.age.fit = glm(num ~ age, family = "binomial", data = HeartDisease) # Significant
summary(heart.age.fit)


heart.full = glm(num ~ ., family = "binomial", data = HeartDisease) # Age is not significant
summary(heart.full)


# Prominence of heart disease in males or females
heart.sex = glm(num ~ sex + cp + trestbps + thalach + exang
                   + slope + ca + thal, family = "binomial", 
                   data = HeartDisease)

summary(heart.sex)


sex.split = split(HeartDisease, HeartDisease$sex)
summary(sex.split[[1]]) # Split for females
summary(sex.split[[2]]) # Split for males


# Random Forest
library(randomForest)

set.seed(123)

heart.rf = randomForest(HeartDisease$num ~ ., data = HeartDisease,
                        ntree = 500, mtry = sqrt(13), importance = T)

heart.rf
varImpPlot(heart.rf)


heart.rf.optimal = randomForest(num ~ sex + cp + trestbps + thalach + exang
                                + slope + ca + thal, data = HeartDisease,
                                ntree = 500, mtry = sqrt(8), importance = T)

heart.rf.optimal
varImpPlot(heart.rf.optimal)


# Random Forest Cross Validation - 80/20 Split
store.errorRate = rep(0, 10)

for(i in c(1: 10)){
  set.seed(i + 100)
  
  sample = sample.int(n = nrow(HeartDisease), size = floor(.8 * nrow(HeartDisease)), replace = F)
  
  train = HeartDisease[sample,]
  test = HeartDisease[-sample,]
  
  heart.rf.cv = randomForest(num ~ sex + cp + trestbps + thalach + exang
                             + slope + ca + thal, data = train, ntree = 500, mtry = sqrt(8), importance = T)
  
  heart.rf.pred = predict(heart.rf.cv, newdata = test)
  conf.mat = table(Predicted = heart.rf.pred, Actual = test$num)
  
  store.errorRate[i] = (conf.mat[1, 2] + conf.mat[2, 1])/sum(conf.mat)
}

conf.mat # 0 does not have heart disease, 1 has heart disease
store.errorRate
mean(store.errorRate)