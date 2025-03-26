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
HeartDisease$num <- as.factor(HeartDisease$num)

# Logistic Regression 

heart.age.fit = glm(num ~ age, family = "binomial", data = HeartDisease)
summary(heart.age.fit)

heart.sex.fit = glm(num ~ sex, family = "binomial", data = HeartDisease)
summary(heart.sex.fit)

heart.cp.fit = glm(num ~ cp, family = "binomial", data = HeartDisease)
summary(heart.cp.fit)

heart.trestbps.fit = glm(num ~ trestbps, family = "binomial", data = HeartDisease)
summary(heart.trestbps.fit)

heart.chol.fit = glm(num ~ chol, family = "binomial", data = HeartDisease)
summary(heart.chol.fit)

heart.fbs.fit = glm(num ~ fbs, family = "binomial", data = HeartDisease)
summary(heart.fbs.fit)

heart.restecg.fit = glm(num ~ restecg, family = "binomial", data = HeartDisease)
summary(heart.restecg.fit)

heart.thalach.fit = glm(num ~ thalach, family = "binomial", data = HeartDisease)
summary(heart.thalach.fit)

heart.exang.fit = glm(num ~ exang, family = "binomial", data = HeartDisease)
summary(heart.exang.fit)

heart.oldpeak.fit = glm(num ~ oldpeak, family = "binomial", data = HeartDisease)
summary(heart.oldpeak.fit)

heart.slope.fit = glm(num ~ slope, family = "binomial", data = HeartDisease)
summary(heart.slope.fit)

heart.ca.fit = glm(num ~ ca, family = "binomial", data = HeartDisease)
summary(heart.ca.fit)

heart.thal.fit = glm(num ~ thal, family = "binomial", data = HeartDisease)
summary(heart.thal.fit)


heart.fit = glm(num ~ ., family = "binomial", data = HeartDisease)
summary(heart.fit)


step(heart.fit)


heart.fit.optimal = glm(num ~ sex + cp + trestbps + thalach + exang
                        + slope + ca + thal, family = "binomial", 
                        data = HeartDisease)
summary(heart.fit.optimal)





