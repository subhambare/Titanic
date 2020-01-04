


setwd("D:/Kaggle/titanic")
titanic.train = read.csv(file="train.csv", stringsAsFactors = FALSE, header= TRUE)
titanic.test = read.csv(file="test.csv", stringsAsFactors = FALSE, header= TRUE)


titanic.train$IsTrainSet = TRUE

titanic.test$IsTrainSet = FALSE

titanic.test$Survived = NA

titanic.full = rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Embarked=='', "Embarked"] = 'S'

median.age = median(titanic.full$Age, na.rm= TRUE)

titanic.full[is.na(titanic.full$Age), "Age"] = median.age

table(is.na(titanic.full$Fare))

median.fare = median(titanic.full$Fare, na.rm = TRUE)

titanic.full[is.na(titanic.full$Fare),"Fare"] = median.fare

titanic.full$Pclass = as.factor(titanic.full$Pclass)
titanic.full$Sex = as.factor(titanic.full$Sex)
titanic.full$Embarked = as.factor(titanic.full$Embarked)


titanic.train = titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test = titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived = as.factor(titanic.train$Survived)

survived.equation = "Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked"
surviced.formula = as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)

titanic.model = randomForest(formula= surviced.formula, data = titanic.train, ntree= 5, mtry = 3 , nodesize = 0.01 * nrow(titanic.test))

feature.equation = "Pclass + Age + Sex + SibSp + Parch + Fare + Embarked"

Survived = predict(titanic.model, newdata = titanic.test)

PassengerID = titanic.test$PassengerId

output.df = as.data.frame(PassengerID)

output.df$Survived = Survived
write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)
