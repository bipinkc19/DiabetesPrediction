mydata = read.csv("diabetes.csv", header = TRUE)

#adding column id
mydata$id <- 1:nrow(mydata) 

#rearranging column to 1st id
mydata <- mydata[,c(10,1,2,3,4,5,6,7,8,9)]

#par for matrix stype plots
par(mfrow = c(1,1))

#to color depend on ooutcome
ddd = as.factor(mydata$Outcome)
plot(mydata, col = ddd, pch = 20)

#total no. of rows
nrow(mydata)

#counting all the no. of zeros
nrow(mydata[mydata$Age== 0,])
.....
#seeing the median
summary(mydata)

#seeing md without 0, 2 means column, 1 = row
apply(mydata,2,function(mydata){median(mydata[mydata>0])})

#seeing the standard deviation without 0
apply(mydata,2,function(mydata){sd(mydata[mydata>0])})


#seeing 1d plot
library(lattice)
hist(mydata$Glucose, main = "Glucose levels", col = "GReen")

#seing the value of outcome when glucose = 0
mydata[mydata$Glucose == 0, ]

#copying new mydata copy
mydata1 <- mydata
mydata1[mydata1$Glucose == 0, "Glucose"] <- 117
nrow(mydata1[mydata1$Glucose==0,])

#histogram plot of BloodPressure
hist(mydata1$BloodPressure, main = "BloodPresure level", col = "green")

#seeing the outcomes when bp is 0
mydata_b0 <- mydata1[mydata1$BloodPressure == 0, ]
fam = as.factor(mydata_b0$Outcome)
plot(mydata_b0, pch = 20, col = fam)

#seeing the corelation
cor(mydata, y = NULL, use = "everything",
    method = c("pearson"))

#md value of blood pressure when outcome is 1 or 0
temp1 <- mydata[mydata$Outcome == 1 & mydata$BloodPressure>0, "BloodPressure"]
median.default(temp1)

temp2 <- mydata[mydata$Outcome == 0 & mydata$BloodPressure>0, "BloodPressure"]
median.default(temp2)

#replacing bp = 0 with above outcome
mydata1[mydata1$BloodPressure == 0 & mydata1$Outcome == 1,"BloodPressure"] <- 74.5 
mydata1[mydata1$BloodPressure == 0 & mydata1$Outcome == 0,"BloodPressure"] <- 70 

#replacing bmi with median of bmi 32
mydata1[mydata1$BMI == 0, "BMI"] <- 32

#write the csv file
write.csv(mydata1, file = "mydata1.csv", row.names = FALSE)

#histogram for attribute = SkinThickness
hist(mydata1$SkinThickness, main = "SkinThickness", col = "green", labels = TRUE)

#creating a seperate d.f. without skinthickness > 1 
#to constrrruct a decision tree
mydata_tree <- mydata1[mydata1$SkinThickness > 1,]

#settting a seed to get recurrent results::##also install package party
set.seed(1234)

#splitting to two parts with 70:30 and changing... ratio for train and test
ind <- sample( 2, nrow(mydata_tree), replace = TRUE, prob = c(0.6, 0.4) )
testData <- mydata_tree[ind == 2,]
trainData <- mydata_tree[ind == 1,]

#active library party
library(party)

#creating a formula for the tree
Formula.tree <- SkinThickness ~  Outcome + Insulin + BloodPressure +Glucose + Pregnancies + BMI +  DiabetesPedigreeFunction + Age 
as.formula(Formula.tree)

#creating a tree based on formula
tree_model <- randomForest(Formula.tree, data = trainData)
#tree_model <- ctree(Formula.tree, data = trainData)

#seeing the importace of attributes
importance <- varImp(tree_model, scale=TRUE)
print(importance)

        
#real
resulta <- trainData$SkinThickness

#predicted of dataTrain
resultb <- predict(tree_model)

#take error and take error avg, sd, min, max for all allocations of rain 
#and test data like 40:60, 50:50,....90:10.
resultc <- data.frame(resulta, resultb)
summary(resulta - resultb)
sd(resulta - resultb)

#plotting for predicted and actual
plot(resulta)
par(new = TRUE)
plot(resultb, col = "red")

#plotting tree
#rpart.plot(tree_model)
plot(tree_model, type = "simple")

#testing in test data
test_res <- predict(tree_model, newdata = testData)
temp <- data.frame(testData$SkinThickness,test_res)
er <- temp[,c(1)]-temp[,c(2)]
summary(er)
sd(er)
plot(test_res, col = "red")
par(new = TRUE)
plot(testData$SkinThickness)

#backup
mydata_sp <- mydata1

#copying thickness 0 to mydata2
mydata2 <- mydata1[mydata1$SkinThickness == 0,]

#deleting mydata1 of skin thick = 0
mydata1 <- mydata1[!(mydata1$SkinThickness == 0),]

#predicting
mydata2$SkinThickness = predict(tree_model, newdata = mydata2)

#binding
siri <- rbind(mydata1, mydata2)


write.csv(siri, file = "cbr.csv", row.names = FALSE)

temp = read.csv(file = "cbr.csv", header = TRUE)

#copying non zero insulin to mydata_ins
mydata_ins <- temp[temp$Insulin >0, ]

#splitting
ind <- sample( 2, nrow(mydata_ins), replace = TRUE, prob = c(0.7, 0.3) )
testData <- mydata_ins[ind == 2,]
trainData <- mydata_ins[ind == 1,]

set.seed(1234)

#formula
insFormula <- Insulin ~  Outcome + Glucose  + BMI +  DiabetesPedigreeFunction + Age 
as.formula(insFormula)

#tree
tree_model1 <- ctree(insFormula , data = trainData, controls = ctree_control(
  remove_weights = TRUE, testtype = "MonteCarlo",maxdepth = 0,mincriterion = 0.95, minsplit = 5, minbucket = 40
))

#plot tree
plot(tree_model1)

#cheching
res <- testData[,"Insulin"]
res1 <- predict(tree_model1, newdata = testData)
 summary(res - res1)
 sd(res - res1)
par(mfrow = c(1,2)) 
plot(res, ylim = c(0,900))
plot((res1), col = "Red", ylim = c(0,900)

varImpPlot(tree_model1)

#for knn
myformula <-Insulin ~ Pregnancies +  Glucose +  BloodPressure + SkinThickness + BMI +DiabetesPedigreeFunction+Age
as.formula(myformula)

siri <- read.csv(file = "cbr.csv", header = TRUE)

apple <- siri[siri$Insulin > 0,]
ind <- sample( 2, nrow(apple), replace = TRUE, prob = c(0.5, 0.5) )
test <- apple[ind == 2,]
train <- apple[ind == 1,]
t1<-test

test1 <- siri[siri$Insulin == 0,]

#normalization
test <- data.Normalization (test, type="n1",normalization="Insulin")
train <- data.Normalization (train, type="n1",normalization="Insulin")

#dmwr package lets yyou normalize and knn at same time
library(DMwR)
set.seed(1234)
new <- kNN(myformula, train[,-5], test[,-5], norm = FALSE, k = 5)

#replacing value
##train$Insulin <- new


#plotting 
par(mfrow = c(1,2))
plot(t1$Insulin, ylim = c(0,800),xlim = c(0,250), main = "actual")
plot(as.numeric(new), ylim = c(0,800),xlim = c(0,250), col = "red", main = "predicted")

#turning to levels for naive bayesian prediction
train$Pregnancies <- cut(train$Pregnancies, c(0,1,2,3,4,5,7,20))
train$Glucose <- cut(train$Glucose, c(0,50,70,72,108,120,150,180,215,250,280,315,1000))
train$BloodPressure <- cut(train$BloodPressure, c(0,80,89,99,100,110))
train$SkinThickness <- cut(train$SkinThickness, c(0,20,40,60,80,100))
train$Insulin <- cut(train$Insulin, c(0,40,60,80,120,180,250,400,600))
train$BMI <- cut(train$BMI, c(0,18.5,25,30,35,40,100))
train$DiabetesPedigreeFunction <- cut(train$DiabetesPedigreeFunction, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,1.2,1.3,1.5,1.7,2.5))
train$Age <- cut(train$Age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,80))
train$Outcome <- cut(train$Outcome, c(0,1))

test$Pregnancies <- cut(test$Pregnancies, c(0,1,2,3,4,5,7,20))
test$Glucose <- cut(test$Glucose, c(0,50,70,72,108,120,150,180,215,250,280,315,1000))
test$BloodPressure <- cut(test$BloodPressure, c(0,80,89,99,100,110))
test$SkinThickness <- cut(test$SkinThickness, c(0,20,40,60,80,100))
test$Insulin <- cut(test$Insulin, c(0,40,60,80,120,180,250,400,600))
test$BMI <- cut(test$BMI, c(0,18.5,25,30,35,40,100))
test$DiabetesPedigreeFunction <- cut(test$DiabetesPedigreeFunction, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,1.2,1.3,1.5,1.7,2.5))
test$Age <- cut(test$Age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,80))
test$Outcome <- cut(test$Outcome, c(0,1))


test1$Pregnancies <- cut(test1$Pregnancies, c(0,1,2,3,4,5,7,20))
test1$Glucose <- cut(test1$Glucose, c(0,50,70,72,108,120,150,180,215,250,280,315,1000))
test1$BloodPressure <- cut(test1$BloodPressure, c(0,80,89,99,100,110))
test1$SkinThickness <- cut(test1$SkinThickness, c(0,20,40,60,80,100))
test1$Insulin <- cut(test1$Insulin, c(0,40,60,80,120,180,250,400,600))
test1$BMI <- cut(test1$BMI, c(0,18.5,25,30,35,40,100))
test1$DiabetesPedigreeFunction <- cut(test1$DiabetesPedigreeFunction, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,1.2,1.3,1.5,1.7,2.5))
test1$Age <- cut(test1$Age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,80))
test1$Outcome <- cut(test1$Outcome, c(0,1))


#using naive baysian....type = raw value...type = class ...class
library(e1071)
model_1 <- naiveBayes(Insulin ~ Pregnancies + Glucose + BloodPressure
                    + SkinThickness + BMI + DiabetesPedigreeFunction + 
                      Age + Outcome, data = train )
ans <- predict(model_1, test, type = "class")

#checking
par(mfrow = c(1,2))
plot(as.numeric(test$Insulin),ylab = "Insulin_factors", xlab ="Index", main = "actual")
plot(as.numeric(ans), xlab ="Index", ,ylab = "Insulin_factors",, main = "predicted", col = "red")


new_ans <- predict(model_1, test1, type = "class")
test1$Insulin <- new_ans

apple$Pregnancies <- cut(apple$Pregnancies, c(0,1,2,3,4,5,7,20))
apple$Glucose <- cut(apple$Glucose, c(0,50,70,72,108,120,150,180,215,250,280,315,1000))
apple$BloodPressure <- cut(apple$BloodPressure, c(0,80,89,99,100,110))
apple$SkinThickness <- cut(apple$SkinThickness, c(0,20,40,60,80,100))
apple$Insulin <- cut(apple$Insulin, c(0,40,60,80,120,180,250,400,600))
apple$BMI <- cut(apple$BMI, c(0,18.5,25,30,35,40,100))
apple$DiabetesPedigreeFunction <- cut(apple$DiabetesPedigreeFunction, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,1.2,1.3,1.5,1.7,2.5))
apple$Age <- cut(apple$Age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,80))
apple$Outcome <- cut(apple$Outcome, c(0,1))

#binding
good <- rbind(test1, apple)



#writing
write.csv(siri, file = "good.csv")

#remaining commands are in project2.r









