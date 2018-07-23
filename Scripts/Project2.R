#reading the last saved file
hello <- read.csv("good.csv", header = TRUE)

#normalizing data
hello$Pregnancies <- (hello$Pregnancies - min(hello$Pregnancies))/(max(hello$Pregnancies)-min(hello$Pregnancies))
hello$Glucose <- (hello$Glucose - min(hello$Glucose))/(max(hello$Glucose)-min(hello$Glucose))
hello$BloodPressure <- (hello$BloodPressure - min(hello$BloodPressure))/(max(hello$BloodPressure)-min(hello$BloodPressure))
hello$SkinThickness <- (hello$SkinThickness - min(hello$SkinThickness))/(max(hello$SkinThickness)-min(hello$SkinThickness))
hello$Insulin <- (hello$Insulin - min(hello$Insulin))/(max(hello$Insulin)-min(hello$Insulin))
hello$BMI <- (hello$BMI - min(hello$BMI))/(max(hello$BMI)-min(hello$BMI))
hello$DiabetesPedigreeFunction <- (hello$DiabetesPedigreeFunction - min(hello$DiabetesPedigreeFunction))/(max(hello$DiabetesPedigreeFunction)-min(hello$DiabetesPedigreeFunction))
hello$Age <- (hello$Age - min(hello$Age))/(max(hello$Age)-min(hello$Age))

hello <- hello[,-1]

#splitting to train and test
ind <- sample( 2, nrow(hello), replace = TRUE, prob = c(0.3, 0.7))
test_t<- hello[ind == 1,]
train_t <- hello[ind == 2,]

myFormula <-Outcome ~ Pregnancies + 
  Glucose + BloodPressure + SkinThickness +
  Insulin + BMI + DiabetesPedigreeFunction +
  Age



library(neuralnet)
set.seed(122334)
#creating the best neuralnet
#this "dammi" net was obtained after many hit and trials along with educated guesses
#with hidden layer = 5
dammi <- neuralnet(myFormula, training, hidden = 5, err.fct = "ce",lifesign = "full" ,linear.output = FALSE, rep = 10)
res <- compute(dammi, testing[,-9])

#sorting only the results of prediction from net.result
pred <- res$net.result

#if prob >0.5 '1' else '0'
p1 <- ifelse(pred>0.5,1,0)

#creating table for checking
tab <- table(p1,testing$Outcome)

#making acuuracy, snsitivity etc
confusionMatrix(tab)
