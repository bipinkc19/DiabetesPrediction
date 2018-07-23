library(neuralnet)
#final[1,"Pregnancies"] <- ()/17
#final[1,"Glucose"] <-(-11)/155
#final[1,"BloodPressure"] <- (-24)/98
#final[1,"SkinThickness"] <- (-7)/92
#final[1,"Insulin"] <- (-25)/846

#final[1,"BMI"] <- (i-18)/51
#final[1,"DiabetesPedigreeFunction"] <- (-0.01)/2.35
#final[1,"Age"] <- (-21)/60

final <- hello[,-1]



shinyServer(function(input, output) {
  preds <- reactive( {
    final[1,"Pregnancies"] <- (input$Pregnancies)/17
    final[1,"Glucose"] <-(input$Glucose-11)/155
    final[1,"BloodPressure"] <- (input$BloodPressure-24)/98
    final[1,"SkinThickness"] <- (input$SkinThickness-7)/92
    final[1,"Insulin"] <- (input$Insulin-25)/846
                           
    final[1,"BMI"] <- (input$BMI-18)/51
    final[1,"DiabetesPedigreeFunction"] <- (input$DiabetesPedigreeFunction-0.01)/2.35
    final[1,"Age"] <- (input$Age-21)/60
    
    res <- compute(hid_9, final[1,-9],rep=9)
   ans <- res$net.result
   wow <- ans[1,]
   
    return((wow*100))
    
  })
  output$test2<- renderText({tmp <- preds()
 return(paste(tmp, '%',sep=''))})
  #output$test2<- renderText(input$Pregnancies)
  
  output$test<- renderText({tmp <- preds()
  if (tmp >= 50){
    tmp="Will suffer from diabetes"
  }else{tmp="Will not suffer from diabetes"}
  return(tmp)
  
  })
  
})
