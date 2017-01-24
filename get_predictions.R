##########################################
## FUNCTION TO GET PREDICTIONS
##########################################

get_predictions <- function(model,df=df_test,thold=pred_thold) {
  if (class(model)[1] == "rpart") {   
    predictions <- as.data.frame(predict(model,df,type="prob"))
    colnames(predictions) <- c("p0","p1")
  } else if (class(model)[1] == "h2o" | class(model)[1] == "H2OBinomialModel" ){ 
    predictions <- as.data.frame(h2o.predict(model,df))
  } else { 
    predictions <- as.data.frame(predict(model,df,type="response"))
    colnames(predictions) <- c("p1")
    predictions$p0 <- 1-predictions$p1
  }
  predictions$target_pred <- ifelse(predictions$p1>pred_thold,1,0)
  predictions$target <- as.vector(df$target)
  return(predictions)
}
