##########################################
## FUNCTION TO GET ROC DATA
##########################################

get_roc_data <- function(model_name,predictions) {
  my_pred <- prediction(predictions[[1]]$p1, predictions[[1]]$target)
  my_perf <- performance(my_pred, measure = "tpr", x.measure = "fpr")
  my_auc <- performance(my_pred, measure = "auc")
  my_auc <- my_auc@y.values[[1]]
  my_roc_data <- data.frame(fpr=unlist(my_perf@x.values),
                            tpr=unlist(my_perf@y.values),
                            model=paste(sep='',model_name,' [AUC:',round(my_auc,2),']'))
  return(my_roc_data)
}
