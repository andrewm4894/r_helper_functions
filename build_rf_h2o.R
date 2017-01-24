##########################################
## FUNCTION TO BUILD RANDOM FOREST
##########################################

build_rf_h2o <- function(model_name,train_data,test_data,output_dir,y_var='target',x_vars=names(train_data)) {
  # try to build the model a few times (if fails) in case of network connection issues
  mod <- NULL
  attempt <- 1
  while( is.null(mod) && attempt <= 3 ) {
    attempt <- attempt + 1
    tryCatch(
      mod <- h2o.randomForest(y = y_var, x = x_vars, training_frame = train_data, ntrees = 100, nfolds = 3),
      error = function(e) print(e)
    )
  }   
  export_summary(model_name,output_dir,mod)
  export_model_plot(model_name,output_dir,mod)
  mod_perf <- h2o.performance(mod)
  mod_varimp <- h2o.varimp(mod)
  capture.output(mod_perf,file=paste(sep='',output_dir,model_name,'_perf.txt'))
  capture.output(mod_varimp,file=paste(sep='',output_dir,model_name,'_varimp.txt'))
  mod_varimp_top_n <- sqldf("select * from mod_varimp order by scaled_importance desc limit 25")
  mod_varimp_top_n <- mod_varimp_top_n[order(mod_varimp_top_n$scaled_importance, decreasing = TRUE),]
  mod_varimp_top_5 <- head(mod_varimp_top_n,5)
  mod_varimp_top_10 <- head(mod_varimp_top_n,10)  
  pdf(paste(sep='',output_dir,model_name,'_varimp_plot.pdf'))
  print(ggplot(mod_varimp_top_n, aes(x = reorder(variable,scaled_importance), y = scaled_importance)) + geom_bar(stat = "identity") + coord_flip() + ggtitle("Feature Importance") + ylab("scaled_importance") + xlab("feature"))
  print(ggplot(mod_varimp_top_n, aes(x = reorder(variable,percentage), y = percentage)) + geom_bar(stat = "identity") + coord_flip() + ggtitle("Feature Importance") + ylab("percentage") + xlab("feature"))
  dev.off()  
  # try to get predictions with redundancy for failures
  train_probs <- NULL
  attempt <- 1
  while( is.null(train_probs) && attempt <= 3 ) {
    attempt <- attempt + 1
    tryCatch(
      train_probs <- get_predictions(mod,train_data),
      error = function(e) print(e)
    )
  }
  test_probs <- NULL
  attempt <- 1
  while( is.null(test_probs) && attempt <= 3 ) {
    attempt <- attempt + 1
    tryCatch(
      test_probs <- get_predictions(mod,test_data),
      error = function(e) print(e)
    )
  }
  export_prob_density(model_name,output_dir,train_probs,test_probs)
  return(list(mod,train_probs,test_probs,mod_varimp_top_n,mod_varimp_top_5,mod_varimp_top_10))
}
