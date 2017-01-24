##########################################
## FUNCTION TO BUILD DECISION TREE
##########################################

build_tree <- function(model_name,train_data,test_data,output_dir) {
  mod <- rpart(target ~ ., data = train_data, method='class')
  mod <- prune(mod, cp= mod$cptable[which.min(mod$cptable[,"xerror"]),"CP"])
  export_summary(model_name,output_dir,mod)
  export_model_plot(model_name,output_dir,mod)
  gc()
  train_probs <- get_predictions(mod,train_data)
  test_probs <- get_predictions(mod,test_data)
  export_prob_density(model_name,output_dir,train_probs,test_probs)
  return(list(mod,train_probs,test_probs))
}
