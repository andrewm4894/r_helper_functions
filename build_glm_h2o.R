##########################################
## FUNCTION TO BUILD RIDGE/LASSO REGRESSION
##########################################

build_glm_h2o <- function(model_name,train_data,test_data,output_dir,y_var='target',x_vars=names(train_data),p_alpha=0) {
  # try to build the model a few times (if fails) in case of network connection issues
  mod <- NULL
  attempt <- 1
  while( is.null(mod) && attempt <= 3 ) {
    attempt <- attempt + 1
    tryCatch(
      mod <- h2o.glm(y=y_var, x=x_vars, training_frame=train_data, family="binomial", alpha = p_alpha, lambda_search = TRUE),
      error = function(e) print(e)
    )
  }    
  export_summary(model_name,output_dir,mod)  
  mod_coef_path <- h2o.getGLMFullRegularizationPath(mod)
  mod_coef_path_plotdata <- data.frame(mod_coef_path$coefficients)
  mod_coef_path_plotdata <- gather(mod_coef_path_plotdata, var, coef, factor_key=TRUE)
  mod_coef_path_plotdata$lambda <- rep(mod_coef_path$lambdas,length(mod_coef_path$coefficient_names))
  myggplot <- ggplot(mod_coef_path_plotdata, aes(lambda, coef, colour = var)) + geom_path(alpha = 1) + geom_vline(xintercept = mod@model$lambda_best)
  myggplotly <- ggplotly(myggplot)
  htmlwidgets::saveWidget(myggplotly, paste(sep='',output_dir,model_name,'_coef_path.html'))
  mod_coefs <- data.frame(mod@model$coefficients_table)
  mod_coefs$odds_ratio <- exp(mod_coefs$coefficients)
  mod_coefs_top_n <- sqldf("select * from mod_coefs order by abs(coefficients) desc limit 50")
  mod_coefs <- sqldf("select * from mod_coefs order by coefficients desc")
  mod_non_zero_coefs <- sqldf("select * from mod_coefs where abs(coefficients)>0 and names<>'Intercept'")
  pdf(paste(sep='',output_dir,model_name,'_coefs.pdf'), height=11, width=10)
  maxrow = 30
  npages = ceiling(nrow(mod_coefs)/maxrow)
  for (i in 1:npages) {idx = seq(1+((i-1)*maxrow), i*maxrow); grid.newpage(); grid.table(mod_coefs[idx, ])}; 
  dev.off()  
  pdf(paste(sep='',output_dir,model_name,'_top_odds_ratios_plot.pdf'), height=12, width=10)
  print(ggplot(mod_coefs_top_n, aes(x = reorder(names,odds_ratio), y = odds_ratio)) + geom_bar(stat = "identity") + coord_flip() + ggtitle(paste(sep='',model_name," - Top Odds Ratios")) + ylab("odds_ratio") + xlab("feature"))
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
  return(list(mod,train_probs,test_probs,mod_non_zero_coefs))
}
