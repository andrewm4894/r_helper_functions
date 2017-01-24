##########################################
## FUNCTION TO BUILD LOGISITC REGRESSION
##########################################

build_logreg <- function(model_name,model_formula,train_data,test_data,output_dir,n_steps_back=10) {
  mod_tmp <- glm(model_formula ,family=binomial, data=train_data)
  mod <- stepAIC(mod_tmp, scope="target ~ 1", direction="backward", trace = 1, steps=n_steps_back)
  print('----BELOW VARIABLES REMOVED----')
  print(setdiff(names(mod_tmp$coefficients),names(mod$coefficients)))
  export_summary(model_name,output_dir,mod)
  gc()
  train_probs <- get_predictions(mod,train_data)
  test_probs <- get_predictions(mod,test_data)
  export_prob_density(model_name,output_dir,train_probs,test_probs)
  var_coverage <- data.frame(coverage=t(colwise(get_var_coverage)(train_data)))
  var_coverage$vars <- rownames(var_coverage)
  var_coverage$label <- paste(sep='',var_coverage$var,' (n=',round(var_coverage$coverage,2),')')
  var_coverage$coverage <- NULL  
  mod_odds_ratios <- exp(cbind(coef(mod), confint(mod)))
  tmp <- data.frame(mod_odds_ratios)
  odds<-tmp[-1,]
  names(odds)<-c("OR", "lower", "upper")
  odds$vars<-rownames(odds)
  ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10)) 
  odds <- merge(odds,var_coverage,by=c('vars'),all.x=T)  
  pdf(paste(sep='',output_dir,model_name,'_odds_ratios.pdf'),height = 10, width = 12)
  print(ggplot(odds, aes(y= OR, x = reorder(label, OR))) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    scale_y_log10(breaks=ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(title = paste(sep='',model_name," Odds Ratios & CI's"), x = 'Variables', y = 'OR') +
    theme_bw()
  )
  dev.off()  
  
  return(list(mod,train_probs,test_probs))
}
