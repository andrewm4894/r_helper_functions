##########################################
## FUNCTION TO EXPORT MODEL PROB PLOTS
##########################################

export_prob_density <- function(model_name,out_path=getwd(),train_probs,test_probs) {
  # sink output to a file
  pdf(paste(sep='',out_path,model_name,'_prob_density.pdf'))
  print(ggplot(train_probs, aes(x=p1, fill=target)) + geom_density(alpha=.3) + ggtitle(paste(sep='',model_name," - Probability Density Plots (Train Data)")))
  print(ggplot(test_probs, aes(x=p1, fill=target)) + geom_density(alpha=.3) + ggtitle(paste(sep='',model_name," - Probability Density Plots (Test Data)")))
  dev.off()
}
