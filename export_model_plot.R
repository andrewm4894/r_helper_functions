##########################################
## FUNCTION TO EXPORT MODEL PLOT
##########################################

export_model_plot <- function(model_name,out_path=getwd(),model) {
  # sink output to a file
  pdf(paste(sep='',out_path,model_name,'_plot.pdf'))
  if (class(model)[1] == "rpart") {   
    rpart.plot(model, extra=104, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE)
  } else { 
    plot(model)
  }
  dev.off()
}
