##########################################
## FUNCTION TO EXPORT MODEL SUMMARY
##########################################

export_summary <- function(model_name,out_path=getwd(),model) {
  # some models might have extra interesting info
  # for all others just print the summary() output
  if (class(model)[1] == "rpart") {   
    capture.output(print(summary(model)),file=paste(sep='',out_path,model_name,'_summary.txt'))
    capture.output(print(printcp(model)),file=paste(sep='',out_path,model_name,'_summary.txt'),append=TRUE)
  } else { 
    capture.output(print(summary(model)),file=paste(sep='',out_path,model_name,'_summary.txt'))
  }
}
