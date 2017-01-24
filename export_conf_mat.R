##########################################
## FUNCTION TO EXPORT CONFUSION MATRIX
##########################################

export_conf_mat <- function(model_name,out_path=getwd(),thold) {
  # cat output to a file
  tmp <- get(paste(sep='',model_name,'_test_probs'))
  tmp_conf <- confusionMatrix(data=tmp[[1]]$target_pred,
                                  reference=tmp[[1]]$target,
                                  positive='1',
                                  prevalence = thold)
  capture.output(tmp_conf,file=paste(sep='',out_path,model_name,'_conf_matrix.txt'))
}
