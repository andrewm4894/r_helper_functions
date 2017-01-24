##########################################
## FUNCTION TO TRY QUERY NTIMES
##########################################

query_exec_retry <- function(p_query,p_project,p_max_pages=Inf,n_retry=3) {
  my_output <- NULL
  attempt <- 1
  while( is.null(my_output) && attempt <= n_retry ) {
    attempt <- attempt + 1
    tryCatch(
      my_output <- query_exec(p_query, p_project, max_pages=p_max_pages),
      error = function(e) print(e)
    )
  }
  return(my_output)
}
