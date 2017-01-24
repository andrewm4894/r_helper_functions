##########################################
## FUNCTION NON ZERO COVERAGE
##########################################

# get non zero values coverage for each var
get_var_coverage <- function(x) sum(ifelse(x>0,1,0))/length(x)  
