##########################################
## FUNCTION TO CHECK IF OBJECT EXISTS
##########################################

# function to check if an object exists and print info
checkExists <- function(obj) {
  if(exists(obj)){
    cat(paste(sep='','[',obj,'] <- EXISTS :)'))}
  else{
    cat(paste(sep='','[',obj,'] <- DOES NOT EXIST :('))}
} 
