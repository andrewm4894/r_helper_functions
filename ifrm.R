##########################################
## FUNCTION TO RM IF EXISTS
##########################################

# taken from http://stackoverflow.com/questions/7172568/write-a-function-to-remove-object-if-it-exists
ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}
