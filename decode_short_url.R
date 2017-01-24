##########################################
## FUNCTION TO UNSHORTEN URL
##########################################

# function to get final target url from a shortened url
decode_short_url <- function(url, ...) {
  # PACKAGES #
  require(RCurl)
  require(curl)
  
  # LOCAL FUNCTIONS #
  decode <- function(u) {
    x <- try( curl_fetch_memory(u)$url )
    if(inherits(x, 'try-error')) {
      return(u)
    } else {
      return(x)
    }
  }
  
  # MAIN #
  # return decoded URLs
  urls <- c(url, ...)
  l <- lapply(urls, decode)
  names(l) <- urls
  return(l)
}
