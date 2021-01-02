balls <- function(count){
  b <- function(ct){
    as.numeric(unlist(str_split(ct, "-"))[1])
  }
  sapply(count, b)
}
