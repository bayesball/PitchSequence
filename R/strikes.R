strikes <- function(count){
  b <- function(ct){
    as.numeric(unlist(str_split(ct, "-"))[2])
  }
  sapply(count, b)
}
