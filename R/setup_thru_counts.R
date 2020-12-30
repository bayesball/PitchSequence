setup_thru_counts <- function(d){
  # removes all non-pitches from PITCH_SEQ_TX
  d$pseq <- gsub("[.>123N+*]", "", d$PITCH_SEQ_TX)

  # create sequence of b's and s's
  d$pseq <- gsub("[BIPV]", "b", d$pseq)
  d$pseq <- gsub("[CFKLMOQRST]", "s", d$pseq)

  one.string4 <- function(ex){
    # create a vector of individual outcomes
    ex.v <- unlist(strsplit(ex,""))
    # remove last X from vector
    ex.v <- ex.v[-length(ex.v)]
    # compute cumulative total of balls and strikes
    n.balls <- cumsum(ex.v == "b")
    n.strikes <- pmin(cumsum(ex.v == "s"), 2)
    the_count <- paste(n.balls, n.strikes, sep="-")
    possible_counts <- c("0-1", "1-0", "0-2", "1-1",
                         "2-0", "1-2", "2-1", "3-0",
                         "2-2", "3-1", "3-2")
    possible_counts %in% the_count
  }
  # applies this function to all strings

  out <- t(sapply(d$pseq, one.string4))
  dimnames(out)[[2]] <- c("c01", "c10", "c02", "c11",
                          "c20", "c12", "c21", "c30",
                          "c22", "c31", "c32")
  out <- data.frame(pseq = d$pseq, out)
  row.names(out) <- NULL
  out
}
