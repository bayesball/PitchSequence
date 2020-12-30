setup_transitions <- function(d){

  # removes all non-pitches from PITCH_SEQ_TX
  d$pseq <- gsub("[.>123N+*]", "", d$PITCH_SEQ_TX)

  # create sequence of b's and s's
  d$pseq <- gsub("[BIPV]", "b", d$pseq)
  d$pseq <- gsub("[CFKLMOQRST]", "s", d$pseq)

  one.string <- function(ex){
    # replace s and b with X for strikeouts and walks
    ex <- gsub("s$", "X", ex)
    ex <- gsub("b$", "X", ex)
    # create a vector of individual outcomes
    ex.v <- unlist(strsplit(ex,""))
    # remove last X from vector
    ex.v <- ex.v[-length(ex.v)]
    # compute cumulative total of balls and strikes
    n.balls <- cumsum(ex.v == "b")
    n.strikes <- pmin(cumsum(ex.v == "s"), 2)
    # create pitch count variable
    S <- paste(n.balls, n.strikes, sep="-")
    # add a beginning and end outcome
    S <- c("0-0", S, "X")
    # before and after counts
    b_count <- S[1:(length(S) - 1)]
    e_count <- S[-1]
    data.frame(b_count = b_count,
         e_count = e_count)
  }

  map_df(d$pseq, one.string)
}

