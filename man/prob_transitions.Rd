\name{prob_transitions}
\alias{prob_transitions}

\title{
  Probability Count Transitions
}
\description{
  Constructs the probabilities of all count transition in a plate appearance
}
\usage{
  prob_transitions(dt, player = "", pitcher = FALSE)
}
\arguments{
  \item{dt}{
    Data frame giving beginning and ending counts for each pitch
  }
  \item{player}{
    Retrosheet id code for player
  }
  \item{pitcher}{
    Indicates if the player is a pitcher or batter
  }
}
\value{
  data frame of the samples sizes N, the counts Y, and probabilities P of all count transitions
}
\author{
  Jim Albert
}


