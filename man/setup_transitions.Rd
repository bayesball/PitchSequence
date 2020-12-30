\name{setup_transitions}
\alias{setup_transitions}

\title{
  Sets Up Data Frame for Pitch Transitions
}
\description{
  Constructs data frame giving beginning and end state for each pitch transition
}
\usage{
  setup_transitions(d)
}
\arguments{
  \item{d}{
    Retrosheet play-by-play data frame
  }
}
\value{
  data frame with variables b_count and e_count giving beginning and end state of each count transition
}
\author{
  Jim Albert
}


