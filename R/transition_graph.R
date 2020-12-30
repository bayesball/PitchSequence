transition_graph <- function(S, title = ""){

  d <- data.frame(count=c('0-0', '1-0', '0-1', '2-0',
                          '1-1', '0-2',
                          '3-0', '2-1', '1-2',
                          '3-1', '2-2', '3-2'),
                  strikes=c(0, 0, 1, 0, 1, 2,
                            0, 1, 2, 1, 2, 2),
                  balls=c(0, 1, 0, 2, 1, 0, 3,
                          2, 1, 3, 2, 3),
                  N.Pitches=c(0, 1, 1, 2, 2, 2,
                              3, 3, 3, 4, 4, 5)) %>%
    mutate(Balls_Adv = balls - strikes)

  p <- ggplot(d,
              aes(N.Pitches, Balls_Adv, label=count)) +
    geom_path(data=filter(d, strikes==0),
              aes(N.Pitches, Balls_Adv), color="brown") +
    geom_path(data=filter(d, strikes==1),
              aes(N.Pitches, Balls_Adv), color="brown") +
    geom_path(data=filter(d, strikes==2),
              aes(N.Pitches, Balls_Adv), color="brown") +
    geom_path(data=filter(d, balls==0),
              aes(N.Pitches, Balls_Adv), color="green") +
    geom_path(data=filter(d, balls==1),
              aes(N.Pitches, Balls_Adv), color="green") +
    geom_path(data=filter(d, balls==2),
              aes(N.Pitches, Balls_Adv), color="green") +
    geom_path(data=filter(d, balls==3),
              aes(N.Pitches, Balls_Adv), color="green") +
    xlab("Pitch Number") +
    ylab("") +
    ggtitle("") +
    geom_label(fill = "brown", color = "white",
               size = 6) +
    xlim(0, 5) +
    theme_minimal()

  b <- function(count){
    as.numeric(unlist(str_split(count, "-")))[1]
  }
  s <- function(count){
    as.numeric(unlist(str_split(count, "-")))[2]
  }
  S$b_balls <- sapply(S$b_count, b)
  S$b_strikes <- sapply(S$b_count, s)
  S$e_balls <- sapply(S$e_count, b)
  S$e_strikes <- sapply(S$e_count, s)
  S %>%
    filter(e_count != "X") -> S1

  S %>%
    filter(e_count == "X") -> S2

  p + geom_text(data = S1,
                mapping = aes((b_balls + b_strikes +
                     e_balls + e_strikes) / 2,
                     (b_balls - b_strikes +
                     e_balls - e_strikes) / 2,
                    label = round(P, 2)),
                nudge_y = 0.3,
                color = "red",
                size = 5) +
    increasefont() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle(paste(title,
          "Probabilities of Count Transitions")) +
    centertitle() -> p1

  p1 +
    geom_text(data = filter(S2, b_strikes < 2),
              mapping = aes(b_balls + b_strikes,
                            b_balls - b_strikes,
                            label = round(P, 2)),
              nudge_y = 0.3, color = "blue",
              size = 5,
              fontface = 'bold') +
    geom_text(data = filter(S2, b_strikes == 2),
              mapping = aes(b_balls + b_strikes,
                            b_balls - b_strikes,
                            label = round(P, 2)),
              nudge_y = -0.3, color = "blue",
              fontface = 'bold', size = 5)

}


