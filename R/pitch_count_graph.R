pitch_count_graph <- function(d_season){
  d <- data.frame(count=c('0-0', '1-0', '0-1', '2-0',
                        '1-1', '0-2',
                        '3-0', '2-1', '1-2', '3-1', '2-2', '3-2'),
                strikes=c(0, 0, 1, 0, 1, 2, 0, 1, 2, 1, 2, 2),
                balls=c(0, 1, 0, 2, 1, 0, 3, 2, 1, 3, 2, 3),
                N.Pitches=c(0, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5),
                Type=c("Neutral", "Batter", "Pitcher", "Batter",
                       "Pitcher", "Pitcher", "Batter", "Batter",
                       "Pitcher", "Batter", "Pitcher", "Batter"),
                Runs=c(mean(d_season$RUNS.VALUE),
                       mean(filter(d_season, c10==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c01==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c20==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c11==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c02==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c30==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c21==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c12==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c31==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c22==1)$RUNS.VALUE, na.rm=TRUE),
                       mean(filter(d_season, c32==1)$RUNS.VALUE, na.rm=TRUE)))

  ggplot(d, aes(N.Pitches, Runs, label=count)) +
    geom_point() +
    geom_path(data=filter(d, strikes==0),
            aes(N.Pitches, Runs), color="blue") +
    geom_path(data=filter(d, strikes==1),
            aes(N.Pitches, Runs), color="blue") +
    geom_path(data=filter(d, strikes==2),
            aes(N.Pitches, Runs), color="blue") +
    geom_path(data=filter(d, balls==0),
            aes(N.Pitches, Runs), color="blue") +
    geom_path(data=filter(d, balls==1),
            aes(N.Pitches, Runs), color="blue") +
    geom_path(data=filter(d, balls==2),
            aes(N.Pitches, Runs), color="blue") +
    geom_path(data=filter(d, balls==3),
            aes(N.Pitches, Runs), color="blue") +
    xlab("Pitch Number") +
    ylab("Runs Value") +
    ggtitle("") +
    theme_minimal() +
    geom_hline(yintercept=0, color="red") +
    geom_label()
}


