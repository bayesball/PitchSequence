prob_transitions <- function(dt, player = "",
                             pitcher = FALSE){
  if(nchar(player) > 0){
    if(pitcher == TRUE){
      dt <- filter(dt, PIT_ID == player)
      } else {
        dt <- filter(dt, BAT_ID == player)
      }
  }
  dt %>%
    group_by(b_count, e_count) %>%
    summarize(Y = n(),
              .groups = "drop") -> summ

  dt %>%
    group_by(b_count) %>%
    summarize(N = n(),
              .groups = "drop") -> summ0

  inner_join(summ, summ0, by = "b_count")  %>%
    mutate(P = Y / N) %>%
    select(b_count, e_count, N, Y, P)
}
