prob_transitions <- function(dt){
  dt %>%
    group_by(b_count, e_count) %>%
    summarize(N = n()) -> summ

  dt %>%
    group_by(b_count) %>%
    summarize(T = n()) -> summ0

  inner_join(summ, summ0, by = "b_count")  %>%
    mutate(P = N / T) %>%
    filter(P > 0.01)  %>%
    select(b_count, e_count, P)
}
