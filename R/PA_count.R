PA_count <- function(d){
  d %>%
    mutate(pseq = str_remove_all(PITCH_SEQ_TX,
                                 "[.>123N+*]"),
           pseq_length = str_length(pseq),
           Count = paste(BALLS_CT, STRIKES_CT,
                         sep="-"))
}

