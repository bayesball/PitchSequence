batter_transition <- function(d, name){
  require(Lahman)
  Names <- unlist(str_split(name, " "))
  pid <- filter(Master, nameFirst == Names[1],
                nameLast == Names[2])$retroID
  p_trans <- setup_transitions(filter(d, BAT_ID == pid))
  p_prob <- prob_transitions(p_trans)
  p <- transition_graph(p_prob, Names[2])
  list(p_trans = p_trans, p_prob = p_prob,
       plot = p)
}
