transition_work <- function(d, title = ""){
  p_trans <- setup_transitions(d)
  p_prob <- prob_transitions(p_trans)
  p <- transition_graph(p_prob, title)
  list(p_trans = p_trans, p_prob = p_prob,
       plot = p)
}
