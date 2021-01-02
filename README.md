# PitchSequence

Package contains functions to assist in the exploration of pitch sequences.

For example, suppose you have a Retrosheet data frame d of play events.

The function 

transition_work(d)

creates a list of three components:

p_trans -- data frame of count transitions
p_prob - data frame giving probabilities of all transitions
plot - graph of the count transitions

