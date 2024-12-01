boston_mmi <- data.frame(
  6:12,
  c(.3, .1, .03, .01, .003, .001, .0003),
  c(.00, .01, .03, .1, .2, .5, .9),
  c(.00, .00, .01, .03, .1, .3, .6),
  c(.00, .02, .08, .2, .4, .8, 1)
)
boston_mmi_names <- c("i", "p_event", "pf_bridge", "pf_concrete", "pf_brick")
names(boston_mmi) <- boston_mmi_names

#2.1.a
output_a <- data.frame(
  boston_mmi$i, 
  boston_mmi$p_event,
  boston_mmi$p_event * boston_mmi$pf_bridge,
  boston_mmi$p_event * boston_mmi$pf_concrete,
  boston_mmi$p_event * boston_mmi$pf_brick
)
output_names <- c("i", "p_event", "p_bridge_failure", "p_concrete_failure", "p_brick_failure")
names(output_a) <- output_names

#2.1.b
boston_mmi_retro <- data.frame(
  i,
  p_event,
  c(pf_bridge[1], pf_bridge[1:length(pf_bridge)-1]),
  c(pf_concrete[1], pf_concrete[1:length(pf_concrete)-1]),
  c(pf_brick[1], pf_brick[1:length(pf_brick)-1])
)
names(boston_mmi_retro) <- boston_mmi_names

output_b <- data.frame(
  boston_mmi_retro$i, 
  boston_mmi_retro$p_event,
  boston_mmi_retro$p_event * boston_mmi_retro$pf_bridge,
  boston_mmi_retro$p_event * boston_mmi_retro$pf_concrete,
  boston_mmi_retro$p_event * boston_mmi_retro$pf_brick
)
names(output_b) <- output_names

#2.2
mmi_levels <- data.frame(
  6:12,
  c(.1, .3, .4, .3, .1, 0, 0),
  c(0, .1, .2, .3, .5, .4, .2),
  c(0, 0, .1, .2, .3, .6, .8)
)
names(mmi_levels) <- c("mmi_level", "p_damage1", "p_damage2", "p_damage3")

d_intensity <- data.frame(
  1:3,
  c(.99, .9, .5),
  c(.01, .08, .4),
  c(0, .2, .1)
)
names(d_intensity) <- c("damage_level", "p_no_deaths", "p_some_deaths", "p_lots_of_death")

nuclear_reactor_failure <- data.frame(
  6:12,
  as.matrix(mmi_levels[2:4]) %*% as.matrix(d_intensity[2:4])
)
names(nuclear_reactor_failure) <- c("mmi", names(d_intensity)[2:4])
