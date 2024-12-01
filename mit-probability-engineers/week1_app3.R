# is there life on mars
# A = probability there is life on Mars
# B = probability our lab says there is life on Mars
# A_c, B_c = the complements... so probability there is no life / probe finds no life

# lfit = "lab finds if there"
# ldfit = "lab doesn't find if there"
# lfint = "lab finds if not there"
# ldfint = "lab doesn't find if not there"
# a = "life is there"

p_lab_finds_life_if_there <- c(.5, .8, .99)
p_lab_finds_life_if_not_there <- .1
p_life_is_there <- .1

# using total probability theorem of last lecture, we can derive B using the complements
# there are two cases: life is there, life is not... what is the probability of the lab in each scenario
# combined with the relative probabilities that there is / is not life
p_lab_finds_life <- p_life_is_there * p_lab_finds_life_if_there + (1 - p_life_is_there) * p_lab_finds_life_if_not_there

# we now have all the ingredients to apply bayes' theorem
# which would give us the odds there is life given a positive result

p_life_there_if_lab_finds_it <- p_life_is_there * (p_lab_finds_life_if_there / p_lab_finds_life)

# 3.2
p_both_positive_if_there <- .4
p_one_positive_if_there <- .25
p_no_positive_if_there <- .1

p_both_positive_if_not_there <- .05
p_one_positive_if_not_there <- .1
p_no_positive_if_not_there <- .75

p_life_is_there <- .1

p_lab_finds_life <- (
  p_life_is_there * p_both_positive_if_there + 
  (1 - p_life_is_there) * p_both_positive_if_not_there +
  p_life_is_there * p_one_positive_if_there + 
  (1 - p_life_is_there) * p_one_positive_if_not_there +
  p_life_is_there * p_no_positive_if_there + 
  (1 - p_life_is_there) * p_no_positive_if_not_there
) / 3
