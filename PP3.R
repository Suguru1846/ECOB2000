PP3
#H0 X = np
#H1 X != np
#CI = 95%

#Rolling dice
how_many_rolls <- 1000
sim_rolls <- sample(1:6, how_many_rolls, replace = TRUE)
if_come_up_6 <- as.numeric(sim_rolls == 6)
np <- how_many_rolls*0.167 #1/6 = 0.167

#z score
(sum(if_come_up_6)-np)/sqrt(np*(1-0.167))

#Dice is unfair if z value is less than 0.05.
