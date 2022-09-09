PP3
#H0 X = np
#H1 X != np
#CI = 95%

#Rolling dice
how_many_rolls <- 1000
sim_rolls <- sample(1:6, how_many_rolls, replace = TRUE)

#Number of 6 you get.
if_come_up_6 <- as.numeric(sim_rolls == 6)

#it is  probability when the dice is fair. Since 1/6 = 0.167
np <- how_many_rolls*0.167

#z score
z <- (sum(if_come_up_6)-np)/sqrt(np*(1-0.167))

#Use the Z score table to find values. If the Z value is out of the range between -1.64 to 1.64, the dice is unfair.
pnorm(q=z, lower.tail=FALSE)
