rm(list = ls())
library(truncnorm)

set.seed(42) 

num_teams <- 20
teams <- c(
    449, 686, 4821, 422, 1727, 1731, 836, 9072, 4099, 2106, 
    5549, 1629, 1086, 614, 8592, 5338, 4472, 2890, 339, 2910
)

values <- rtruncnorm(20, a = 0, b = 100, mean = 50, sd = 25)
names(values) <- teams

save(values, file = "thebunnybotsalliance/simulation/data/values.rda")