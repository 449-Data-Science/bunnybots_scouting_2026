rm(list = ls())

library(tidyverse)

load("thebunnybotsalliance/simulation/data/values.rda")
load("thebunnybotsalliance/simulation/data/schedule.rda")

col_vars <- c(
    "taxi_R1", "taxi_R2", "taxi_R3",
    "carrot_oven_red", "carrot_L1_red", "carrot_L2_red", "carrot_L3_red",
    "cake_oven_red", "cake_L1_red", "cake_L2_red", "cake_L3_red",
    "park_R1", "park_R2", "park_R3", 
    "carrot_haul_R1", "carrot_haul_R2", "carrot_haul_R3", 
    "cake_haul_R1", "cake_haul_R2", "cake_haul_R3", 
    "minor_red", "major_red",
    "taxi_B1", "taxi_B2", "taxi_B3",
    "carrot_oven_blue", "carrot_L1_blue", "carrot_L2_blue", "carrot_L3_blue",
    "cake_oven_blue", "cake_L1_blue", "cake_L2_blue", "cake_L3_blue",
    "park_B1", "park_B2", "park_B3", 
    "carrot_haul_B1", "carrot_haul_B2", "carrot_haul_B3", 
    "cake_haul_B1", "cake_haul_B2", "cake_haul_B3", 
    "minor_blue", "major_blue"
)

result <- schedule

nrow <- length(result$match)
ncol <- length(col_vars)
blank_tidy <- data.frame(matrix(NA, nrow, ncol))
names(blank_tidy) <- col_vars
result <- bind_cols(result, blank_tidy)

generate_haul <- function(value, parked) {
    if (!parked) return(list(0, 0))
    
    random_factor <- rnorm(1, mean = 0, sd = 10)
    strength <- max(0, min(100, value + random_factor))
        
    if (strength >= 80) {
        return(list(3, 0))
    } else if (strength >= 60) {
        p_super <- 1 - ((strength - 33) / (66 - 33)) 
        if (runif(1) < p_super) {
            return(list(2, 1))
        } else {
            return(list(3, 0))
        }
    } else {
        num_pieces <- sample(
            c(0, 1, 2), size = 1, 
            prob = exp(c(30 - strength, 60 - strength, strength))
            )
        return(list(num_pieces, 0))
    }
}

result <- result |>
    rowwise() |>
    mutate(
        t_r1 = values[as.character(R1)],
        t_r2 = values[as.character(R2)],
        t_r3 = values[as.character(R3)],
        t_b1 = values[as.character(B1)],
        t_b2 = values[as.character(B2)],
        t_b3 = values[as.character(B3)],
        t_red = ((t_r1 + t_r2 + t_r3) / 300) * 5,
        t_blue = ((t_b1 + t_b2 + t_b3) / 300) * 5,
        
        taxi_R1 = sample(c(TRUE, FALSE), size = 1, prob = c(t_r1 + 50, 100 - t_r1)),
        taxi_R2 = sample(c(TRUE, FALSE), size = 1, prob = c(t_r2 + 50, 100 - t_r2)),
        taxi_R3 = sample(c(TRUE, FALSE), size = 1, prob = c(t_r3 + 50, 100 - t_r3)),
        carrot_oven_red = sample(0:15, size = 1, prob = exp(t_red * 0:15 / 16)),
        carrot_L1_red = sample(0:3, size = 1, prob = exp(t_red * 0:3 / 4)),
        carrot_L2_red = sample(0:3, size = 1, prob = exp(t_red * 0:3 / 4)),
        carrot_L3_red = sample(0:2, size = 1, prob = exp(t_red * 0:2 / 4)),
        cake_oven_red = sample(0:3, size = 1, prob = exp(t_red * 0:3 / 4)),
        cake_L1_red = sample(0:2, size = 1, prob = exp(t_red * 0:2 / 4)),
        cake_L2_red = sample(0:2, size = 1, prob = exp(t_red * 0:2 / 4)),
        cake_L3_red = sample(0:3, size = 1, prob = exp(t_red * 0:3 / 4)),
        park_R1 = sample(c(TRUE, FALSE), size = 1, prob = c(t_r1, 100 - t_r1)),
        park_R2 = sample(c(TRUE, FALSE), size = 1, prob = c(t_r2, 100 - t_r2)),
        park_R3 = sample(c(TRUE, FALSE), size = 1, prob = c(t_r3, 100 - t_r3)),
        
        t_haul_r1 = list(generate_haul(t_r1, park_R1)),
        t_haul_r2 = list(generate_haul(t_r2, park_R2)),
        t_haul_r3 = list(generate_haul(t_r3, park_R3)),
        
        carrot_haul_R1 = t_haul_r1[[1]],
        carrot_haul_R2 = t_haul_r2[[1]],
        carrot_haul_R3 = t_haul_r3[[1]],
        cake_haul_R1 = t_haul_r1[[2]],
        cake_haul_R2 = t_haul_r2[[2]],
        cake_haul_R3 = t_haul_r3[[2]],
        
        minor_red = sample(0:4, size = 1, prob = c(75, 8, 7, 5, 5)),
        major_red = sample(0:3, size = 1, prob = c(85, 10, 4, 1)),
        
        taxi_B1 = sample(c(TRUE, FALSE), size = 1, prob = c(t_b1 + 50, 100 - t_b1)),
        taxi_B2 = sample(c(TRUE, FALSE), size = 1, prob = c(t_b2 + 50, 100 - t_b2)),
        taxi_B3 = sample(c(TRUE, FALSE), size = 1, prob = c(t_b3 + 50, 100 - t_b3)),
        
        carrot_oven_blue = sample(0:15, size = 1, prob = exp(t_blue * 0:15 / 16)),
        carrot_L1_blue = sample(0:3, size = 1, prob = exp(t_blue * 0:3 / 4)),
        carrot_L2_blue = sample(0:3, size = 1, prob = exp(t_blue * 0:3 / 4)),
        carrot_L3_blue = sample(0:2, size = 1, prob = exp(t_blue * 0:2 / 4)),
        cake_oven_blue = sample(0:3, size = 1, prob = exp(t_blue * 0:3 / 4)),
        cake_L1_blue = sample(0:2, size = 1, prob = exp(t_blue * 0:2 / 4)),
        cake_L2_blue = sample(0:2, size = 1, prob = exp(t_blue * 0:2 / 4)),
        cake_L3_blue = sample(0:3, size = 1, prob = exp(t_blue * 0:3 / 4)),
        park_B1 = sample(c(TRUE, FALSE), size = 1, prob = c(t_b1, 100 - t_b1)),
        park_B2 = sample(c(TRUE, FALSE), size = 1, prob = c(t_b2, 100 - t_b2)),
        park_B3 = sample(c(TRUE, FALSE), size = 1, prob = c(t_b3, 100 - t_b3)),
        
        t_haul_b1 = list(generate_haul(t_b1, park_B1)),
        t_haul_b2 = list(generate_haul(t_b2, park_B2)),
        t_haul_b3 = list(generate_haul(t_b3, park_B3)),
        
        carrot_haul_B1 = t_haul_b1[[1]],
        carrot_haul_B2 = t_haul_b2[[1]],
        carrot_haul_B3 = t_haul_b3[[1]],
        cake_haul_B1 = t_haul_b1[[2]],
        cake_haul_B2 = t_haul_b2[[2]],
        cake_haul_B3 = t_haul_b3[[2]],
        
        minor_blue = sample(0:4, size = 1, prob = c(75, 8, 7, 5, 5)),
        major_blue = sample(0:3, size = 1, prob = c(85, 10, 4, 1)),
    ) |>
    select(!starts_with("t_"))

result <- result |>
    mutate(
        stocked_rp_red = ifelse(
            (((carrot_L1_red + cake_L1_red) >= 3) &&
            ((carrot_L2_red + cake_L2_red) >= 3) &&
            ((carrot_L3_red + cake_L3_red) >= 3)),
            TRUE, FALSE
        ),
        baked_rp_red = ifelse(
            (cake_L1_red > 0) && (cake_L2_red > 0) && (cake_L3_red > 0),
            TRUE, FALSE
        ),
        dinner_rp_red = ifelse(
            ((park_R1 + park_R2 + park_R3) * 2 + 
            (carrot_haul_R1 + carrot_haul_R2 + carrot_haul_R3) * 1 + 
            (cake_haul_R1 + cake_haul_R2 + cake_haul_R3) * 3) >= 10,
            TRUE, FALSE
        ),
        
        stocked_rp_blue = ifelse(
            (((carrot_L1_blue + cake_L1_blue) >= 3) &&
            ((carrot_L2_blue + cake_L2_blue) >= 3) &&
            ((carrot_L3_blue + cake_L3_blue) >= 3)),
            TRUE, FALSE
        ),
        baked_rp_blue = ifelse(
            (cake_L1_blue > 0) && (cake_L2_blue > 0) && (cake_L3_blue > 0),
            TRUE, FALSE
        ),
        dinner_rp_blue = ifelse(
            ((park_B1 + park_B2 + park_B3) * 2 + 
            (carrot_haul_B1 + carrot_haul_B2 + carrot_haul_B3) * 1 + 
            (cake_haul_B1 + cake_haul_B2 + cake_haul_B3) * 3) >= 10,
            TRUE, FALSE
        ),
        
        red_score = 
            (taxi_R1 + taxi_R2 + taxi_R3) * 2 + 
            (carrot_oven_red + cake_oven_red) * 2 +
            (carrot_L1_red) * 3 + (carrot_L2_red) * 4 + (carrot_L3_red) * 5 + 
            (cake_L1_red) * 8 + (cake_L2_red) * 10 + (cake_L3_red) * 12 + 
            (park_R1 + park_R2 + park_R3) * 2 +
            (carrot_haul_R1 + carrot_haul_R2 + carrot_haul_R3) * 1 +
            (cake_haul_R1 + cake_haul_R2 + cake_haul_R3) * 3 +
            (minor_blue) * 4 + (major_blue) * 8,
        
        blue_score = 
            (taxi_B1 + taxi_B2 + taxi_B3) * 2 + 
            (carrot_oven_blue + cake_oven_blue) * 2 +
            (carrot_L1_blue) * 3 + (carrot_L2_blue) * 4 + (carrot_L3_blue) * 5 + 
            (cake_L1_blue) * 8 + (cake_L2_blue) * 10 + (cake_L3_blue) * 12 + 
            (park_B1 + park_B2 + park_B3) * 2 +
            (carrot_haul_B1 + carrot_haul_B2 + carrot_haul_B3) * 1 +
            (cake_haul_B1 + cake_haul_B2 + cake_haul_B3) * 3 +
            (minor_red) * 4 + (major_red) * 8,
    )

matches <- result |>
    rename(
        match_number = match,
        red1 = R1, red2 = R2, red3 = R3,
        blue1 = B1, blue2 = B2, blue3 = B3
        )

write.csv(matches, "thebunnybotsalliance/data/matches.csv", row.names = FALSE)
save(result, file = "thebunnybotsalliance/simulation/data/matches.rda")