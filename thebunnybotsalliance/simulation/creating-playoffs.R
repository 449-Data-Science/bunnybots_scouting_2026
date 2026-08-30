rm(list = ls())

load("thebunnybotsalliance/simulation/data/values.rda")
load("thebunnybotsalliance/simulation/data/rankings.rda")

# ----- FUNCTIONS ----- 

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

simulate_match <- function(red, blue, values, match, match_string) {
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
        "minor_blue", "major_blue",
        "stocked_rp_red", "baked_rp_red", "dinner_rp_red", 
        "stocked_rp_blue", "baked_rp_blue", "dinner_rp_blue",
        "red_score", "blue_score"
    )
    
    if (is.null(blue)) {
        result <- data.frame(
            match_number = match, match_string, 
            red1 = red[1], red2 = red[2], red3 = red[3],
            blue1 = NA, blue2 = NA, blue3 = NA
        )
        
        blank_tidy <- data.frame(matrix(NA, 1, length(col_vars)))
        names(blank_tidy) <- col_vars
        result <- bind_cols(result, blank_tidy)
        result$red_score <- 1
        result$blue_score <- 0
        return(result)
    }
    
    if (is.null(red)) {
        result <- data.frame(
            match_number = match, match_string, 
            red1 = NA, red2 = NA, red3 = NA,
            blue1 = blue[1], blue2 = blue[2], blue3 = blue[3]
        )
        
        blank_tidy <- data.frame(matrix(NA, 1, length(col_vars)))
        names(blank_tidy) <- col_vars
        result <- bind_cols(result, blank_tidy)
        result$red_score <- 0
        result$blue_score <- 1
        return(result)
    }
    
    result <- data.frame(
        match, match_string, 
        R1 = red[1], R2 = red[2], R3 = red[3],
        B1 = blue[1], B2 = blue[2], B3 = blue[3]
        )
    
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
}

# ----- SETUP -----

num_alliances <- min(8, floor(length(unique(rankings$team_key)) / 3))

alliances <- data.frame(
    alliance = paste0("Alliance ", 1:num_alliances),
    captain = rankings$team_key[1:num_alliances],
    first_pick = rankings$team_key[(num_alliances + 1) : (num_alliances * 2)],
    second_pick = rankings$team_key[(num_alliances * 2 + 1) : (num_alliances * 3)]
)

match_schedule <- list(
    list("Alliance 1", "Alliance 8"),        # M1
    list("Alliance 4", "Alliance 5"),        # M2
    list("Alliance 2", "Alliance 7"),        # M3
    list("Alliance 3", "Alliance 6"),        # M4
    list("Loser of M1", "Loser of M2"),      # M5
    list("Loser of M3", "Loser of M4"),      # M6
    list("Winner of M1", "Winner of M2"),    # M7
    list("Winner of M3", "Winner of M4"),    # M8
    list("Loser of M8", "Winner of M5"),     # M9
    list("Loser of M7", "Winner of M6"),     # M10
    list("Winner of M7", "Winner of M8"),    # M11
    list("Winner of M10", "Winner of M9"),   # M12 / SF
    list("Loser of M11", "Winner of M12")    # M13 / SF
)

match_names <- c(paste0("Playoff Match ", 1:13), paste0("Finals" ), 1:3)

alliances_vector <- alliances |>
    rowwise() |>
    mutate(teams = list(c(captain, first_pick, second_pick))) |>
    pull(teams)

type = c(
    "Winner of M1", "Loser of M1", "Winner of M2", "Loser of M2", 
    "Winner of M3", "Loser of M3", "Winner of M4", "Loser of M4", 
    "Winner of M5", "Loser of M5", "Winner of M6", "Loser of M6", 
    "Winner of M7", "Loser of M7", "Winner of M8", "Loser of M8", 
    "Winner of M9", "Loser of M9", "Winner of M10", "Loser of M10", 
    "Winner of M11", "Loser of M11", "Winner of M12", "Loser of M12", 
    "Winner of M13", "Loser of M13"
)

alliances_vector <- c(alliances_vector, rep(NA, 26))
names(alliances_vector) = c(alliances$alliance, type)

match_num = 1
matches <- NULL

# ----- MAKING QUARTER AND SEMI FINALS  ----- 

for (match in match_schedule) {
    red <- match[[1]]
    blue <- match[[2]]
    red_teams <- unlist(alliances_vector[red])
    blue_teams <- unlist(alliances_vector[blue])
    
    name <- match_names[match_num]
    
    match <- simulate_match(red_teams, blue_teams, values, match_num, name)
    matches <- rbind(matches, match)
    
    red_score <- match$red_score
    blue_score <- match$blue_score
    red_foul <- match$major_red
    blue_foul <- match$major_blue
    
    winners <- ifelse(
        (red_score > blue_score) || (red_score == blue_score && red_foul < blue_foul),
        list(red_teams), list(blue_teams)
        )
    
    losers <- ifelse(
        red_score > blue_score || (red_score == blue_score && red_foul < blue_foul),
        list(blue_teams), list(red_teams)
    )
    
    alliances_vector[paste0("Winner of M", match_num)] <- winners
    alliances_vector[paste0("Loser of M", match_num)] <- losers
    
    match_num = match_num + 1
}

# ----- MAKING FINALS ----- 
red_wins = 0
blue_wins = 0
final_number = 1

while (red_wins < 2 && blue_wins < 2) {
    red <- unlist(alliances_vector["Winner of M11"])
    blue <- unlist(alliances_vector["Winner of M13"])
    
    name <- paste0("Finals ", final_number)
    
    match <- simulate_match(red, blue, values, match_num, name)
    matches <- rbind(matches, match)
    
    red_score <- match$red_score
    blue_score <- match$blue_score
    red_foul <- match$major_red
    blue_foul <- match$major_blue
    
    red_wins <- ifelse(
        (red_score > blue_score) || (red_score == blue_score && red_foul < blue_foul),
        red_wins + 1, red_wins
    )
    
    blue_wins <- ifelse(
        blue_score > red_score || (red_score == blue_score && red_foul > blue_foul),
        blue_wins + 1, blue_wins
    )
    
    final_number = final_number + 1
    match_num = match_num + 1
}

# ----- FINISHING UP -----

playoffs <- matches
save(playoffs, file = "thebunnybotsalliance/simulation/data/playoffs.rda")
save(alliances, file = "thebunnybotsalliance/simulation/data/alliances.rda")
write.csv(playoffs, "thebunnybotsalliance/data/playoffs.csv")
write.csv(playoffs, "thebunnybotsalliance/data/alliances.csv")