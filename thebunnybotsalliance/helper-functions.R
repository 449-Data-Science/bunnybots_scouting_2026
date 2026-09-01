library(tidyverse)

process_matches <- function(raw) {
    data <- raw |>
        select(
            match_number, 
            red1, red2, red3, 
            blue1, blue2, blue3, 
            red_score, blue_score
        ) |>
        mutate(
            across(
                c(blue1, blue2, blue3, red1, red2, red3),
                ~ gsub("frc", "", .x)
                )
        ) |>
        rename(
            Match = match_number, 
            `Red 1` = red1, `Red 2` = red2, `Red 3` = red3,
            `Blue 1` = blue1, `Blue 2` = blue2, `Blue 3` = blue3,
            `Red Score` = red_score, `Blue Score` = blue_score
        )
    
    return(data)
}

process_rankings <- function(raw) {
    data <- raw |>
        mutate(
            team_key = gsub("frc", "", team_key),
            ties = matches_played - wins - losses - dq,
            string = paste0(wins, "-", losses, "-", ties)
            ) |>
        select(
            rank, team_key, `Ranking Score`, `Avg Match`, string, 
            dq, matches_played, `Ranking Total`
        ) |>
        rename(
            Rank = rank, Team = team_key, `Record (W-L-T)` = string, 
            DQ = dq, Played = matches_played
        )
}

process_playoffs <- function(raw) {
    data <- raw |>
        select(
            red1, red2, red3, 
            blue1, blue2, blue3, 
            red_score, blue_score
        ) |>
        mutate(
            across(
                c(red1, red2, red3, blue1, blue2, blue3),
                ~ gsub("frc", "", .x)
            )
        ) |>
        rename(
            `Red 1` = red1, `Red 2` = red2, `Red 3` = red3,
            `Blue 1` = blue1, `Blue 2` = blue2, `Blue 3` = blue3,
            `Red Score` = red_score, `Blue Score` = blue_score
        )
    
    num_finals <- length(raw$blue1) - 13
    string <- c(paste("Playoff ", 1:13), paste("Finals", 1:num_finals))
    data$Match <- string
    data <- data |>
        select(Match, everything())
    
    return(data)
}