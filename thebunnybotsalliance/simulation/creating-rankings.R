rm(list = ls())

library(tidyverse)

load("thebunnybotsalliance/simulation/data/matches.rda")
load("thebunnybotsalliance/simulation/data/values.rda")

ranking_points <- values
ranking_points[] <- 0

calculate <- function(raw, col, sel_team) {
    raw$matches <- rep(TRUE, length(raw$match))
    
    row <- raw |>
        filter(team == sel_team) |>
        pull(col)
    
    return(sum(row))
}

matches_tidy <- result |>
    pivot_longer(
        cols = c(R1, R2, R3, B1, B2, B3),
        names_to = "robot",
        values_to = "team"
    ) |>
    select(match, robot, team, everything()) |>
    rowwise() |>
    mutate(
        rp = sum(
            ifelse(robot %in% c("R1", "R2", "R3") && stocked_rp_red, 1, 0),
            ifelse(robot %in% c("R1", "R2", "R3") && baked_rp_red, 1, 0),
            ifelse(robot %in% c("R1", "R2", "R3") && dinner_rp_red, 1, 0),
            ifelse(robot %in% c("B1", "B2", "B3") && stocked_rp_blue, 1, 0),
            ifelse(robot %in% c("B1", "B2", "B3") && baked_rp_blue, 1, 0),
            ifelse(robot %in% c("B1", "B2", "B3") && dinner_rp_blue, 1, 0),
            ifelse(robot %in% c("R1", "R2", "R3") && red_score > blue_score, 3, 0),
            ifelse(robot %in% c("B1", "B2", "B3") && red_score < blue_score, 3, 0),
            ifelse(red_score == blue_score, 1, 0)
        ),
        win = case_when(
            robot %in% c("R1", "R2", "R3") && red_score > blue_score ~ TRUE,
            robot %in% c("B1", "B2", "B3") && red_score < blue_score ~ TRUE,
            .default = FALSE
        ),
        loss = case_when(
            robot %in% c("R1", "R2", "R3") && blue_score > red_score ~ TRUE,
            robot %in% c("B1", "B2", "B3") && blue_score < red_score ~ TRUE,
            .default = FALSE
        ),
        tie = ifelse(red_score == blue_score, TRUE, FALSE),
        score = ifelse(robot %in% c("R1", "R2", "R3"), red_score, blue_score)
    )

teams <- as.integer(names(ranking_points))

rankings <- data.frame(team = names(ranking_points))
rankings$wins = sapply(teams, calculate, raw = matches_tidy, col = "win")
rankings$losses = sapply(teams, calculate, raw = matches_tidy, col = "loss")
rankings$ties = sapply(teams, calculate, raw = matches_tidy, col = "tie")
rankings$rp = sapply(teams, calculate, raw = matches_tidy, col = "rp")
rankings$plays = sapply(teams, calculate, raw = matches_tidy, col = "matches")
rankings$dq = rep(0, length(rankings$team))
rankings$average_match = round(
    sapply(teams, calculate, raw = matches_tidy, col = "score") / rankings$plays,
    2
    )

rankings <- rankings |>
    mutate(average_rp = round(rp / plays, 2)) |>
    arrange(desc(average_rp))

rankings$rank <- 1:length(rankings$team)

rankings <- rankings |>
    select(team, rank, plays, dq, wins, losses, ties, average_rp, rp, average_match) |>
    rename(
        team_key = team, matches_played = plays, `Ranking Score` = average_rp,
        `Avg Match` = average_match, `Ranking Total` = rp
    )

write.csv(rankings, "thebunnybotsalliance/data/rankings.csv", row.names = FALSE)
save(rankings, file = "thebunnybotsalliance/simulation/data/rankings.rda")
