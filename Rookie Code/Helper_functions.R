
#Problems
#Defense
#Logo(Make Nico Do This)
#Yap per comment(use nchar function)
#Scouting Streak(Hard to do, procrastinate as long as possible)
#Individual Comments
#Make Non general individual tabs do something
#Predicted Score
#Oven Scored Carrots
#Oven Scored Cakes
#Match Carrots/Cakes
#Match Auto(IDK what this is, figure it out)
#Score Boxplots
#Auto Scoring Locations
#W/L ratio (Make it work somehow)
#Endgame carrots/Cakes held
#

library(tidyverse)
library(ggplot2)
teams_selected <- 449
data <- df

individual_carrots_graph <- function(raw=data,team_selected=teams_selected) {
    data <- raw |>
        filter(team == team_selected) |>
        group_by(team) |>
        summarize(
            tele_carrots_l1 = mean(tele_carrots_l1, na.rm = TRUE), 
            tele_carrots_l2 = mean(tele_carrots_l2, na.rm = TRUE), 
            tele_carrots_l3 = mean(tele_carrots_l3, na.rm = TRUE),
            tele_carrots_oven = mean(tele_carrots_oven, na.rm = TRUE),
        ) |> 
        pivot_longer(cols = c(tele_carrots_l1, tele_carrots_l2, tele_carrots_l3, tele_carrots_oven), names_to = "Locations", values_to = "Carrots")
    ggplot(
        data = data,
        mapping = aes(x = Locations, y = Carrots),
    ) +
        geom_bar(stat = "identity") +
        theme_bw() +
        scale_y_continuous(breaks = seq(0, 50, by = 5))
}

individual_cakes_graph <- function(raw=data,team_selected=teams_selected) {
    data <- raw |>
        filter(team == team_selected) |>
        group_by(team) |>
        summarize(
            tele_cakes_l1 = mean(tele_cakes_l1, na.rm = TRUE), 
            tele_cakes_l2 = mean(tele_cakes_l2, na.rm = TRUE), 
            tele_cakes_l3 = mean(tele_cakes_l3, na.rm = TRUE),
            tele_cakes_oven = mean(tele_cakes_oven, na.rm = TRUE),
        ) |> 
        pivot_longer(cols = c(tele_cakes_l1, tele_cakes_l2, tele_cakes_l3, tele_cakes_oven), names_to = "Locations", values_to = "Cakes")
    ggplot(
        data = data,
        mapping = aes(x = Locations, y = Cakes),
    ) +
        geom_bar(stat = "identity") +
        theme_bw() +
        scale_y_continuous(breaks = seq(0, 50, by = 5))
}

individual_intake_graph <- function(raw=data,team_selected=teams_selected) {
    data <- raw |>
        filter(team == team_selected) |>
        group_by(team) |>
        summarize(
            kitchen_intake = mean(kitchen_intake, na.rm = TRUE), 
            neutral_intake = mean(neutral_intake, na.rm = TRUE), 
            source_intake = mean(source_intake, na.rm = TRUE)
        ) |> 
        pivot_longer(cols = c(kitchen_intake, neutral_intake, source_intake), names_to = "Intake Locations", values_to = "Carrots/Cakes Intaked")
    ggplot(
        data = data,
        mapping = aes(x = `Intake Locations`, y = `Carrots/Cakes Intaked`),
    ) +
        geom_bar(stat = "identity") +
        theme_bw() +
        scale_y_continuous(breaks = seq(0, 50, by = 2))
    
}

individual_driver_rating_graph <- function(raw=data,team_selected=teams_selected) {
    data <- raw |>
        filter(team == team_selected)
    
    ggplot(data, aes(x = match, y = driver_rating)) +
        geom_line() +
        theme_bw() +
        scale_y_continuous(breaks = unique(match)) +
        coord_cartesian(xlim = c(0, 20), ylim = c(0, 5)) +
        labs(x = "Match", y = "Driver Rating")
}

scouting_times_graph <- function(raw=data) {
    data <- raw |>
        group_by(scouter_initials) |>
        summarise(count = n()) |>
        mutate(scouter_initials = factor(scouter_initials), 
               scouter_initials = fct_reorder(scouter_initials, desc(count))
        )
    
    ggplot(data, aes(x = scouter_initials, y = count)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(breaks = seq(0, 1000, by = 5)) +
        theme_bw() +
        labs(y = "Count", x = "Scouter Initials")
    
}
total_score <- function(raw=data){
data <- raw |>
    group_by(team) |> 
    summarise(
        taxi = mean(taxi),
        auto_pantry = mean(auto_pantry),
        auto_oven = mean(auto_oven),
        tele_cakes_l1 = mean(tele_cakes_l1),
        tele_cakes_l2 = mean(tele_cakes_l2),
        tele_cakes_l3 = mean(tele_cakes_l3),
        tele_cakes_oven = mean(tele_cakes_oven),
        tele_carrots_l1 = mean(tele_carrots_l1),
        tele_carrots_l2 = mean(tele_carrots_l2),
        tele_carrots_l3 = mean(tele_carrots_l3),
        tele_carrots_oven = mean(tele_carrots_oven),
        park = mean(park),
        parked_cakes = mean(parked_cakes),
        parked_carrots = mean(parked_carrots)
    ) |> 
    mutate(
        tot_auto = auto_pantry * 4 + auto_oven * 2,
        tot_endgame = park * 2 + parked_carrots + parked_cakes * 3,
        tot_tele_pantry = tele_cakes_l1 * 8 + 
            tele_cakes_l2 * 10 + tele_cakes_l3 * 12 + tele_carrots_l1 * 3 + 
            tele_carrots_l2 * 4 + tele_carrots_l3 * 5,
        tot_tele_oven = tele_carrots_oven * 2 + tele_cakes_oven,
        tot_tele = tot_tele_pantry + tot_tele_oven,
        tot_score = tot_auto + tot_endgame + tot_tele,
        team = factor(team),
        team = fct_reorder(team, desc(tot_score))
    ) |>
    select(c("team", "tot_auto", "tot_tele_pantry", "tot_tele_oven", "tot_endgame", "tot_score")) |>
    pivot_longer(cols = c("tot_auto", "tot_tele_pantry", "tot_tele_oven", "tot_endgame"), 
                 names_to = "Phase",
                 values_to = "Score"
                 )
ggplot(data, aes(fill = Phase, x = team, y = Score)) + geom_bar(position = "stack", stat = "identity") + coord_flip()
}