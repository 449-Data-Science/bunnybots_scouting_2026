library(tidyverse)
library(ggplot2)
team_selected <- 1885
raw <- df

individual_carrots_graph <- function(raw,team_selected) {
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
    
individual_cakes_graph <- function(raw,team_selected) {
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
    
individual_intake_graph <- function(raw,team_selected) {
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

individual_driver_rating_graph <- function(raw,team_selected) {
data <- raw |>
    filter(team == team_selected)

ggplot(data, aes(x = match, y = driver_rating)) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(breaks = unique(match)) +
    coord_cartesian(xlim = c(0, 20), ylim = c(0, 5)) +
    labs(x = "Match", y = "Driver Rating")
}

scouting_times_graph <- function(raw) {
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