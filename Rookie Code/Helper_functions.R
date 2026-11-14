library(tidyverse)

raw <- df
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
