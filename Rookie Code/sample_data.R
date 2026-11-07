n = 120
auto_pantry <- sample(0:3, 120, TRUE, c(30, 5, 50, 10))
auto_oven <- sample(0:3, 120, TRUE, c(50, 10, 20, 10))
tele_carrots_l1 <- sample(0:5, 120, TRUE, c(25, 30, 20, 20, 10, 5))
tele_carrots_l2 <- sample(0:5, 120, TRUE, c(45, 30, 20, 20, 10, 5))
tele_carrots_l3 <- sample(0:5, 120, TRUE, c(25, 30, 20, 20, 10, 5))
tele_cakes_l1 <- sample(0:5, 120, TRUE, c(40, 30, 20, 10, 5, 1))
tele_cakes_l2 <- sample(0:5, 120, TRUE, c(30, 40, 20, 10, 5, 1))
tele_cakes_l3 <- sample(0:5, 120, TRUE, c(20, 50, 30, 20, 5, 1))
tele_carrots_oven <- sample(0:75, 120, TRUE)
driver_rating <- sample(0:5, 120, TRUE)
#fill in when able to find real problem options
problems <- sample(c("No Problems", "Problem Example 1", "Problem Example 2"), 120, TRUE, c(6,2,2))
#fill in when able to find real defense options
defensive_strategy <- sample(c("No Defense", "Defense Example 1", "Defense Example 2"), 120, TRUE, c(6,2,2))
yellow_red_card <- sample(c("No Card", "Yellow Card", "Red Card"), 120, TRUE, c(5,2,1))
scouter_initials <- sample(c("NS", "Other NS", "Third NS", "CH"), 120, TRUE)
match_number <- c(rep(1:20, each = 6))
robot <- sample(c("Red 1", "Red 2", "Red 3", "Blue 1", "Blue 2", "Blue 3"), 120, TRUE)
team_number <- sample(c(449, 499, 444, 494, 949, 999, 944, 994), 120, TRUE)
df <- data.frame(
    cbind(
        match_number,
        team_number,
        robot,
        auto_pantry,
        auto_oven,
        tele_carrots_l1,
        tele_carrots_l2,
        tele_carrots_l3,
        tele_cakes_l1,
        tele_cakes_l2,
        tele_cakes_l3,
        tele_carrots_oven,
        driver_rating, problems,
        defensive_strategy,
        yellow_red_card,
        scouter_initials
    )
)
