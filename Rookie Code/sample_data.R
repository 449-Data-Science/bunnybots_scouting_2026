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
problems <- sample(c("No Problems", "Died", "No Show", "Browned out", "A-Stop", "E-Stop", "Parts Fell Off"), 120, TRUE, c(6, 2, 2, 2, 2, 2, 2))
defensive_strategy <- sample(c("No Defense", "Thief", "OZ", "NZ"), 120, TRUE, c(6,2,2,2))
scouter_initials <- sample(c("NS", "HG", "LM", "CH"), 120, TRUE)
match_number <- c(rep(1:20, each = 6))
robot <- sample(c("Red 1", "Red 2", "Red 3", "Blue 1", "Blue 2", "Blue 3"), 120, TRUE)
team_number <- sample(c(449, 3247, 5642, 214, 7483, 1424, 5432, 3531,1353,1), 120, TRUE)
parked_carrots <- sample(0:3, 120, TRUE, c(3, 2, 4, 5))
parked_cakes <- sample(0:3, 120, TRUE, c(5, 2, 1, 1))
kitchen_intake <- sample(0:10, 120, TRUE)
neutral_intake <- sample(0:10, 120, TRUE)
source_intake <- sample(0:10, 120, TRUE)
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
        scouter_initials,
        parked_carrots,
        parked_cakes,
        kitchen_intake,
        neutral_intake,
        source_intake
    )
)
