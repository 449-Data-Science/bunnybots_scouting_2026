set.seed(449)

n = 120
scout_initials = sample(c("NS","HR", "MH", "MY", "RY"), size=n, replace=TRUE)
teams_list = c(116,122,449,526,539,611,612,620,1731,1793,1885,1895,1908,2028,2068,2186,2363,2377,2534,2890,2963,2988,3136,3258,3359,3373,4099,4472,4638,5243,5338,5353,5549,5587,6882,8230,8326,9072,9403,10449)
team = as.vector(replicate(ceiling(n / length(teams_list)), sample(teams_list, length(teams_list), replace = FALSE)))
team = team[-seq(length(team) - (ceiling(n/length(teams_list))*length(teams_list)-n) + 1, length(team)+1)]
match = floor(seq(from=6,to=n+5)/6)
robot = rep(c("R1", "R2", "R3", "B1", "B2", "B3"), n/6)

taxi <- sample(0:1, n, TRUE, c(2,8))
park <- sample(0:1, n, TRUE, c(2,8))
auto_pantry <- sample(0:3, n, TRUE, c(30, 5, 50, 10))
auto_oven <- sample(0:3, n, TRUE, c(50, 10, 20, 10))
tele_carrots_l1 <- sample(0:5, n, TRUE, c(25, 30, 20, 20, 10, 5))
tele_carrots_l2 <- sample(0:5, n, TRUE, c(45, 30, 20, 20, 10, 5))
tele_carrots_l3 <- sample(0:5, n, TRUE, c(25, 30, 20, 20, 10, 5))
tele_cakes_l1 <- sample(0:5, n, TRUE, c(40, 30, 20, 10, 5, 1))
tele_cakes_l2 <- sample(0:5, n, TRUE, c(30, 40, 20, 10, 5, 1))
tele_cakes_l3 <- sample(0:5, n, TRUE, c(20, 50, 30, 20, 5, 1))
tele_carrots_oven <- sample(0:75, n, TRUE)
tele_cakes_oven <- sample(0:75, n, TRUE)
driver_rating <- sample(0:5, n, TRUE)
problems <- sample(c("No Problems", "Died", "No Show", "Browned out", "A-Stop", "E-Stop", "Parts Fell Off"), n, TRUE, c(6, 2, 2, 2, 2, 2, 2))
defensive_strategy <- sample(c("No Defense", "Thief", "OZ", "NZ"), n, TRUE, c(6,2,2,2))
scouter_initials <- sample(c("NS", "HG", "LM", "CH"), n, TRUE, c(0.1,0.2,0.2,0.5))
match_number <- c(rep(1:20, each = 6))
robot <- sample(c("Red 1", "Red 2", "Red 3", "Blue 1", "Blue 2", "Blue 3"), n, TRUE)
team_number <- sample(c(449, 3247, 5642, 214, 7483, 1424, 5432, 3531,1353,1), n, TRUE)
parked_carrots <- sample(0:3, n, TRUE, c(3, 2, 4, 5))
parked_cakes <- sample(0:3, n, TRUE, c(5, 2, 1, 1))
kitchen_intake <- sample(0:10, n, TRUE)
neutral_intake <- sample(0:10, n, TRUE)
source_intake <- sample(0:10, n, TRUE)
df <- data.frame(
        match,
        team,
        robot,
        taxi,
        auto_pantry,
        auto_oven,
        tele_carrots_l1,
        tele_carrots_l2,
        tele_carrots_l3,
        tele_cakes_l1,
        tele_cakes_l2,
        tele_cakes_l3,
        tele_carrots_oven,
        tele_cakes_oven,
        driver_rating, problems,
        defensive_strategy,
        scouter_initials,
        park,
        parked_carrots,
        parked_cakes,
        kitchen_intake,
        neutral_intake,
        source_intake
)
