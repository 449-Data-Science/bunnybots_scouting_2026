library(httr2)
library(jsonlite)
library(dplyr)

cheesy_base_url <- "http://localhost:8080"

get_matches <- function(match_type = "qualification") {
    req <- request(paste0(cheesy_base_url, "/api/matches/", match_type))
    resp <- req_perform(req)
    raw <- resp_body_json(resp, simplifyVector = FALSE)
    purrr::map_dfr(raw, function(m) {
        tibble(
            match    = m$Id,
            short_name  = m$ShortName,
            red_score   = m$Result$RedSummary$Score %||% NA,
            blue_score  = m$Result$BlueSummary$Score %||% NA,
            result = list(m$Result)
        ) |>
            unnest_wider(result)
    })
    
}

get_schedule <- function(match_type = "qualification") {
    req <- request(paste0(cheesy_base_url, "/reports/csv/schedule/", match_type))
    resp <- req_perform(req)
    raw <- resp_body_string(resp) |> 
        read_csv()
}

get_rankings <- function() {
    resp <- request(paste0(cheesy_base_url, "/api/rankings")) |> req_perform()
    raw <- resp_body_json(resp, simplifyVector = TRUE)
    as_tibble(raw$Rankings)
}

get_alliances <- function() {
    req <- request(paste0(cheesy_base_url, "/api/alliances"))
    resp <- req_perform(req)
    raw <- resp_body_json(resp, simplifyVector = FALSE)
    purrr::map_dfr(raw, function(m) {
        tibble(
            Alliance       = paste0("Alliance ", m$Id),
            Captain        = m$TeamIds[1],
            `First Pick`   = m$TeamIds[2],
            `Second Pick`  = m$TeamIds[3]
        )
    })
}
