library(httr2)
library(jsonlite)
library(dplyr)

cheesy_base_url <- "http://temp-link"

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
            # add more later after i get to see the actual result
        )
    })
}

get_rankings <- function() {
    resp <- request(paste0(cheesy_base_url, "/api/rankings")) |> req_perform()
    raw <- resp_body_json(resp, simplifyVector = TRUE)
    as_tibble(raw$Rankings)
}