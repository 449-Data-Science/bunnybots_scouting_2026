function(input, output, session) {
    qual_matches_raw <- reactivePoll(
        intervalMillis = POLL_INTERVAL, session = session,
        checkFunc = function() { Sys.time() },
        valueFunc = function() { get_matches("qualification") }
    )
    
    elim_matches_raw <- reactivePoll(
        intervalMillis = POLL_INTERVAL, session = session,
        checkFunc = function() { Sys.time() },
        valueFunc = function() { get_matches("playoff") }
    )
    
    qual_schedule_raw <- reactivePoll(
        intervalMillis = POLL_INTERVAL, session = session,
        checkFunc = function() { Sys.time() },
        valueFunc = function() { get_schedule("qualification") }
    )
    
    elim_schedule_raw <- reactivePoll(
        intervalMillis = POLL_INTERVAL, session = session,
        checkFunc = function() { Sys.time() },
        valueFunc = function() { get_schedule("playoff") }
    )
    
    rankings_raw <- reactivePoll(
        intervalMillis = POLL_INTERVAL, session = session,
        checkFunc = function() { Sys.time() },
        valueFunc = function() { get_rankings() }
    )
    
    alliances_raw <- reactivePoll(
        intervalMillis = POLL_INTERVAL,
        session = session,
        checkFunc = function() Sys.time(),
        valueFunc = function() get_alliances()
    )
    
    matches_raw <- reactiveVal(read_csv("data/matches.csv"))
    rankings_raw <- reactiveVal(read_csv("data/rankings.csv"))
    playoffs_raw <- reactiveVal(read_csv("data/playoffs.csv"))
    alliances_raw <- reactiveVal(read_csv("data/alliances.csv"))
    qual_schedule_raw <- reactiveVal(read_csv("data/qual_schedule.csv"))
    #elim_schedule_raw <- reactiveVal(read_csv("data/elim_schedule.csv"))
    
    matches_data <- reactiveVal()
    rankings_data <- reactiveVal()
    playoffs_data <- reactiveVal()
    
    observe({
        matches_data(process_matches(matches_raw(), qual_schedule_raw()))
        rankings_data(process_rankings(rankings_raw()))
        playoffs_data(process_playoffs(playoffs_raw()))
    })
    
    observe({
        unique_teams <- sort(as.integer(rankings_data()$Team))
        updateVirtualSelect("selected_team", choices = unique_teams)
    })
    
    # ---- Search boxes ----
    # Filtering happens in the browser (www/script.js). This reads the current
    # search text without triggering a redraw, so a table that refreshes with
    # new data keeps its filter.
    query_of <- function(id) {
        value <- isolate(input[[id]])
        if (is.null(value)) "" else value
    }
    
    output$matches_table  <- renderDT({
        match_table(matches_data(), query = query_of("matches_q"))
    })
    
    output$playoffs_table <- renderDT({
        match_table(playoffs_data())
    })
    
    output$alliances_table <- renderDT({
        dataframe <- alliances_raw()
        # Same headers whether the data comes from the simulation or the API
        names(dataframe) <- tools::toTitleCase(gsub("_", " ", names(dataframe)))
        simple_table(dataframe)
    })
    
    output$rankings_table <- renderDT({
        rankings_table(rankings_data(), query = query_of("rankings_q"))
    })
    
    output$detailed_table  <- renderDT({
        dataframe <- rbind(matches_raw(), playoffs_raw() |> select(!match_string))
        simple_table(dataframe, query = query_of("detailed_q"))
    })
    
    output$team_table <- renderDT({
        req(input$selected_team)
        team <- as.character(input$selected_team)
        
        dataframe <- rbind(matches_data(), playoffs_data()) |>
            filter(
                `Red 1` == team | `Red 2` == team | `Red 3` == team |
                    `Blue 1` == team | `Blue 2` == team | `Blue 3` == team
            )
        
        match_table(dataframe, highlight = team)
    })
    
    output$download_qual_matches <- downloadHandler(
        filename = function() { "quals.csv" },
        content = function(file) { write.csv(matches_data(), file, row.names = FALSE) }
    )
    
    output$download_playoffs <- downloadHandler(
        filename = function() { "playoffs.csv" },
        content = function(file) { 
            write.csv(
                playoffs_data() |> select(!ends_with(" RP")), 
                file, row.names = FALSE
                ) 
            }
    )
    
    output$download_rankings <- downloadHandler(
        filename = function() { "rankings.csv" },
        content = function(file) { write.csv(rankings_data(), file, row.names = FALSE) }
    )
    
    output$download_alliances <- downloadHandler(
        filename = function() { "alliances.csv" },
        content = function(file) { write.csv(alliances_raw(), file, row.names = FALSE) }
    )
}