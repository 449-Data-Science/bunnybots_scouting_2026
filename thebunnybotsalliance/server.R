function(input, output, session) {
    #matches_data <- reactivePoll(
    #    intervalMillis = POLL_INTERVAL,
    #    session = session,
    #    checkFunc = function() {
    #        Sys.time()
    #    },
    #    valueFunc = function() {
    #        get_matches("qualification")
    #    }
    #)
    
    #rankings_data <- reactivePoll(
    #    intervalMillis = POLL_INTERVAL,
    #    session = session,
    #    checkFunc = function() Sys.time(),
    #    valueFunc = function() get_rankings()
    #)
    
    matches_raw <- reactiveVal(read_csv("data/matches.csv"))
    rankings_raw <- reactiveVal(read_csv("data/rankings.csv"))
    playoffs_raw <- reactiveVal(read_csv("data/playoffs.csv"))
    alliances_raw <- reactiveVal(read_csv("data/alliances.csv"))

    matches_data <- reactiveVal()
    rankings_data <- reactiveVal()
    playoffs_data <- reactiveVal()

    observe({
        matches_data(process_matches(matches_raw()))
        rankings_data(process_rankings(rankings_raw()))
        playoffs_data(process_playoffs(playoffs_raw()))
    })
    
    observe({
        unique_teams <- sort(as.integer(rankings_data()$Team))
        updateVirtualSelect("selected_team", choices = unique_teams)
    })
    
    output$matches_table  <- renderDT({
        dataframe <- matches_data()
        datatable(dataframe, options = list(pageLength = nrow(dataframe)))
    })
    
    output$playoffs_table <- renderDT({
        dataframe <- playoffs_data()
        datatable(dataframe, options = list(pageLength = nrow(dataframe)))
    })
    
    output$alliances_table <- renderDT({
        dataframe <- alliances_raw()
        datatable(dataframe, options = list(pageLength = nrow(dataframe)))
    })
    
    output$rankings_table <- renderDT({
        dataframe <- rankings_data()
        datatable(dataframe, options = list(pageLength = nrow(dataframe)))
    })
    
    output$detailed_table  <- renderDT({
        dataframe <- rbind(matches_raw(), playoffs_raw() |> select(!match_string))
        datatable(dataframe, options = list(pageLength = nrow(dataframe)))
    })
    
    output$team_table <- renderDT({
        team <- as.character(input$selected_team)
        
        dataframe <- rbind(matches_data(), playoffs_data()) |>
            filter(
                `Red 1` == team | `Red 2` == team | `Red 3` == team |
                `Blue 1` == team | `Blue 2` == team | `Blue 3` == team
                )
        
        datatable(dataframe, options = list(pageLength = nrow(dataframe)))
    })
    
    output$download_qual_matches <- downloadHandler(
        filename = function() { "quals.csv" },
        content = function(file) { write.csv(matches_data(), file, row.names = FALSE) }
    )
    
    output$download_playoffs <- downloadHandler(
        filename = function() { "playoffs.csv" },
        content = function(file) { write.csv(playoffs_data(), file, row.names = FALSE) }
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