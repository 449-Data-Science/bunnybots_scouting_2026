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
    
    matches_data <- reactiveVal()
    rankings_data <- reactiveVal()
    playoffs_data <- reactiveVal()
    
    observe({
        matches_data(process_matches(matches_raw()))
        rankings_data(process_rankings(rankings_raw()))
        playoffs_data(process_playoffs(playoffs_raw()))
    })
    
    
    output$matches_table  <- renderDT(matches_data())
    output$rankings_table <- renderDT(rankings_data())
    output$playoffs_table <- renderDT(playoffs_data())
}