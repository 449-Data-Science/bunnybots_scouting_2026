

# Define server logic required to draw a histogram
function(input, output, session) {
    observe({
        teams = sort(unique(raw$team))
        updateVirtualSelect("team_selected", choices = teams)
    })
    
    output$carrots <- renderPlot({
        individual_carrots_graph(team_selected = input$team_selected)
    })
    output$cakes <- renderPlot({
        individual_cakes_graph(team_selected = input$team_selected)
    })
    output$intake <- renderPlot({
        individual_intake_graph(team_selected = input$team_selected)
    })
    output$driver_rating <- renderPlot({
        individual_driver_rating_graph(team_selected = input$team_selected)
    })
    output$tot_score <- renderPlot({
        total_score()
    })
    output$scout_times <- renderPlot({
        scouting_times_graph()
    })
}
