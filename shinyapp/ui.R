fluidPage(
    theme = bs_theme(
        version = 5,
        preset = "flatly",
    ),
    navbarPage(
        title = "Shiny App",
        tabPanel(
            title = "Main",
            plotOutput("tot_score")
        ),
        tabPanel(
            title = "Individual",
            page_sidebar(
                sidebar = sidebar(
                    virtualSelectInput("team_selected", label = "Select a Team", choices = NULL),
                    
                    #Does nothing, fix later
                    virtualSelectInput("phase_selected", label = "Select a Phase", choices = c("Auto", "Tele", "General"))
                ),
                layout_columns(
                    card(
                        card_header("Carrot Plot"),
                        plotOutput("carrots")
                    ),
                    card(
                        card_header("Cake Plot"),
                        plotOutput("cakes"),
                    )
                ),
                layout_columns(
                card(
                    card_header("Intake Plot"),
                    plotOutput("intake"),
                ),
                card(
                    card_header("Driver Rate Plot"),
                    plotOutput("driver_rating")
                )
                )
            )
        ),
        tabPanel(
            title = "Match",
        ),
        tabPanel(
            title = "Scouts",
            plotOutput("scout_times")
        )
        
        
    )
)
