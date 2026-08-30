navbarPage(
    title = "The Bunnybots Alliance: Harvest Havoc 2026",
    theme = bs_theme(version = 5, preset = "flatly"),
    collapsible = TRUE,
    header = tagList(
        tags$link(rel = "stylesheet", type = "text/css", href = "assets/styles.css"),
        tags$head(tags$script(src = "assets/script.js", type = "text/javascript")),
    ),
    tabPanel(
        title = "Results",
        div(
            card_header("Qualification Results"),
            DTOutput("matches_table")
        ),
        div(
            card_header("Playoff Results"),
            DTOutput("playoffs_table")
        )
    ),
    tabPanel(
        title = "Rankings",
        card(
            DTOutput("rankings_table")
            )
    ),
    tabPanel(
        title = "Scouting",
        
    )
)
