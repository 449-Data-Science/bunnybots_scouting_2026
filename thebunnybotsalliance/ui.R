navbarPage(
    title = "The Bunnybots Alliance: Harvest Havoc 2026",
    theme = bs_theme(version = 5, preset = "flatly"),
    collapsible = TRUE,
    header = tagList(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        #tags$head(tags$script(src = "script.js", type = "text/javascript")),
    ),
    tabPanel(
        title = "Results",
        
        tags$div(
            class = "results-grid",
            
            # Qualification Matches
            card(
                class = "qualification-card",
                card_header("Qualification Results"),
                fill = FALSE,
                card_body(
                    fillable = FALSE,
                    DTOutput("matches_table")
                )
            ),
            
            # Alliances
            card(
                class = "alliances-card",
                card_header("Alliances"),
                fill = FALSE,
                card_body(
                    fillable = FALSE,
                    DTOutput("alliances_table")
                )
            ),
            
            # Playoffs
            card(
                class = "playoffs-card",
                card_header("Playoff Results"),
                fill = FALSE,
                card_body(
                    fillable = FALSE,
                    DTOutput("playoffs_table")
                )
            )
        )
    ),
    tabPanel(
        title = "Rankings",
        card(
            fill = FALSE,
            card_body(
                fillable = FALSE,
                DTOutput("rankings_table")
            )
        )
    ),
    tabPanel(
        title = "Matches",
        card(
            fill = FALSE,
            card_body(
                fillable = FALSE,
                DTOutput("detailed_table")
            )
        )
    ),
    tabPanel(# ------------------------ TEAMS -------------------------
        title = "Teams",
        div(class = "container-fluid", div(class = "row",
        div(class = "col-12 col-lg-3", div(
            style = "
                background-color: #f8f9fa; padding: 15px; 
                border-radius: 5px; min-height: 100%;",
            virtualSelectInput(
                "selected_team", label = "Select a Team", 
                choices = NULL, multiple = FALSE, search = TRUE
                )
        )),
        div(class = "col-12 col-lg-9",
            card(
                fill = FALSE,
                card_body(
                    fillable = FALSE,
                    DTOutput("team_table")
                    )
                )
            )
        ))
        ),
    tabPanel( #------------------------- SCOUTING -----------------------------
        title = "Scouting",
        card(
            card_header("Data Downloads"),
            downloadButton(
                outputId = "download_qual_matches",
                label = "Download Qualification Matches",
                icon = icon("download"),
                class = "btn btn-primary btn-sm"
                ),
            downloadButton(
                outputId = "download_playoffs",
                label = "Download Playoff Matches",
                icon = icon("download"),
                class = "btn btn-primary btn-sm"
                ),
            downloadButton(
                outputId = "download_rankings",
                label = "Download Rankings",
                icon = icon("download"),
                class = "btn btn-primary btn-sm"
                ),
            downloadButton(
                outputId = "download_alliances",
                label = "Download Alliances",
                icon = icon("download"),
                class = "btn btn-primary btn-sm"
                ),
            
            )
        )
    )
