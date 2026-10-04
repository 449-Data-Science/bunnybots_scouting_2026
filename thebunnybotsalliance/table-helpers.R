library(DT)
library(htmltools)

# Shared look for every table in the app. Styling lives in www/styles.css
# (classes: tba-table, zebra, red-cell, blue-cell, win, pending, me).

# Searching is handled by our own search box (see table_header() here and the
# search handler in www/script.js), so DataTables' built-in search box is off.
# `query` re-applies the current search text when a table is redrawn.
base_options <- function(query = "", ordering = FALSE, ...) {
    modifyList(
        list(
            paging = FALSE, info = FALSE, scrollX = TRUE, ordering = ordering,
            dom = "t", search = list(search = query), autoWidth = FALSE
        ),
        list(...)
    )
}

# Card header with the title on the left and a search box on the right
# (the box drops under the title on phones).
table_header <- function(title, query_id, table_id) {
    card_header(
        class = "card-header-search",
        span(class = "card-title-text", title),
        div(
            class = "table-search", `data-target` = table_id,
            textInput(query_id, label = NULL, placeholder = "Search", width = "100%")
        )
    )
}

# Marks the winning score, greys out matches that haven't been played, and
# highlights `highlight` (a team number) wherever it appears.
match_row_callback <- function(highlight = NULL) {
    hl <- if (is.null(highlight)) "" else gsub("[^0-9A-Za-z]", "", highlight)
    JS(sprintf("function(row, data) {
        // Score cells contain badge HTML, so strip tags before reading the number
        var num = function(x) { return parseFloat(String(x).replace(/<[^>]*>/g, '')); };
        var r = num(data[7]), b = num(data[8]);
        if (isNaN(r) || isNaN(b)) { $(row).addClass('pending'); }
        else if (r > b) { $('td:eq(7)', row).addClass('win'); }
        else if (b > r) { $('td:eq(8)', row).addClass('win'); }
        var hl = '%s';
        if (hl !== '') {
            [1, 2, 3, 4, 5, 6].forEach(function(i) {
                if (String(data[i]) === hl) { $('td:eq(' + i + ')', row).addClass('me'); }
            });
        }
    }", hl))
}

# Score followed by three tiny RP badges (S, B, D) stacked to its right. Badges are
# filled when the bonus RP was earned and outlined when it wasn't. No badges if flags are NA.
score_cell <- function(score, stocked, baked, dinner) {
    badge <- function(flag, letter, name) {
        paste0("<span class='rp ", ifelse(flag, "on", "off"), "' title='", name,
               ifelse(flag, " RP earned", " RP not earned"), "'>", letter, "</span>")
    }
    has_rp <- !is.na(score) & !is.na(stocked) & !is.na(baked) & !is.na(dinner)
    badges <- paste0(
        "<span class='rp-stack'>",
        badge(stocked, "S", "Stocked"), badge(baked, "B", "Baked"),
        badge(dinner, "D", "Dinner"), "</span>"
    )
    ifelse(is.na(score), NA_character_,
           ifelse(has_rp,
                  paste0("<span class='score-cell'>", score, badges, "</span>"),
                  as.character(score)))
}

# Qualification, playoff and per-team match tables
match_table <- function(df, highlight = NULL, query = "") {
    df <- df |>
        mutate(
            `Red Score` = score_cell(
                `Red Score`, `Red Stocked RP`, `Red Baked RP`, `Red Dinner RP`),
            `Blue Score` = score_cell(
                `Blue Score`, `Blue Stocked RP`, `Blue Baked RP`, `Blue Dinner RP`)
        ) |>
        select(
            Match, `Red 1`, `Red 2`, `Red 3`,
            `Blue 1`, `Blue 2`, `Blue 3`, `Red Score`, `Blue Score`
        )
    
    container <- withTags(table(
        class = "tba-table",
        thead(
            tr(
                th(rowspan = 2, "Match"),
                th(colspan = 3, class = "grp-red", "Red alliance"),
                th(colspan = 3, class = "grp-blue", "Blue alliance"),
                th(colspan = 2, "Score")
            ),
            tr(
                lapply(rep(c("Team 1", "Team 2", "Team 3"), 2), th),
                th(class = "grp-red", "Red"),
                th(class = "grp-blue", "Blue")
            )
        )
    ))
    
    datatable(
        df, container = container, rownames = FALSE, selection = "none",
        class = "tba-table match-table",
        escape = setdiff(names(df), c("Red Score", "Blue Score")),
        options = base_options(
            query = query,
            rowCallback = match_row_callback(highlight),
            columnDefs = list(
                list(targets = 0, className = "match-cell"),
                list(targets = c(1:3, 7), className = "dt-center red-cell"),
                list(targets = c(4:6, 8), className = "dt-center blue-cell"),
                list(targets = "_all", defaultContent = "\u2013")
            )
        )
    )
}

rankings_table <- function(df, query = "") {
    datatable(
        df, rownames = FALSE, selection = "none", class = "tba-table zebra",
        options = base_options(
            query = query, ordering = TRUE,
            order = list(list(0, "asc")),
            columnDefs = list(
                list(targets = "_all", className = "dt-center"),
                list(targets = "_all", defaultContent = "\u2013")
            )
        )
    ) |>
        formatRound(c("Ranking Score", "Avg Match"), 2) |>
        formatStyle("Rank", fontWeight = "700") |>
        formatStyle("Team", fontWeight = "700") |>
        formatStyle(
            "Ranking Score",
            background = styleColorBar(c(0, max(df$`Ranking Score`, na.rm = TRUE)), "#f9d3b5"),
            backgroundSize = "100% 70%", backgroundRepeat = "no-repeat",
            backgroundPosition = "center"
        )
}

# Alliances and the wide per-match detail table
simple_table <- function(df, query = "") {
    df <- df |>
        mutate(across(where(is.logical), ~ ifelse(.x, "\u2713", "\u2013")))
    
    datatable(
        df, rownames = FALSE, selection = "none", class = "tba-table zebra",
        options = base_options(
            query = query,
            columnDefs = list(
                list(targets = "_all", className = "dt-center"),
                list(targets = "_all", defaultContent = "\u2013")
            )
        )
    )
}