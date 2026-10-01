library(shiny)

# Locate the app directory regardless of where testthat is invoked from
app_dir <- normalizePath(file.path(testthat::test_path(), "..", "..", "app"))

# server.R defines its helper functions at top level and then registers the
# server with shinyServer(). Evaluate the helpers and the server function into
# one environment, without registering anything or modifying the app code.
load_server <- function() {
    env <- new.env(parent = globalenv())
    exprs <- parse(file.path(app_dir, "server.R"), keep.source = FALSE)
    server <- NULL
    for (e in exprs) {
        if (is.call(e) && identical(e[[1]], as.name("<-"))) {
            eval(e, env)
        } else if (is.call(e) && identical(e[[1]], as.name("shinyServer"))) {
            server <- eval(e[[2]], env)
        }
    }
    list(env = env, server = server)
}

app <- load_server()

# Enzyme table in the format the app expects after parsing:
# name, number of cuts, space separated cleavage positions
enzyme_df <- function() {
    data.frame(
        Enzyme    = c("None_start", "None_end", "Trypsin"),
        Cuts      = c("0", "0", "2"),
        Positions = c("0", "10", "3 6"),
        stringsAsFactors = FALSE
    )
}

# Same table as the tab separated text a user pastes in
enzyme_text <- "Enzyme\tCuts\tPositions\nTrypsin\t2\t3 6"
