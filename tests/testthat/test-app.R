test_that("the UI contains all inputs and outputs used by the server", {
    skip_if_not_installed("shinythemes")
    skip_if_not_installed("shinyjs")
    skip_if_not_installed("DT")

    env <- new.env(parent = globalenv())
    sys.source(file.path(app_dir, "global.R"), envir = env)
    sys.source(file.path(app_dir, "ui.R"), envir = env)

    html <- as.character(env$ui)
    for (id in c("prot", "tab", "clicko", "slic", "search", "searchbut", "out", "outtab", "cutt", "maybe")) {
        expect_match(html, sprintf('id="%s"', id), fixed = TRUE, info = id)
    }
})

# shinyjs needs the UI to be loaded, which testServer does not do
mock_shinyjs <- function(env = parent.frame()) {
    testthat::local_mocked_bindings(
        show = function(...) invisible(NULL),
        hide = function(...) invisible(NULL),
        .package = "shinyjs",
        .env = env
    )
}

test_that("READ DATA parses the protein and shows its length", {
    skip_if_not_installed("shinyjs")
    mock_shinyjs()

    shiny::testServer(app$server, {
        suppressWarnings(session$setInputs(prot = "1 ABCDE\n  6 FGHIJ", tab = enzyme_text, clicko = 1))
        # line numbers (GenBank style) are stripped from the pasted sequence
        expect_equal(seqx(), "ABCDEFGHIJ")
        expect_equal(output$out, "Length: 10\nABCDEFGHIJ")

        df <- dfx()
        expect_equal(df[[1]], c("None_start", "None_end", "Trypsin"))
        expect_equal(df[[3]], c("0", "10", "3 6"))
        expect_null(cutdf())
        expect_null(searchdf())
    })
})

test_that("SLICE IT UP creates the peptide table", {
    skip_if_not_installed("shinyjs")
    mock_shinyjs()

    shiny::testServer(app$server, {
        suppressWarnings(session$setInputs(prot = "ABCDEFGHIJ", tab = enzyme_text, clicko = 1))
        session$setInputs(slic = 1)
        expect_equal(nrow(cutdf()), 6)
        expect_true("DEF" %in% cutdf()$Peptide)
    })
})

test_that("SEARCH finds peptides in the protein", {
    skip_if_not_installed("shinyjs")
    mock_shinyjs()

    shiny::testServer(app$server, {
        suppressWarnings(session$setInputs(prot = "ABCDEFGHIJ", tab = enzyme_text, clicko = 1))
        session$setInputs(search = "DEF", searchbut = 1)
        expect_equal(searchdf()$Peptide, "DEF")
        expect_equal(searchdf()$Start, 3)
    })
})

test_that("nothing happens before data is read", {
    skip_if_not_installed("shinyjs")
    mock_shinyjs()

    shiny::testServer(app$server, {
        session$setInputs(slic = 1)
        expect_null(cutdf())
    })
})
