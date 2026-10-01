test_that("make_table slices a sequence at every pair of cleavage sites", {
    result <- NULL
    app$env$make_table(
        seqx  = function() "ABCDEFGHIJ",
        input = list(outtab_rows_selected = NULL),
        dfx   = function() enzyme_df(),
        cutdf = function(x) result <<- x
    )

    expect_equal(colnames(result), c("Peptide", "Start", "End", "Start_Enzymes", "End_enzymes"))
    # sites 0, 3, 6 and 10 give choose(4, 2) = 6 peptides
    expect_equal(nrow(result), 6)
    expect_setequal(result$Peptide, c("ABC", "ABCDEF", "ABCDEFGHIJ", "DEF", "DEFGHIJ", "GHIJ"))

    row <- result[result$Peptide == "DEF", ]
    expect_equal(as.numeric(row$Start), 3)
    expect_equal(as.numeric(row$End), 6)
    expect_equal(row$Start_Enzymes, "Trypsin")
    expect_equal(row$End_enzymes, "Trypsin")

    full <- result[result$Peptide == "ABCDEFGHIJ", ]
    expect_equal(full$Start_Enzymes, "None_start")
    expect_equal(full$End_enzymes, "None_end")
})

test_that("make_table only uses the selected enzymes", {
    df <- rbind(enzyme_df(), data.frame(Enzyme = "Other", Cuts = "1", Positions = "5",
                                        stringsAsFactors = FALSE))
    result <- NULL
    app$env$make_table(
        seqx  = function() "ABCDEFGHIJ",
        input = list(outtab_rows_selected = 4),   # only "Other" (plus the two ends)
        dfx   = function() df,
        cutdf = function(x) result <<- x
    )
    expect_setequal(result$Peptide, c("ABCDE", "ABCDEFGHIJ", "FGHIJ"))
})

test_that("make_table merges enzymes that cut at the same position", {
    df <- rbind(enzyme_df(), data.frame(Enzyme = "Other", Cuts = "1", Positions = "3",
                                        stringsAsFactors = FALSE))
    result <- NULL
    app$env$make_table(
        seqx  = function() "ABCDEFGHIJ",
        input = list(outtab_rows_selected = NULL),
        dfx   = function() df,
        cutdf = function(x) result <<- x
    )
    expect_equal(result$End_enzymes[result$Peptide == "ABC"], "Trypsin;Other")
})

test_that("search_table locates peptides and annotates flanking enzymes", {
    result <- NULL
    app$env$search_table(
        input    = list(search = "DEF,GHI"),
        dfx      = function() enzyme_df(),
        seqx     = function() "ABCDEFGHIJ",
        searchdf = function(x) result <<- x
    )

    expect_equal(colnames(result), c("Peptide", "Start", "End", "Start_Enzymes", "End_enzymes"))
    expect_equal(result$Peptide, c("DEF", "GHI"))
    expect_equal(result$Start, c(3, 6))
    expect_equal(result$End, c(6, 9))
    expect_equal(result$Start_Enzymes, c("Trypsin", "Trypsin"))
    # nothing cuts at position 9
    expect_equal(result$End_enzymes, c("Trypsin", ""))
})

test_that("search_table ignores peptides that are not in the protein", {
    result <- NULL
    app$env$search_table(
        input    = list(search = "ZZZ,DEF"),
        dfx      = function() enzyme_df(),
        seqx     = function() "ABCDEFGHIJ",
        searchdf = function(x) result <<- x
    )
    expect_equal(result$Peptide, "DEF")
})
