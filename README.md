# pepcutapp
quick'n'dirty app to identify peptides based on PeptideCutter results


A small Shiny app that takes a protein sequence and a PeptideCutter cleavage table, slices the protein at all combinations of cleavage sites and lets you search for known peptides to see which enzymes could have produced them.

[![tests](https://github.com/scheelelab/pepcutapp/actions/workflows/tests.yml/badge.svg)](https://github.com/scheelelab/pepcutapp/actions/workflows/tests.yml)

## Running the app

Install the required R packages and start the app from the repository root:

```r
install.packages(c("shiny", "shinythemes", "shinyjs", "stringr", "DT", "writexl"))
shiny::runApp("app")
```

## Repository layout

| Path | Contents |
|---|---|
| `app/global.R` | Package loading |
| `app/ui.R` | User interface |
| `app/server.R` | Server logic and the helper functions `get_table`, `make_table` and `search_table` |
| `app/www/` | Images shown in the help dialogs |
| `tests/` | testthat tests |

## Tests

The tests cover the helper functions, the UI and the server logic (using `shiny::testServer`). They need `testthat` in addition to the packages above:

```bash
Rscript tests/testthat.R
```

They run automatically on every push and pull request through GitHub Actions (R 4.5.2).
