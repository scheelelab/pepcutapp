
make_table <- function(seqx, input, dfx, cutdf) {
		
			lns <- nchar(seqx())
			if (is.null(input$outtab_rows_selected)) { 
				charcl <- stringr::str_split(dfx()[[3]], " ")
				# getting information on desired ezymes
				charcl <- lapply(charcl, FUN = function(x) { as.numeric(x) } )
				names(charcl) <- dfx()[[1]]
				
			} else {
				charcl <- stringr::str_split(dfx()[[3]][ unique(c(1,2,input$outtab_rows_selected)) ] , " ")
				# getting information on desired ezymes
				charcl <- lapply(charcl, FUN = function(x) { as.numeric(x) } )
				names(charcl) <- dfx()[[1]][unique(c(1,2,input$outtab_rows_selected))]
			}
			# making a cleavage-site-to-enzyme lookup dict
			g <- list()
			for (i in names(charcl) ) { for (ii in as.character(charcl[[i]])) {if (ii %in% names(g)) {g[[ii]] <- c(g[[ii]], i) } else { g[[ii]] <- i }  } }
		
			# all combinations of enzymes
			lst <- combn( sort( as.numeric( names(g) ) ), 2 )
		
			out <- data.frame( do.call( rbind, lapply(1:dim(lst)[[2]], FUN=function(x) {	start <- lst[,x][[1]]
														end <- lst[,x][[2]]
														
														#if ( start == 1 & end == lns) {
														
														#	return( NULL) 
														
														#} else {
														
															c(
																substr(seqx(), start+1, end )  ,
																start,
																end,
																paste( g[[as.character(start)]], collapse=";" ),
																paste( g[[as.character(end)]], collapse=";" )
															)
														#}
					})))
			# providing names I want
			names(out) <- c("Peptide", "Start", "End", "Start_Enzymes", "End_enzymes")

			cutdf( out )
		}


get_table <- function(input, seqx, dfx, cutdf, searchdf) {
		#reading protein sequence
		x <- stringr::str_split_1(input$prot, " ")
		seqx(  stringr::str_remove_all( paste(x[is.na(as.numeric(x))],collapse=""), "\n"))
		
		# reading Enzyme table
		df <- data.frame(do.call(rbind, lapply( stringr::str_split_1(input$tab, "\n"), FUN=function(x){stringr::str_split_1(x, "\t")}  )))
		names(df) <- unname(unlist(df[1,]))
		df <- df[setdiff( seq_len(nrow(df)), 1),]
		df <- rbind( c("None_end", 0, nchar(seqx())), df)
		df <- rbind( c("None_start", 0, 0), df)
		rownames(df) <- 1:nrow(df)
		dfx( df )
		searchdf(NULL)
		cutdf(NULL)
		}


search_table <- function(input, dfx, seqx, searchdf) {
		lns <- nchar(seqx())
		# getiing information far all ezymes
		charcl <- stringr::str_split(dfx()[[3]], " ")
		#charcl <- lapply(charcl, as.numeric)
		charcl <- lapply(charcl, FUN = function(x) { as.numeric(x) } )
		
		names(charcl) <- dfx()[[1]]
		# making a cleavage-site-to-enzyme lookup dict
		g <- list()
		for (i in names(charcl) ) { for (ii in as.character(charcl[[i]])) {if (ii %in% names(g)) {g[[ii]] <- c(g[[ii]], i) } else { g[[ii]] <- i }  } }

		# reading comma-sep peptides
		p <- stringr::str_split_1(input$search, ",")
		
		# finding all peptides in protein
		xx <- sapply(p, FUN=function(x) {stringr::str_locate_all(seqx(), x) } )
		
		# combing result to table
		df <- do.call( rbind, lapply(names(xx), FUN=function(x) { if (dim(xx[[x]])[[1]] > 0) { d <- data.frame(xx[[x]]); d[["Peptide"]] <- x; return(d) }  } ) )
		df[["start"]] <- df[["start"]] - 1
		df[["Start_Enzymes"]] <- apply(df["start"], MARGIN = 1, FUN = function(x) {paste(g[[as.character(x)]], collapse = ";")} )
		df[["End_enzymes"]] <- apply(df["end"], MARGIN = 1, FUN = function(x) {paste(g[[as.character(x)]], collapse = ";")} )
		
		names(df) <- c("Start", "End", "Peptide", "Start_Enzymes", "End_enzymes")
		df <- df[complete.cases(df),]
		searchdf( df[c("Peptide", "Start", "End", "Start_Enzymes", "End_enzymes")])
		}


shinyServer(function(input, output, session) {

	seqx <- reactiveVal()
	dfx <- reactiveVal()
	cutdf <- reactiveVal()
	searchdf <- reactiveVal()

	observeEvent(input$back, {
		showModal(modalDialog(
        title = "Right click and click 'Back'",
        {
          tags$figure(
                       class = "centerFigure",
                       tags$img(#id = "b1",
                       src = "back.png",
					   height = "100%",
                       width = "100%", #style="margin-left: auto;margin-right: auto;"
                      ))
        }, size = "l",
        ))
	})

	observeEvent(input$forw, {
		showModal(modalDialog(
        title = "Right click and click 'Forward'",
        {
          tags$figure(
                       class = "centerFigure",
                       tags$img(#id = "b1",
                       src = "forward.png",
					   height = "100%",
                       width = "100%", #style="margin-left: auto;margin-right: auto;"
                      ))
        }, size = "l",
        ))
	})

	observeEvent(input$showimg1,{
	
		showModal(modalDialog(
        title = "Replicate marking and copy - no more no less",
        {
          tags$figure(
                       class = "centerFigure",
                       tags$img(#id = "b1",
                       src = "prsqtbl.PNG",
					   height = "100%",
                       width = "100%", #style="margin-left: auto;margin-right: auto;"
                      ))
        }, size = "l",
        ))
	
	})
	
	
	observeEvent(input$showimg2,{
	
		showModal(modalDialog(
        title = "Replicate marking and copy - no more no less",
        {
          tags$figure(
                       class = "centerFigure",
                       tags$img(#id = "b1",
                       src = "prcltbls.PNG",
					   height = "100%",
                       width = "100%", #style="margin-left: auto;margin-right: auto;"
                      ))
        }, size = "l",
        ))
	
	})


	observeEvent(input$clicko,{
		
		req(input$prot)
		req(input$tab)
		get_table(input, seqx, dfx, cutdf, searchdf)
		
	})

	observeEvent(dfx(), {
	
		if (is.null(dfx())) { 
		shinyjs::hide("al")
		} else {
		shinyjs::show("al")
		}

	}, ignoreNULL = F)


	#paste0( paste( seq_len(nchar(seqx())-1), collapse = " " ), "\n", paste( stringr::str_split(seqx(), pattern = "")[[1]], collapse = " ") )
	output$out <- renderText( { paste0( "Length: ", nchar(seqx()), "\n", seqx()) } )
	
	output$outtab <- DT::renderDataTable(DT::datatable({ dfx() }, 	extensions = "Buttons", class = "display", 
																	options = list(
																		paging = TRUE,
																		searching = TRUE,
																		fixedColumns = TRUE,
																		autoWidth = TRUE,
																		ordering = TRUE,
																		dom = 'Blrtip',
																		buttons = c('copy', 'csv', 'excel'), 
																		pageLength = 25, lengthMenu=c(5,10,25, 50, 100) ) ))


	observeEvent(input$slic, {
	
		#req(input$outtab_rows_selected)
		req(seqx())
		make_table(seqx, input, dfx, cutdf)
	
	})
	
	
	observeEvent(cutdf(), {
	
		if (is.null( cutdf() )) { 
		shinyjs::hide("sliced")
		} else {
		shinyjs::show("sliced")
		}

	}, ignoreNULL = F)


	output$cutt <- DT::renderDataTable(DT::datatable({ cutdf() }, #extensions = "Buttons", class = "display", 
																	options = list(
																		paging = TRUE,
																		searching = TRUE,
																		fixedColumns = TRUE,
																		autoWidth = TRUE,
																		ordering = TRUE,
																		#dom = 'Blrtip',
																		#buttons = c('copy', 'csv', 'excel'), 
																		pageLength = 10, lengthMenu=c(5,10,25, 50, 100) ) ))


	output$dl <- downloadHandler(
		filename = function() {
		  paste("data-", Sys.Date(), ".xlsx", sep="")
		},
		content = function(file) {
		  writexl::write_xlsx(cutdf(), file)
		  #write.csv(data, file)
		}
  )


	observeEvent(input$searchbut, {
	
		req(input$search)
		search_table( input, dfx, seqx, searchdf )
		
	})

	output$maybe <- DT::renderDataTable(DT::datatable({ searchdf() }, extensions = "Buttons", class = "display", 
																	options = list(
																		paging = TRUE,
																		searching = TRUE,
																		fixedColumns = TRUE,
																		autoWidth = TRUE,
																		ordering = TRUE,
																		dom = 'Blrtip',
																		buttons = c('copy', 'csv', 'excel'), pageLength = 10, lengthMenu=c(5,10,25, 50, 100) ) ))
																		
})