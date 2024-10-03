


shinyServer(function(input, output, session) {

	seqx <- reactiveVal()
	dfx <- reactiveVal()
	cutdf <- reactiveVal()

	observeEvent(input$clicko,{
		
		req(input$prot)
		req(input$tab)

		
		#reading protein sequence
		x <- stringr::str_split_1(input$prot, " ")
		seqx( stringr::str_remove_all( paste(x[is.na(as.numeric(x))],collapse=""), "\n"))
		
		# reading Enzyme table
		df <- data.frame(do.call(rbind, lapply( stringr::str_split_1(input$tab, "\n"), FUN=function(x){stringr::str_split_1(x, "\t")}  )))
		names(df) <- unname(unlist(df[1,]))
		dfx( df[setdiff( seq_len(nrow(df)), 1),] )
		
	})


	output$out <- renderText( {seqx()} )
	
	output$outtab <- DT::renderDataTable(DT::datatable({ dfx() }, 	extensions = "Buttons", class = "display", 
																	options = list(
																		paging = TRUE,
																		searching = TRUE,
																		fixedColumns = TRUE,
																		autoWidth = TRUE,
																		ordering = TRUE,
																		dom = 'Blrtip',
																		buttons = c('copy', 'csv', 'excel'), pageLength = 25, lengthMenu=c(5,10,25, 50, 100, 1000, 10000) ) ))


	observeEvent(input$slic, {
	
		req(input$outtab_rows_selected)
		
		# getiing information on desired ezymes
		charcl <- stringr::str_split(dfx()[[3]][input$outtab_rows_selected], " ")
		charcl <- lapply(charcl, as.numeric)
		names(charcl) <- dfx()[[1]][input$outtab_rows_selected]

		# making a cleavage-site-to-enzyme lookup dict
		g <- list()
		for (i in names(charcl) ) { for (ii in as.character(charcl[[i]])) {if (ii %in% names(g)) {g[[ii]] <- c(g[[ii]], i) } else { g[[ii]] <- i }  } }
	
		# all combinations of enzymes
		lst <- combn( sort( as.numeric( names(g) ) ), 2 )
	
		out <- data.frame( do.call( rbind, lapply(1:dim(lst)[[2]], FUN=function(x) {	start <- lst[,x][[1]]
													end <- lst[,x][[2]]
													c(
														substr(seqx(), start, end )  ,
														start,
														end,
														paste( g[[as.character(start)]], collapse=";" ),
														paste( g[[as.character(end)]], collapse=";" )
													)
				})))
		# providing names I want
		names(out) <- c("Peptide", "Start", "End", "Start_Enzymes", "End_enzymes")

		cutdf( out )
		
	
	})


	output$cutt <- DT::renderDataTable(DT::datatable({ cutdf() }, extensions = "Buttons", class = "display", 
																	options = list(
																		paging = TRUE,
																		searching = TRUE,
																		fixedColumns = TRUE,
																		autoWidth = TRUE,
																		ordering = TRUE,
																		dom = 'Blrtip',
																		buttons = c('copy', 'csv', 'excel'), pageLength = 10, lengthMenu=c(5,10,25, 50, 100, 1000, 10000) ) ))


	observeEvent(input$searchbut, {
	
		req(input$search)
		
		# getiing information far all ezymes
		charcl <- stringr::str_split(dfx()[[3]], " ")
		charcl <- lapply(charcl, as.numeric)
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
		
		
		df[["Start_Enzymes"]] <- apply(df["start"], MARGIN = 1, FUN = function(x) {paste(g[[as.character(x)]], collapse = ";")} )
		df[["End_enzymes"]] <- apply(df["end"], MARGIN = 1, FUN = function(x) {paste(g[[as.character(x)]], collapse = ";")} )
		
		
		names(df) <- c("Start", "End", "Peptide", "Start_Enzymes", "End_enzymes")
		
		#cutdf()[ cutdf()[["Peptide"]] %in% p, ]
		output$maybe <- DT::renderDataTable(DT::datatable({ df[c("Peptide", "Start", "End", "Start_Enzymes", "End_enzymes")] }, extensions = "Buttons", class = "display", 
																	options = list(
																		paging = TRUE,
																		searching = TRUE,
																		fixedColumns = TRUE,
																		autoWidth = TRUE,
																		ordering = TRUE,
																		dom = 'Blrtip',
																		buttons = c('copy', 'csv', 'excel'), pageLength = 10, lengthMenu=c(5,10,25, 50, 100, 1000, 10000) ) ))
																		
																		
																		
		
	
	})



})