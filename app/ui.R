

ui <- fillPage(
	
	tags$head(
                  tags$style(HTML("
                                  body {
                                          height: 95vh;
										  overflow-y: auto;
										  overflow-x: hidden;
                                        }
                                        "))),


	div(#style = "height: 95vh; overflow-y: auto; overflow-x: auto;",
	
	fluidRow(
				column(1),
				column(10,
					textAreaInput("prot", "Protein sequence", width="100%", height="400px")
				),
				column(1)
			), 

	fluidRow(
				column(1),
				column(10,
					textAreaInput("tab", "Paste table", width="100%", height="400px")
				),
				column(1)
			), 
	fluidRow(
				column(5),
				column(2, align = "center",
					actionButton("clicko", "READ")
				),
				column(5)
			),
	fluidRow(
				column(2),
				column(8, align = "center",
					h4("Protein sequence:"),
					hr(),
					div(style = "overflow-y: auto; overflow-x: auto;",
						textOutput("out")
					)
				),
				column(2)
			),
	
	fluidRow(
				column(2),
				column(8, align = "center",
				h4("Data table (click me):"),
					hr(),
					div(style = "overflow-y: auto; overflow-x: auto;",
						DT::dataTableOutput("outtab")
					)
				),
				column(2)
			),
			
			fluidRow(
				column(5),
				column(2, align = "center",
					actionButton("slic", "Slice me up!")
				),
				column(5)
			),
			
	fluidRow(
				column(1),
				column(10, align = "center",
				h4("Possible peptides"),
					hr(),
					div(style = "overflow-y: auto; overflow-x: auto;",
						DT::dataTableOutput("cutt")
					)
				),
				column(1)
			),
			
	fluidRow(
				column(1),
				column(10, align = "center",
				h4("Look up petides"),
				hr(),
				textAreaInput("search", "Insert comma seperated peptides", width="80%", height="200px"),
				actionButton("searchbut", "SEARCH"),
				div(style = "overflow-y: auto; overflow-x: auto;",
						DT::dataTableOutput("maybe")
					)
				),
				column(1)
			),
    )
)