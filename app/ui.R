

ui <- fluidPage(theme = shinytheme("darkly"),
	
	useShinyjs(),
	
	tags$head(
                 tags$style(HTML("
                                  body {
                                          height: 95vh;
										  overflow-y: auto;
										  overflow-x: hidden;
										  background-color: #808080;
                                        }
                                        ")),
										
				tags$style(HTML("
                                  .modal-dialog.modal-lg {
                                    height: 70%;
									width: 80%;
                                  }
                                  ")),
										
			),

	div(#style = "height: 95vh; overflow-y: auto; overflow-x: auto;",
	
	tabsetPanel(type = 'tabs', id = "alltabs",
                            
                            tabPanel("PepCut", value = "tab0")),
	
	fluidRow(
		column(1),
		column(2, align = "left",
			actionButton("back", "HOW GO BACK", class = "btn btn-warning btn-lg btn-block")
		),
		column(6),
		column(2, align = "right",
			actionButton("forw", "HOW GO FORWARD", class = "btn btn-warning btn-lg btn-block")
		),
		column(1),
	),
	
	
	fluidRow(
				column(1, style="background-color:#404971; height:100%;"),
				column(10,
				
					tags$iframe(src = "https://web.expasy.org/peptide_cutter/", seamless=F, width="100%", height="800px", id="ifr", style="background-color:#FFFFFF")
				),
				column(1, style="background-color:#404971; height:100%;")
	),
	
	fluidRow(
				column(2),
				column(8, align = "center",
					
					HTML("<h2>Copy the <a href=# id='showimg1' class='action-button shiny-bound-input'>Protein sequence table </a> and the <a href=# id='showimg2' class='action-button shiny-bound-input'>Protease cleavage table</a> to the text fields below.</h2>"),
				),
				column(2)
			),
	
	br(),
	
	fluidRow(
				column(2),
				column(8,
					textAreaInput("prot", "paste 'protein sequence table'", width="100%", height="300px")
				),
				column(2)
			), 

	fluidRow(
				column(2),
				column(8,
					textAreaInput("tab", "Paste 'protease cleavage table'", width="100%", height="300px")
				),
				column(2)
			), 
	fluidRow(
				column(5),
				column(2, align = "center",
					actionButton("clicko", "READ DATA", class = "btn btn-success btn-lg btn-block")
				),
				column(5)
			),
	div( id="al", style="display:none;",
	fluidRow(
				column(2),
				column(8, align = "center",
					hr(),
					h4("Protein sequence:"),
					div(style = "overflow-y: auto; overflow-x: auto;",
						verbatimTextOutput("out")
					)
				),
				column(2)
			),
	
	fluidRow(
				column(1),
				column(10, align = "center",
					hr(),
					h4("Data table - use all or click on desired proteases - then 'SLICE IT UP!':"),
					div(style = "overflow-y: auto; overflow-x: auto;",
						DT::dataTableOutput("outtab")
					)
				),
				column(1)
			),
			br(),
			
			fluidRow(
				column(5),
				column(2, align = "center",
					actionButton("slic", "SLICE IT UP!", class = "btn btn-success btn-lg btn-block")
				),
				column(5)
			),
			div(id="sliced", style="display:none;",
	fluidRow(
				column(1),
				column(10, align = "center",
					hr(),
					h4("Possible peptides"),
					div(style = "overflow-y: auto; overflow-x: auto;",
						DT::dataTableOutput("cutt")
					)
				),
				column(1)
			),
			br(),
			fluidRow(
				column(5),
				column(2, align = "center",
					downloadButton("dl", "GET TABLE", class = "btn btn-success btn-lg btn-block")
				),
				column(5)
			),
			
			
	fluidRow(
				column(1),
				column(10, align = "center",
				hr(),
				h4("Look up petides"),
				p("Inster"),
				textAreaInput("search", "Insert comma seperated peptides", width="80%", height="200px", placeholder = "HAHAHA,KVKVGVN,RDGRGALQNIIPASTGAAKAV"),
				div(style = "overflow-y: auto; overflow-x: auto;",
						DT::dataTableOutput("maybe")
					)
				),
				column(1)
			),
	
	fluidRow(
				column(5),
				column(2, align = "center",
					actionButton("searchbut", "SEARCH", class = "btn btn-success btn-lg btn-block"),
				),
				column(5)
			),
	)),
	br(),
    )
)