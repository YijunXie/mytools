ui <- fluidPage(
	titlePanel("GapMinder",windowTitle = "STAT 547 HW 08"),
	fluidRow(
		
		# present results
		column(12,
					 plotOutput("plots")
		)
	),
	
	
		# control inputs
		column(4,offset = 1,
			wellPanel(
				#sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
				selectInput("yearInput", "Year",
										choices = c("1952", "1957", "1962", "1967","1972","1977",
																"1982","1987","1992","1997","2002","2007")),
				radioButtons("variableInput", "Variable of Interest",
										 choices = c("GDP Per Captital", "Life Expectancy",
										 						"Population"),
										 selected = "GDP Per Captital")
			)
		),
	 		
	# present the table of min and max of each continent
	column(6,offset = 1,
				 strong(textOutput("texts")),
				 br(),
				 tableOutput("results")
		)
)