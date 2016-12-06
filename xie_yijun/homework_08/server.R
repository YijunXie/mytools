library(ggplot2)
library(dplyr)
library(gapminder)
library(purrr)
server = function(input, output) {
	# filter gapmind with only selected year
	filtered <- reactive({
		gapminder %>%
			filter(year == input$yearInput)
	})
	# variable of interest
	va = reactive({input$variableInput})
	
	# generate box-plots
	output$plots <- renderPlot({
		if (is.null(filtered())) {
			return()
		}
		# plot the varaible of interest
		if(va() == "GDP Per Captital"){
			filtered() %>% ggplot(mapping = aes(x = continent, y = log(gdpPercap))) + 
							 geom_boxplot(aes(group = continent,color = continent))
		} else{
			if(va() == "Life Expectancy"){
				filtered() %>% ggplot(mapping = aes(x = continent, y = lifeExp)) + 
					geom_boxplot(aes(group = continent,color = continent))
			} else{
				if(va() == "Population"){
					filtered() %>% ggplot(mapping = aes(x = continent, y = log(pop))) + 
						geom_boxplot(aes(group = continent,color = continent))
				}
			}
		}
		
	})
	
	# generate the table of max and min in each continent
	output$results <- renderTable({
		# select the variable of interest
		if(va() == "GDP Per Captital"){
			tempdat = filtered() %>% 
				select(continent,country,gdpPercap)
		} else{
			if(va() == "Life Expectancy"){
				tempdat = filtered() %>% 
					select(continent,country,lifeExp)
			} else{
				if(va() == "Population"){
					tempdat = filtered() %>% 
						select(continent,country,pop)
				}
			}
		}
		# change the data type from factor to string
		tempdat = tempdat %>% map_if(is.factor,as.character) %>% as_data_frame
		# create subsets of each continent
		africa = tempdat %>% 
				filter(continent == "Africa")
		america = tempdat %>% 
				filter(continent == "Americas")
		asia = tempdat %>% 
				filter(continent == "Asia")
		europe = tempdat %>% 
				filter(continent == "Europe")
		oc = tempdat %>% 
				filter(continent == "Oceania")
		# min of each continent
		mins = c(min(africa[,3]),min(america[,3]),min(asia[,3]),
							min(europe[,3]),min(oc[,3]))
		# contries of mins
		minc = c(africa[,2][which.min(unlist(africa[,3])),],
						 america[,2][which.min(unlist(america[,3])),],
						 asia[,2][which.min(unlist(asia[,3])),],
						 europe[,2][which.min(unlist(europe[,3])),],
						 oc[,2][which.min(unlist(oc[,3])),])
		# max of each continent
		maxs = c(max(africa[,3]),max(america[,3]),max(asia[,3]),
						 max(europe[,3]),max(oc[,3]))
		# contries of maxs
		maxc = c(africa[,2][which.max(unlist(africa[,3])),],
						 america[,2][which.max(unlist(america[,3])),],
						 asia[,2][which.max(unlist(asia[,3])),],
						 europe[,2][which.max(unlist(europe[,3])),],
						 oc[,2][which.max(unlist(oc[,3])),])
		
		results = data.frame(continents = c("Africa","Americas","Asia",
																				"Europe","Oceania"),
												 Minumum = mins,
												 Country_min = as.character(minc),
												 Maximum = maxs,
												 Country_max = as.character(maxc))
		results
		})
	# title of the table
	output$texts = renderText({
		text = paste("The maximum and minimum", va(), "of each continent are",sep = " ")
	})
}