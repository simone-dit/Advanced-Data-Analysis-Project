
library(shiny)
library(visNetwork)

nodes_max = 100
nodes_cur = 3

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hillary Clinton Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    	
    	  #for searching by typing name of node
    	  # sidebarSearchForm(textId = "searchText", label = "Search..."),
    	
       #randomization seed
    	  sliderInput("Seed",
    	  		    "Randomization Seed",
    	  		    min = 1,
    	  		    max = 999,
    	  		    value = 500),
    	
    	  #number of nodes
       sliderInput("Nodes",
                   "Number of Nodes",
                   min = 1,
                   max = nodes_max,
                   value = nodes_cur),
       
       #number degree
       sliderInput("Max_Deg",
       		    "Max Degree of Any Node as % of Total Nodes",
       		    min = 0,
       		    max = 1,
       		    step = 0.01,
       		    value = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    		tabsetPanel(
        		tabPanel("Network", visNetworkOutput("network")),
        		tabPanel("Another tab")
        	)
    )
  )
))
