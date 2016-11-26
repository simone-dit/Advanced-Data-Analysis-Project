#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)

nodes_max = 10
nodes_cur = 3

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hillary Clinton Data"),
  
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
    	
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
    
    # Show plot
    mainPanel(
       # plotOutput("ShowPlot")
       visNetworkOutput("network")
    )
  )
))
