#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(igraph)

# Define server logic required to graph
shinyServer(function(input, output) {
	
	
  output$network <- renderVisNetwork({
    set.seed(input$Seed)
  	
    rand_graph = sample_gnp(input$Nodes, input$Max_Deg, directed = FALSE, loops = FALSE)
    rand_graph_edges = as_data_frame(rand_graph, what = "edges")
  	
  	
    nodes <- data.frame(
    				id = 1:input$Nodes, 
    				label = paste("Node", 1:(input$Nodes))
    )
    
    edges <- data.frame(
    				from = unlist(rand_graph_edges$from),
    				to = unlist(rand_graph_edges$to), 
    				title = paste("Edge", 1:dim(rand_graph_edges)[1])
    	)
    
    visNetwork(nodes, edges, main = "Social Network") %>% 
    visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE) %>%
    visEdges(arrows = 'from') %>%
    visEdges(smooth = FALSE)

  })

   
  #not currently used
  output$ShowPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
  	
  })
  
})
