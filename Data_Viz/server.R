#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$network <- renderVisNetwork({
  	
    #defining the graph nodes and edges
    set.seed(input$Seed)
  	
    rand_graph = sample_gnp(input$Nodes, input$Max_Deg, directed = FALSE, loops = FALSE)
    rand_graph_edges = as_data_frame(rand_graph, what = "edges")
  	
  	
    nodes <- data.frame(
    				id = 1:input$Nodes, 
    				label = paste("Node", 1:(input$Nodes)),
    				value = rep(1, input$Nodes)
    )
    
    edges <- data.frame(
    				from = unlist(rand_graph_edges$from),
    				to = unlist(rand_graph_edges$to), 
    				title = paste("Edge", 1:dim(rand_graph_edges)[1]),
    				length = rep(200, dim(rand_graph_edges)[1])
    )
    
    #graph visualization and options
    visNetwork(nodes, edges, main = "Social Network", width = "10%") %>%
    visHierarchicalLayout(direction = "LR") %>%
    visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE) %>%
    visEdges(arrows = 'from', smooth = FALSE)
    
    
    #NEED TO ADD: GRAPH WEIGHTS, INDIVIDUAL COLORING/IMAGE FOR NODES, TABS, OTHER NETWORK VISUALIZATIONS
  })
  
})
