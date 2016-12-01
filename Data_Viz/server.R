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
    				value = rep(1, input$Nodes),
    				group = c(rep("A", floor(input$Nodes/2)), rep("B", input$Nodes - floor(input$Nodes/2))),
    				level = c(rep(1, floor(input$Nodes/2)), rep(2, input$Nodes - floor(input$Nodes/2)))
    )

    
    edges <- data.frame(
    				from = unlist(rand_graph_edges$from),
    				to = unlist(rand_graph_edges$to), 
    				title = paste("Edge", 1:dim(rand_graph_edges)[1]),
    				length = rep(200, dim(rand_graph_edges)[1]),
    				width = c(rep(10, floor(dim(rand_graph_edges)[1]/2)), rep(1, dim(rand_graph_edges)[1] - floor(dim(rand_graph_edges)[1]/2)))
    )
    
    #graph visualization and options
    visNetwork(nodes, edges, main = "Social Network", height = "500px", width = "100%") %>%
    visIgraphLayout() %>%
    visPhysics(stabilization = FALSE) %>%
    # visHierarchicalLayout() %>%
    visLayout(improvedLayout = TRUE) %>%
    visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE) %>%
    visEdges(arrows = 'from', smooth = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE, selectedBy = "group")
    
    
    #NEED TO ADD: INDIVIDUAL COLORING/IMAGE FOR NODES, OTHER NETWORK VISUALIZATIONS
  })
  
  # observe({
  # 	visNetworkProxy("network_proxy_nodes") %>%
  # 	visFit(nodes = c(1:min(10, input$Nodes)))
  # })
  
})
