###################
#				  #
# SNA with iGraph #
#				  #
###################

h.edges = read.csv("hillary_edges.csv", header=T)
# dim(h.edges) 9306 4
h.nodes = read.csv("hillary_nodes.csv", header=T)
# dim(h.nodes) 513 4


## collapse all links of the same type between the same two nodes by summing their weights 
edge.for.graph = aggregate(h.edges[,3],h.edges[,-3],sum) 
dim(edge.for.graph) # 739 4

edge.for.graph = edge.for.graph[order(edge.for.graph$from,edge.for.graph$to),]
colnames(edge.for.graph)[4] = "weight"
rownames(edge.for.graph) <-  NULL

library(igraph)

net = graph_from_data_frame(d=edge.for.graph, vertices=h.nodes,directed=T)
class(net)
E(net)$type

plot(net, vertex.label=NA, vertex.size=3,edge.arrow.size=.3)

## simplify networks by removing loops
net = simplify(net, remove.multiple = F, remove.loops = T)

names(V(net))

V(net)$size =  (log(V(net)$active_size)+1)/1.5

colrs = c("tomato","gold","gray50")
V(net)$color = colrs[V(net)$person_type]

E(net)$width = log(E(net)$weight)/6

E(net)$arrow.size = 0.2
E(net)$edge.color = "gray80"

plot(net,vertex.label=NA)

layouts = grep("^layout_", ls("package:igraph"),value=T)[-1]
layouts = layouts[!grepl("bipartite|merge|norm|sugiyama|tree",layouts)]
# [1] "layout_as_star"       "layout_components"    "layout_in_circle"     "layout_nicely"       
# [5] "layout_on_grid"       "layout_on_sphere"     "layout_randomly"      "layout_with_dh"      
# [9] "layout_with_drl"      "layout_with_fr"       "layout_with_gem"      "layout_with_graphopt"
# [13] "layout_with_kk"       "layout_with_lgl"      "layout_with_mds"     

par(mfrow=c(3,5),mar=c(1/2,1/2,1/2,1/2))
for (layout in layouts){
	print(layout)
	l = do.call(layout,list(net))
	plot(net,vertex.label=NA,layout=l,main=layout)
}

### Choose:
	## "layout_nicely" "layout_with_dh" "layout_with_fr" "layout_with_mds"
	
	
	
#### Improving network plots


## Deleteing edges with low weight
summary(E(net)$weight)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    7.0    17.0    18.0   225.8    49.5 25640.0 

cut.off = 17
net.cut = delete.edges(net, E(net)[weight<cut.off])

nicely.l = layout_nicely(net.cut)
dh.l = layout_with_dh(net.cut)
mds.l = layout_with_mds(net.cut)
par(mfrow=c(1,3),mar=c(1/3,1/3,1/3,1/3))
plot(net.cut,layout=nicely.l, vertex.label=NA,main="layout_nicely")
plot(net.cut,layout=dh.l, vertex.label=NA, main="layout_with_dh")
plot(net.cut,layout=mds.l, vertex.label=NA,main="layout_with_mds")



##
net.r = net.cut - E(net.cut)[E(net.cut)$type=="sent"]
net.s = net.cut - E(net.cut)[E(net.cut)$type=="received"]

par(mfrow=c(1,2),mar=c(1,1,1,1))
plot(net.r, vertex.color = "orange", layout=mds.l, vertex.label=NA,main="Tie: Received (dms layout)")
plot(net.s, vertex.color = "lightsteelblue2", layout=mds.l, vertex.label=NA,main="Tie: Sent (dms layout)")