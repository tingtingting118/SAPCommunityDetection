# Ali Tafti
# Optional Advanced Lab 2: SAP Community Network
#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"C:/Users/karen/Downloads/UIC/IDS564-social medi/advanced lab/SAP community knowlege"
setwd(dir_path)


# clear everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# ---
# [1] "data.frame"
# ---
# Describe the data frame
str(el)

# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_SAPSub)
## Vertices
vcount(g_SAPSub)


## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)
#Is it a simple graph? No!
# ---
#[1] FALSE
# ---

# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

# Edges
ecount(g_SAPSub_simpl)
## Vertices
vcount(g_SAPSub_simpl)

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <-inv_weight

# You can see the neighbors of some selected nodes
neighbors(g_SAPSub_simpl, v=c('900'),mode='in')
neighbors(g_SAPSub_simpl, v=c('592540'),mode = 'out')
neighbors(g_SAPSub_simpl,v=c('592540'),mode='in')
neighbors(g_SAPSub_simpl,v=c('160'),mode='in')

par(mfrow=c(2,1))

plot(degree(g_SAPSub_simpl,mode='in'),xlab='node index',ylab='number of in-path',col='blue')
plot(degree(g_SAPSub_simpl,mode='out'),xlab='node index',ylab='number of out-path',col='red')


#degree of the nodes
hist(degree(g_SAPSub_simpl,mode = 'in'),xlim=c(0,100),xlab="Vertex degree",main="in-path  distribution",col='pink')
hist(graph.strength(g_SAPSub_simpl,mode='out'),xlab='vertex Strength',main='out-path distribution',col='blue')






## For example, there is only a path to 814 from 511; not from it. So there are outpaths from 511 to 814, but no in paths. (or the other vectors)
E(g_SAPSub_simpl)$weight <- inv_weight

reciprocity(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")


# find out giant component 
comps<-decompose.graph(g_SAPSub_simpl)
table(sapply(comps, vcount))

gc<-decompose.graph(g_SAPSub_simpl)[[1]]
average.path.length(gc,directed=TRUE)
transitivity(gc)
diameter(gc)
g.cut<-articulation.points(gc)
length(g.cut)
# Diameter with both kinds of weights


#dyad detection
dyad.census(g_SAPSub_simpl)
# this means that adecided one-sidedness to the manner in which blogs in this network reference each other.
#only 12 of them are helping each other



# Clustering
transitivity(g_SAPSub_simpl, weights = inv_weight)

# Avg. path length and diameter
average.path.length(g_SAPSub_simpl, directed=TRUE)
diameter(g_SAPSub_simpl)
diameter(g_SAPSub_simpl, weights= num_weight)
diameter(g_SAPSub_simpl, weights= inv_weight)
# Summarize the graph structure
summary(g_SAPSub_simpl)

# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))

A <- get.adjacency(g_SAPSub_simpl, sparse=FALSE)
library(network)
g <- network::as.network.matrix(A)
library(sna)
sna::gplot.target(g, degree(g), main="Degree",circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"), edge.col="darkgray")

cliques(g_SAPSub_simpl)[sapply(cliques(g_SAPSub_simpl), length) == 5]
# Can try either of these weighting schemes for various measures; they change the interpretation of the measures
# Inverse weight
E(g_SAPSub_simpl)$weight <- inv_weight
# Regular weight
E(g_SAPSub_simpl)$weight <- num_weight

# Embeddedness/ inverse of structural hole access (see Burt 2004)
constraints_SAP <- round(constraint(g_SAPSub_simpl, nodes=V(g_SAPSub_simpl)), digits=4)
# Degree centrality
degree_sap <- degree(g_SAPSub_simpl)
# Node betweenness
betweens_SAP <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))

plot(betweens_SAP,main='betweeness centrality',col='red')
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_SAPSub_simpl, e=E(g_SAPSub_simpl), directed = TRUE)
plot(edgebetweens_SAP,main='edge betweeness',col='pink')
# Local clustering coefficients
clustering_SAP <- transitivity(g_SAPSub_simpl, type="local", vids=V(g_SAPSub_simpl)) 

# Plots 1 and 2: Can run them together
par(mfrow=c(1, 2))
edge_frame<-data.frame(edgebetweens_SAP, num_weight, inv_weight)
a_edge<-aggregate(edgebetweens_SAP ~ inv_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
node_frame<-data.frame(betweens_SAP, constraints_SAP, clustering_SAP, degree_sap)
a_node<-aggregate(betweens_SAP ~ clustering_SAP, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Clustering", ylab="Average Betweenness of nodes")


# Plot set 2: Four plots 
par(mfrow=c(2, 2))
a_node<-aggregate(betweens_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Betweenness")
a_edge<-aggregate(edgebetweens_SAP ~ num_weight, data=edge_frame, mean)
plot(a_edge, col="blue", log="xy", xlab="Weight of edge", ylab="Average Betweenness of edges")
a_node<-aggregate(clustering_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Clustering")
a_node<-aggregate(constraints_SAP ~ degree_sap, data=node_frame, mean)
plot(a_node, col="blue", log="xy", xlab="Degree", ylab="Average Constraint (Embeddedness)")

# Log-log degree distributino
par(mfrow=c(1, 2))
d.net <-degree(g_SAPSub_simpl)
dd.net <- degree.distribution(g_SAPSub_simpl)
d <- 1:max(d.net)-1
ind <- (dd.net != 0)
plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

# CHUNK 8# Average neighbor degree versus vertex degree
a.nn.deg <- graph.knn(g_SAPSub_simpl,V(g_SAPSub_simpl))$knn
plot(d.net, a.nn.deg, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))







#neighbor
k.nbhds <- graph.neighborhood(g_SAPSub_simpl)
#check which nodes contain most neighbors
sapply(k.nbhds,vcount)




#layout of the network
set.seed(42)
l<-layout.kamada.kawai(g_SAPSub_simpl)

igraph.options(vertex.size=10)

par(mfrow=c(1,1))


#V(g_SAPSub_simpl)$label<-sub("user","",V(g_SAPSub_simpl))
#leader of the nodes
#V(g_SAPSub_simpl)[c('592540','22328','3510478', '2704623', '3552437')]$shape <-'rectangle'
V(g_SAPSub_simpl)[c('592540','22328','3510478', '2704623', '3552437')]$color<-c('red','dodgerblue','pink','blue','green')
V(g_SAPSub_simpl)$shape<-'circle'
plot(g_SAPSub_simpl,layout=l,vertex.label=NA)
V(g_SAPSub_simpl)[c('592540')]$size<-15

#weights of the edges
E(g_SAPSub_simpl)$weight<-inv_weight
E(g_SAPSub_simpl)$width<-E(g_SAPSub_simpl)$weight
F1<-V(g_SAPSub_simpl)[neighbors(g_SAPSub_simpl, v=c('592540'))]
F2<-V(g_SAPSub_simpl)[neighbors(g_SAPSub_simpl,v=c('22328'))]
F3<-V(g_SAPSub_simpl)[neighbors(g_SAPSub_simpl,v=c('3510478'))]
F4<-V(g_SAPSub_simpl)[neighbors(g_SAPSub_simpl,v=c('2704623'))]
F5<-V(g_SAPSub_simpl)[neighbors(g_SAPSub_simpl,v=c('3552437'))]


F1$size<-20
F2$size<-20
F3$size<-20
F4$size<-20
F5$size<-20

E(g_SAPSub_simpl)[F1%--%F1]$color<-'lightblue'
E(g_SAPSub_simpl)[F2%--%F2]$color<-'pink'
E(g_SAPSub_simpl)[F3%--%F3]$color<-'yellow'
E(g_SAPSub_simpl)[F4%--%F4]$color<-'darkblue'
E(g_SAPSub_simpl)[F5%--%F5]$color<-'red'
E(g_SAPSub_simpl)[F1%--%F2]$color<-'green'


# Offset vertex labels for smaller points (default=0).
#V(g_SAPSub_simpl)$size<-graph.strength(g_SAPSub_simpl)
summary(graph.strength(g_SAPSub_simpl))
V(g_SAPSub_simpl)$label.dist<-ifelse(V(g_SAPSub_simpl)$size>=2.5,0,10)
V(g_SAPSub_simpl)$label.size<-2
V(g_SAPSub)[c('592540','22328','3510478', '2704623', '3552437')]$label.size<-10
plot(g_SAPSub_simpl,layout=l)



















