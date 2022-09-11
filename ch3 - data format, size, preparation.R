# https://kateto.net/sunbelt2021#data-format-size-and-preparation

# ch3.1 - Dataset 1: edgelist

nodes <- read.csv("./data/Dataset1-Media-example-NODES.csv", header = T, as.is = T)
links <- read.csv("./data/Dataset1-Media-Example-Edges.csv", header = T, as.is = T)

head(nodes)
head(links)

# ch3.2 - Creating an igraph object

library('igraph')
net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)
net

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find nodes and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[media=="BBC"]
E(net)[type=="mention"]


# Get an edge list or a matrix:
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight") # gives matrix of sum of weights from node (row) to node (col)

# You can also examine the network adjacency matrix directly:
net[1,]
net[5,7]
### what are these two lines telling us??????


# Or data frames describing nodes and edges:
as_data_frame(net, what="edges") # gives us back initial "links" df
as_data_frame(net, what="vertices") # gives us back initial "nodes" df

# Create starter network graph
plot(net)

# clean it up by removing loops
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# reduce size of arrowheads, remove labels
plot(net, edge.arrow.size = 0.4, vertex.label = NA)

#ch3.3 - Dataset 2: matrix

nodes2 <- read.csv("./data/Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("./data/Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

head(nodes2)
head(links2)

links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)
