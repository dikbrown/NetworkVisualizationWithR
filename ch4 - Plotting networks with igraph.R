# https://kateto.net/sunbelt2021#plotting-networks-with-igraph

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
# Note that using curved edges will allow you to see multiple links
# between two nodes (e.g. links going in either direction, or multiplex links)
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to light gray, the node & border color to orange 
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black") 

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"

# We can even set the network layout:
graph_attr(net, "layout") <- layout_with_lgl # layout_with_lgl is a layout generator for larger graphs

    # Some other layouts include:
    #   layout_as_star
    #   layout_as_bipartite
    #   layout_as_tree
    #   layout_in_circle

plot(net) 

plot(net, edge.color="orange", vertex.color="gray50") 

### Add legend
plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

# Plot only labels
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

# Make edge color dependent on source node color
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)


######
## 4.2 Network Layouts
######

net.bg <- sample_pa(100) 
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- "" 
E(net.bg)$arrow.mode <- 0
plot(net.bg)

plot(net.bg, layout=layout_randomly)

l <- layout_in_circle(net.bg)
plot(net.bg, layout=l)

## Look at all available layouts
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(5,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }
dev.off()

######
## 4.3 Highlighting Aspects of the Network
######

hist(links$weight) # roughly bimodal: many between 0-5 and between 20-25, not many between 5-20
mean(links$weight) # mean edge weight
sd(links$weight)

cut.off <- mean(links$weight) 
net.sp <- delete_edges(net, E(net)[weight<cut.off]) # get rid of low-weight edges
plot(net.sp, layout=layout_with_kk)


par(mfrow=c(1,2))

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(net)
class(clp)

# Community detection returns an object of class "communities" 
# which igraph knows how to plot: 
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])
dev.off()


##############
## 4.4 Highlighting Specific Nodes or Links
##############

dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], 
                           to=V(net), weights=NA)

# Set colors to plot the distances:
#   darkest red = NYT
#   one-away: brick red
#   two-away: orange
#   three-away: yellow-gold
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
     vertex.label.color="white")
####
# Highlight a path (from NY Post to MSNBC)
news.path <- shortest_paths(net, 
                            from = V(net)[media=="MSNBC"], 
                            to  = V(net)[media=="New York Post"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0)


## Highlight edges going into or out of a vertex
#### use incident() for a single node
#### use incident_edges() for multiple nodes
inc.edges <- incident(net,  V(net)[media=="Wall Street Journal"], mode="all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(net))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"
plot(net, vertex.color=vcol, edge.color=ecol)


## Look at immediate neighbors
#### use neighbors() for all neighbors for a single node
#### use adjacent_vertices() for neighbors for multiple nodes
neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(net, vertex.color=vcol)


## Can "mark" node groups
par(mfrow=c(1,2))
plot(net, mark.groups=c(1,4,5,8), mark.col="#C5E5E7", mark.border=NA)

# Mark multiple groups:
plot(net, mark.groups=list(c(1,4,5,8), c(15:17)), 
     mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)
dev.off()

##############
## 4.5 Interactive plotting with tkplot
##############
#tweak layout by adjusting node locations

#create interactive graph in new plot window
tkid <- tkplot(net) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
l2 <- l
l2[1,2] <- 160 # was 204.8
par(mfrow = c(1,2))
plot(net, layout=l)
plot(net, layout = l2)
par(mfrow = c(1,1))


##############
## 4.6 Plotting Two-Mode Networks
##############

## Dataset 2
head(nodes2)
head(links2)

net2
plot(net2, vertex.label=NA)

# Media outlets are blue squares, audience nodes are orange circles:
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]

# Media outlets will have name labels, audience members will not:
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F] 
V(net2)$label.cex=.6
V(net2)$label.font=2

plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8) 

# special layout for bipartite networks - doesn't work great
plot(net2, vertex.label=NA, vertex.size=7, layout=layout.bipartite) 

## Use text as nodes
plot(net2, vertex.shape="none", vertex.label=nodes2$media,
     vertex.label.color=V(net2)$color, vertex.label.font=2, 
     vertex.label.cex=.6, edge.color="gray70",  edge.width=2)

## Use images as nodes
library('png')

img.1 <- readPNG("./data/images/news.png")
img.2 <- readPNG("./data/images/user.png")

V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

## add random image of a dog
plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

img.3 <- readPNG("./data/images/puppy.png")
rasterImage(img.3,  xleft=-1.6, xright=-0.6, ybottom=-1.1, ytop=-.4)

## Create bipartite projections for two-mode network
par(mfrow=c(1,2))

net2.bp <- bipartite.projection(net2) 

plot(net2.bp$proj1, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[!is.na(nodes2$media.type)])

plot(net2.bp$proj2, vertex.label.color="black", vertex.label.dist=1,
     vertex.label=nodes2$media[ is.na(nodes2$media.type)]) 
dev.off()

#################
## 4.7 Plotting Multiplex Networks
#################

E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.color="gray40", layout=layout_in_circle, edge.curved=.3)


net.m <- net - E(net)[E(net)$type=="hyperlink"] # another way to delete edges:
net.h <- net - E(net)[E(net)$type=="mention"]   # using the minus operator

# Plot the two links separately:
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", layout=layout_with_fr, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=layout_with_fr, main="Tie: Mention")

# Make sure the nodes stay in place in both plots:
## i.e., both graphs have same node positions
l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")
dev.off()


## Very simple network with two nodes and three edges
multigtr <- graph( edges=c(1,2, 1,2, 1,2), n=2 )
l <- layout_with_kk(multigtr)

# Let's just plot the graph:
plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white",
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5,
     edge.arrow.size=3, edge.curved=0.1, layout=l)
# can only see one edge because they all have same curvature

# Give each edge its own curvature
plot(multigtr, vertex.color="lightsteelblue", vertex.frame.color="white", 
     vertex.size=40, vertex.shape="circle", vertex.label=NA,
     edge.color=c("gold", "tomato", "yellowgreen"), edge.width=5,
     edge.arrow.size=3, edge.curved=curve_multiple(multigtr), layout=l) ### edge.curved is different

#### Detach package to prevent conflicts
detach('package:igraph')
