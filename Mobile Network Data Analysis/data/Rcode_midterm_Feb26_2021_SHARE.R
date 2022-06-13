###############################################################
# Ali Tafti, IDS 564, starter file

# clear everything out of memory
rm(list=ls())  

getwd()
# Set your working directory
# Save the data files to a location on your hard drive and specify the path here (Windows systems use forward slashes)
#dir_path <-"~/YourWorkingDirectoryFilePath"
#setwd("/Users/mine/")

## Load igraph package
library(igraph)

# load data from Stehle´ J, Voirin N, Barrat A, Cattuto C, Isella L, et al. (2011): doi:10.1371/journal.pone.0023176
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

###
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")
g_primschool_orig<-graph.data.frame(edge_frame, directed = FALSE)

med<-median(E(g_primschool_orig)$weight)

# This is the network that you will analyze: g_primschool_final
g_primschool_final<-delete.edges(g_primschool_orig, which(E(g_primschool_orig)$weight < med))

## For visualization purposes, the edge widths are set to the standardized value of weight.
E(g_primschool_final)$width <- (E(g_primschool_final)$weight - mean(E(g_primschool_final)$weight))/sd(E(g_primschool_final)$weight)


### This  happens to be the Fruchterman-Reingold, but you may choose any layout algorithm by changing the optional setting below
plot(g_primschool_final,  vertex.size=3, vertex.label= NA, layout=layout_with_fr)

V(g_primschool_final)$color <- "red"

plot(g_primschool_final,  vertex.size=0.05*sqrt(strength(g_primschool_final)), vertex.label= V(g_primschool_final)$names,vertex.label.cex=c(0.25), layout=layout_with_fr)
plot(g_primschool_final,  vertex.size=0.05*sqrt(strength(g_primschool_final)), vertex.label= V(g_primschool_final)$names,vertex.label.cex=c(0.25), layout=layout.fruchterman.reingold)
plot(g_primschool_final,  vertex.size=0.05*sqrt(strength(g_primschool_final)), vertex.label= V(g_primschool_final)$names,vertex.label.cex=c(0.2), layout=lay)




##################################################################

#the strongest edges 1780-1833
E(g_primschool_final)[34]
edge.betweenness(g_primschool_final)[34]

#The Weakest edges
#edge 1700-1866
E(g_primschool_final)[939]
E1700and1866<-edge.betweenness(g_primschool_final)[939]

#edge 1665-1761
E(g_primschool_final)[1376]
E1665and1761<-edge.betweenness(g_primschool_final)[1376]

#edge 1439-1890
E(g_primschool_final)[2225]
E1439and1890<-edge.betweenness(g_primschool_final)[2225]




x56 <- data.frame(E1700and1866,E1665and1761,E1439and1890)

View(x56)


########################
#nodes in weak edges
V(g_primschool_final)$degree1 <- degree(g_primschool_final)[1700]
V(g_primschool_final)$degree1

##########################
#Question4
#?betweenness
#betweenness(g_primschool_final, v = V(g_primschool_final), weights = weight1)
#options(scipen=999)
#plot(g_primschool_final,vertex.label=V(g_primschool_final)$names, vertex.size = 5)
#Question5
#g_acq_scc <-g_primschool_final - vertices('814', '925', '928')
#inv_weight_scc<-1/log(E(g_acq_scc)$weight  + 1)
#closeness(g_acq_scc, weights = inv_weight_scc, mode = "out")
#round(closeness(g_acq_scc, mode=c("out"), v=c('511', '541', '518', '519'), weights=inv_weight_scc), 3)   

###########33
weight1<-E(g_primschool_final)$weight
inv_weight1<-1/log(E(g_primschool_final)$weight  + 1)

V(g_primschool_final)$closeness <- closeness(g_primschool_final,weights=inv_weight1)                  # Closeness centrality
V(g_primschool_final)$betweenness <- betweenness(g_primschool_final, v = V(g_primschool_final), weights = weight1 )              # Vertex betweenness centrality
plot(g_primschool_final,vertex.label=V(g_primschool_final)$names, vertex.size = 5)

V(g_primschool_final)$betweenness

V(g_primschool_final)$color <- "yellow"
plot(g_primschool_final,  vertex.size=0.25*sqrt(V(g_primschool_final)$betweenness), vertex.label= V(g_primschool_final)$names,vertex.label.cex=c(0.2), layout=layout_with_fr)

plot(g_primschool_final,  vertex.size=0.25*sqrt(V(g_primschool_final)$betweenness), vertex.label= V(g_primschool_final)$names,vertex.label.cex=c(0.2), layout=layout.fruchterman.reingold)
plot(g_primschool_final,  vertex.size=0.4*sqrt(V(g_primschool_final)$betweenness), vertex.label= V(g_primschool_final)$names,vertex.label.cex=c(0.2), layout=lay)





centrality <- data.frame(row.names   = V(g_primschool_final)$name,
                         closeness   = V(g_primschool_final)$closeness,
                         betweenness = V(g_primschool_final)$betweenness)

View(centrality)
##################################################################
V(g_primschool_final)$name
E(g_primschool_final)$name

vcount(g_primschool_final)
ecount(g_primschool_final)

is_simple(g_primschool_final)

E(g_primschool_final)[3]

edge.betweenness(g_primschool_final)[1]

edge.betweenness(g_primschool_final)[1:10]

tail(edge.betweenness(g_primschool_final))

V(g_primschool_final)[3:4]

E(g_primschool_final)[1:30]

strength(g_primschool_final)[1]
E(g_primschool_final)[1]
V(g_primschool_final)[1]

nrow(g_primschool_final)
E(g_primschool_final)$weight

x1<- data.frame(edge.betweenness(g_primschool_final)[1:3125])
x2<-data.frame(E(g_primschool_final)$weight)

library(writexl)

write_xlsx(x1,"/Users/mine/Desktop/THIRD Semester/IDS 564.xlsx")
write_xlsx(x2,"/Users/mine/Desktop/THIRD Semester/IDS 564.xlsx")
###My part###My part###My part###My part###My part###My part###My part###My part
#library(igraph)

neighborhood_overlap(g_primschool_final,1789,1764)

#edge.betweenness.community (g_primschool_final, directed = FALSE,edge.betweenness = TRUE, merges = TRUE, bridges = TRUE)

#edge.betweenness.community.merges (g_primschool_final, edges)



#sub_net<-induced_subgraph(g_acq_simpl, v=c('511', '541' ))

#strength(g_primschool_final,vids = V(g_primschool_final),mode = c("all", "out", "in", "total"),loops = TRUE,weights = NULL)



#library(sand)
#data(g_primschool_final)
#hist(degree(g_primschool_final), col="lightblue", xlim=c(0,50),
     #xlab="Vertex Degree", ylab="Frequency", main="")

# CHUNK 2
hist(strength(g_primschool_final), col="pink",
     xlab="Vertex Strength", ylab="Frequency", main="")



ST <- read.table('edge2.csv', sep = ',') 

S = ST[,1] 
T = ST[,2] 

OVERLAP = as.data.frame(matrix(nrow = length(S),ncol = 2)) 


for (i in 2:length(S)) { 
  A = ST[i,1] 
  B = ST[i,2] 
  Connected_to_A = unique(ST[ST[,1] == A, 2]) 
  Connected_to_B = unique(ST[ST[,1] == B, 2]) 
  Union_AB = union(Connected_to_A, Connected_to_B) 
  Intersect_AB = intersect(Connected_to_A, Connected_to_B) 
  NAME = paste(as.character(A), " -> ", as.character(B)) 
  OVERLAP[i, 1] = NAME 
  OVERLAP[i, 2] = length(Intersect_AB)/length(Union_AB) 
}

# http://igraph.wikidot.com/community-detection-in-r
# "The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external"
# degrees of a community in order to quantify its significance. Let us call the edges within a 
# community "internal" and the edges connecting the vertices of a community with the rest of the graph "external".
# The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges 
# incident to a vertex of the community. More internal than external edges show that the community is significant; less 
# internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by 
# this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community."
help("wilcox.test")
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")
# Does edge weight make any difference here?

# Community detection using the Fast Greedy Algorithm
school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)
c.m.fastgreedy <- membership(school_comm_fast)
# Assignment to communities, based on class section or teacher status. This analysis can be extended to gender (see below).
table(c.m.fastgreedy, stud.class, useNA = c("no"))


plot(school_comm_fast,g_primschool, vertex.label=V(g_primschool)$classname,vertex.label.cex=c(0.8), vertex.size=2)
dendPlot(school_comm_fast)
#Diiferent Community Algorithms
#Walk Trap Algorithm


# Here, we are testing community significance for just two of the communities. Students will complete tests for the remainder of communities for each algorithm. 
v_comp1 <- V(g_primschool)[c.m.fastgreedy==1]
v_comp2 <- V(g_primschool)[c.m.fastgreedy==2]
v_comp3 <- V(g_primschool)[c.m.fastgreedy==3]
v_comp4 <- V(g_primschool)[c.m.fastgreedy==4]
v_comp5 <- V(g_primschool)[c.m.fastgreedy==5]
v_comp6 <- V(g_primschool)[c.m.fastgreedy==6]
v_comp7 <- V(g_primschool)[c.m.fastgreedy==7]
community.significance.test(g_primschool, v_comp1)
community.significance.test(g_primschool, v_comp2)
community.significance.test(g_primschool, v_comp3)
community.significance.test(g_primschool, v_comp4)
community.significance.test(g_primschool, v_comp5)
community.significance.test(g_primschool, v_comp6)
community.significance.test(g_primschool, v_comp7)


help("walktrap.community")
wc <- walktrap.community(g_primschool,weights=V(g_primschool)$weight,membership = TRUE)
wc.member <- membership(wc)
table(wc.member,stud.class,useNA = c("no"))
plot(wc,g_primschool,vertex.label=V(g_primschool)$classname,vertex.label.cex=c(0.8), vertex.size=2)
v_comp_wc1 <- V(g_primschool)[wc.member==1]
v_comp_wc2 <- V(g_primschool)[wc.member==2]
v_comp_wc3 <- V(g_primschool)[wc.member==3]
v_comp_wc4 <- V(g_primschool)[wc.member==4]
v_comp_wc5 <- V(g_primschool)[wc.member==5]
v_comp_wc6 <- V(g_primschool)[wc.member==6]
community.significance.test(g_primschool, v_comp_wc1)
community.significance.test(g_primschool, v_comp_wc2)
community.significance.test(g_primschool, v_comp_wc3)
community.significance.test(g_primschool, v_comp_wc4)
community.significance.test(g_primschool, v_comp_wc5)
community.significance.test(g_primschool, v_comp_wc6)

library(dplyr)
membership(wc)
length(wc)
sizes(wc)


#Label Propagation Community Algorithm

help("label.propagation.community")
lc <- label.propagation.community(g_primschool,weights=V(g_primschool)$weight)
lc.member <- membership(lc)
table(lc.member,stud.class,useNA = c("no"))
plot(lc,g_primschool,vertex.label= NA, vertex.size=2)







