###############################################################
# Ali Tafti
# This script uses and modifies code from http://www.tb.ethz.ch/education/learningmaterials/modelingcourse/level-2-modules/network.html


# clear everything out of memory
rm(list=ls())  

#Read in the hs0 data over the internet using the read.table() function.
getwd()
# Set your working directory
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"~/YourWorkingDirectoryFilePath"
setwd(dir_path)
library(igraph)
################
# Function definition #
# Students: Please modify the function below according to the lab exercise instructions
################


# This one implements stats for susceptible as well as remove
# Also, make use of the susceptible again feature
# Please note that the only required argument for this function is the graph object. The other arguments can be overriden or skipped by the calling function; if skipped,
# the default values are used.
# Arguments: 1) network.i: graph object (required), 2) simlength: number of iterations (rounds) for the simulation, 3) p.t: probability of infecting a connected susceptible neighbor, if a node is infected
# 4) display_net: show the evolving network plots for each round of the simulation (press Enter to continue, q to quit). Set it to FALSE if you have a lot of 
# rounds and just want to collect the summary timestats. 5) removeafter: Number of rounds that an infected node can infect neigbors (infectious), 
# after which it moves to a Removed state where it is immune and not infectious. 5) susceptibleafter: Number of rounds after which a node in the Removed state becomes susceptible to infection again

simulate_sirs <- function(network.i, simlength=15, p.t=0.2, display_net=TRUE, removeafter=2, susceptibleafter=10000) {
  
  links <-get.edgelist(network.i)
  N<- vcount(network.i)
  time_stats<-list()
  
  # Initialize time stats. 
  # Number of nodes in S, I, or R status in each round of time
  time_stats$infected_t<-rep(1,simlength)
  time_stats$removed_t<-rep(0,simlength)
  #susceptible is total that are not removed or infected
  time_stats$susceptible_t<-rep(N-1,simlength)
  
  # For advanced lab [mid-term exercise], you will also need to keep track of susceptible and removed status of each node   
  infected <- logical(N) # initialize infection status
  #   susceptible <- rep(TRUE, N) # initialize susceptible status
  #   removed<-logical(N)
  patientzero <- sample(N,1) # select 'patient zero'
  
  # Initialize a vector that keeps track of the time of infection for each node. For advanced lab, you will need to use this appropriately
  infected_time<-rep(0, N)
  # For advanced lab, you will also need to keep track of the number of time steps that nodes are in the removed state, so that you can make them susceptible again in due time 
  # removed_time<-rep(0, N)
  
  #patient zero  
  infected[patientzero] <- TRUE
  # For advanced lab, you will need to use this to keep track of which nodes are susceptible
  # susceptible[patientzero] <-FALSE
  
  # Used to count towards a removal; after a certain number of periods, the node will be immune (i.e. removed)
  infected_time[patientzero] <- 1
  
  if (N > 50) {
    V(network.i)$size <- 2
    V(network.i)$label <- ""
  }
  if (display_net) {
    
    fixlayout <- layout.kamada.kawai(network.i)  # store a fixed layout for the graph
    node.colour <- rep("SkyBlue2",N) # initialize node colours (SkyBlue2 is also the default node colour in igraph)
    node.colour[patientzero] <- "red" # infected nodes will be coloured red
    plot(network.i,layout=fixlayout, main="Time = 0", vertex.color=node.colour)
  }
  for (i in 1:simlength) {
    
    # Original spreading mechanism, that did not account for removed nodes
    # Advanced lab: Need to update this to consider removed (immune) or newly susceptible nodes
    discordant.links <- which(xor(infected[as.integer(links[,1])],infected[as.integer(links[,2])])) # find the indices of links that connect an infected individual to an uninfected
    
    transmit <- rbinom(length(discordant.links),1,p.t) # determine randomly which of the discordant links transmit the disease
    
    # Update the infection vector in three steps to make it easier to read:
    transmitter.links <- discordant.links[transmit==1]
    nodes.of.transmitter.links <- unique(as.vector(as.integer(links[transmitter.links,1:2]))) # gets both nodes of the transmitter links into a single vector; unique just filters out repetitions
    infected[nodes.of.transmitter.links] <- TRUE # here I simply set both nodes to TRUE (although the transmitter already had 'TRUE'). In more complex models, you might want to do a further check here and overwrite only the newly infected nodes.
    
    # At this point in this loop, you need to update the number of nodes infected in the current round i, and for advanced lab, number removed and susceptible
    # JUST ADD ONE LINE OF CODE HERE BELOW TO MAKE THIS FUNCTION WORK FOR THE REGULAR LAB
    
    
    # For advanced lab, you also need to update the time that nodes have been in infected and removed states about here in the code.    
    # Increment length of time infected, for those that are infected.

    # Also, keep track of when to make nodes susceptible again after a certain number of periods until the susceptibleafter setting

    if (display_net) {
      node.colour[infected] <- "red"
      # Make the removed points as yellow, susceptible points as skyblue. Uncomment these for advanced lab.
      #       node.colour[removed] <- "yellow"
      #       node.colour[susceptible] <-"SkyBlue2"
      input<-readline() # waits for the user to press <ENTER> before proceeding; you need to switch to the console to do this
      
      # TIP: Hit q to break the simulation between renderings of the network
      if (input=="q") {break}
      plot(network.i,layout=fixlayout, main=paste("Time =", i, " out of ", simlength), vertex.color=node.colour)
    }
    
    
    # Advanced lab [Mid-term exercise]: Around this point, you need to remove nodes that have been infected after time determined by removeafter variable.

    # Advanced lab  [Mid-term exercise]: Around this point, you need to make certain nodes susceptible again if they have been in the removed state more than the time determined by susceptibleafter variable

    #Advanced lab  [Mid-term exercise]: Around here, update the susceptible vector so that any node that is not infected and not removed is susceptible

    #Advanced lab  [Mid-term exercise]: Around here, reset the infection times for nodes that have been infected for more than the time determined by the removeafter variable
    #Advanced lab  [Mid-term exercise]: Around here, also reset the removed times for nodes that have been in the removed state for more than the time determined by susceptibleafter variable

    # Also, will need to make sure that infection status is FALSE (off) for nodes that are in the removed state 
  }  
  # time_stats is a list of three vectors that keeps track of number of infected, removed, and susceptible nodes over each round of time.
  # In the regular lab, you need only worry about the number of infected. 
  return(time_stats)
}



################
# Main Program #
################


# Primary school network 
infile_edges<-"Edges_sp_data_school_day_2.csv"
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
g_primschool_orig<-graph.data.frame(edge_frame, directed = FALSE)

## Median and mean of weights
med<-median(E(g_primschool_orig)$weight)
mean_p<-mean(E(g_primschool_orig)$weight)
g_primschool<-delete.edges(g_primschool_orig, which(E(g_primschool_orig)$weight < med))
# Make sure to convert names to integers for this graph before running the simulation
V(g_primschool)$name<-as.integer(1:vcount(g_primschool))

g_primschool_overmed<-delete.edges(g_primschool_orig, which(E(g_primschool_orig)$weight < med))
mean(E(g_primschool_orig)$weight)
vcount(g_primschool)

# For Quiz 6, running simulate_sirs (solution version)
#infected_school_overmed<-simulate_sirs(g_primschool_overmed, simlength= 100,  removeafter=100000, p.t=0.05, display_net=FALSE)

g_primschool_overmean<-delete.edges(g_primschool_orig, which(E(g_primschool_orig)$weight < mean_p))

# Make sure to convert names to integers for this graph before running the simulation
V(g_primschool_overmed)$name<-as.integer(1:vcount(g_primschool_overmed))
V(g_primschool_overmean)$name<-as.integer(1:vcount(g_primschool_overmean))

### Sept 21 2020 simulation
infected_school_overmed<-simulate_sirs(g_primschool_overmed, simlength= 100,  removeafter=100000,susceptibleafter = 2, p.t=0.05, display_net=FALSE)
infected_school_overmean<-simulate_sirs(g_primschool_overmean, removeafter=10000, simlength= 100,  p.t=0.05, susceptibleafter = 2, display_net=FALSE)

#Quiz 6 question 1
par(mar=c(2,5,2,2), mfrow=c(2,1))
plot(100*infected_school_overmed$infected_t/vcount(g_primschool_overmed), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))
plot(100*infected_school_overmean$infected_t/vcount(g_primschool_overmean), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))

#Quiz 6 question 2
infected_school_p005<-simulate_sirs(g_primschool_overmed, simlength= 200,  removeafter=100000, p.t=0.005, display_net=FALSE)
infected_school_p001<-simulate_sirs(g_primschool_overmed, simlength= 200,  removeafter=100000, p.t=0.001, display_net=FALSE)
par(mar=c(2,5,2,2), mfrow=c(2,1))
plot(100*infected_school_p005$infected_t/vcount(g_primschool_overmed), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))
plot(100*infected_school_p001$infected_t/vcount(g_primschool_overmed), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))

# Three kinds of networks. Please explore by changing the parameters-- numbers of nodes, edges, and probabilities of 
gl <- list()
gl$ws <- watts.strogatz.game(1, 50, 2, 0.1)
gl$er <- erdos.renyi.game(75, 75, type=c("gnm"))
# gl$ba <- barabasi.game(60, m=5, directed=FALSE)

gl$ba <- barabasi.game(45, power=0.98, m=1,  directed=FALSE)
# Run the simulation to view the spread over each round
par(mfrow=c(1,1))
##Simulation in class sept. 2020
infected_time_1<-simulate_sirs(gl$ba , removeafter=10000, simlength= 20, p.t=0.3, display_net=FALSE)
## Hit q to quit, hit Enter to iterate to next step
simulate_sirs(gl$ba, p.t = 0.3, removeafter=4, simlength= 40,  susceptibleafter = 3)

#Quiz 6: Question 3
gl$ba_1 <- barabasi.game(90, power=0.98, m=1,  directed=FALSE)
gl$ba_2 <- barabasi.game(90, power=0.98, m=2,  directed=FALSE)

infected_time_ba_m1<-simulate_sirs(gl$ba_1, removeafter=10000, simlength= 100, p.t=0.05, display_net=FALSE)
infected_time_ba_m2<-simulate_sirs(gl$ba_2, removeafter=10000, simlength= 100, p.t=0.05, display_net=FALSE)

par(mar=c(2,5,2,2), mfrow=c(2,1))
plot(100*infected_time_ba_m1$infected_t/vcount(gl$ba_1), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))
plot(100*infected_time_ba_m2$infected_t/vcount(gl$ba_2), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))

#Quiz 6: Question 4
gl$ws_01 <- watts.strogatz.game(1, 50, 2, 0.01)
gl$ws_02 <- watts.strogatz.game(1, 50, 2, 0.1)
gl$ws_03 <- watts.strogatz.game(1, 50, 2, 0.2)

infected_time_ws_p1<-simulate_sirs(gl$ws_01, removeafter=1000, simlength= 200, p.t=0.05, display_net=FALSE)
infected_time_ws_p2<-simulate_sirs(gl$ws_02, removeafter=1000, simlength= 200, p.t=0.05, display_net=FALSE)
infected_time_ws_p3<-simulate_sirs(gl$ws_03, removeafter=1000, simlength= 200, p.t=0.05, display_net=FALSE)

par(mar=c(2,5,2,2), mfrow=c(3,1))
plot(100*infected_time_ws_p1$infected_t/vcount(gl$ws_01), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))
plot(100*infected_time_ws_p2$infected_t/vcount(gl$ws_02), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))
plot(100*infected_time_ws_p3$infected_t/vcount(gl$ws_03), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))

#Quiz 6: Question 5
# April 23, 2018: Takes each additional edge as adding one to the average degree; total edges is equal to (total nodes X average degree) / 2
# However, the answer does not change in the quiz!
num_nodes<-75
er_connections<-log(num_nodes)*num_nodes
er_edges<- er_connections/2
gl$er_a <- erdos.renyi.game(num_nodes, er_edges, type=c("gnm"))
gl$er_b <- erdos.renyi.game(num_nodes, er_edges/2, type=c("gnm"))
## Verify that average degree approximately equals log of number of nodes (4.3)
mean(degree(gl$er_a))
log(num_nodes)

infected_time_er_a<-simulate_sirs(gl$er_a, removeafter=1000, simlength= 100, p.t=0.05, display_net=FALSE)
infected_time_er_b<-simulate_sirs(gl$er_b, removeafter=1000, simlength= 100, p.t=0.05, display_net=FALSE)
# Run three plots together for the advanced lab. Regular lab 6 requires only the infected numbers (top plot).
par(mar=c(2,5,2,2), mfrow=c(2,1))
plot(100*infected_time_er_a$infected_t/vcount(gl$er_a), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))
plot(100*infected_time_er_b$infected_t/vcount(gl$er_b), type="l", col="red", ylab="Pct. Infected over time", xlab = "time index", ylim=c(0, 100))

is.connected(gl$er_a, mode="weak")
is.connected(gl$er_b, mode="weak")
plot(gl$er_a, vertex.size = 4, vertex.label= NA)
plot(gl$er_b, vertex.size = 4, vertex.label= NA)

#Quiz 6: Question 6
# On the coding for this question, you are ON YOUR OWN. (Plenty of clues provided above.) Thanks!



