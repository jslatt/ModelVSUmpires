library(glue)
library(lubridate)
library(tidyverse)
library(igraph)

setwd("~/Documents/Booth/Classes/Spring 24/Big Data/Final/")

source("combine_pitch_ump.R")
## outputs: 
############# combo (pitch data with umpire names)
############# umps_with_game_ids (ump data for networks)

## Set up umpire network matrix
edge_list <- do.call(rbind, apply(umps_with_game_ids[, -1], 1, function(row) {
  combn(row, 2, simplify = FALSE)
}))

# Convert list of pairs to data frame
edge_list <- do.call(rbind, edge_list)
colnames(edge_list) <- c("from", "to")

# Create an igraph object from the edge list
g <- graph_from_data_frame(edge_list, directed = FALSE)
plot(g, vertex.size=5, vertex.label=NA, edge.arrow.size=.5,
     main="Raw Network Graph of Umpires")

# Get the adjacency matrix as a sparse matrix
adj_matrix <- as_adjacency_matrix(g, sparse = TRUE)

# Print the sparse matrix
print(adj_matrix)
plot(adj_matrix, vertex.size=5, vertex.label=NA, edge.arrow.size=.5,
     main="Adjacency Matrix of Umpire Network")

# Degree centrality
degree_centrality <- degree(g, mode = "all")
print(degree_centrality)

# Betweenness centrality
betweenness_centrality <- betweenness(g, directed = FALSE)
print(betweenness_centrality)

# Closeness centrality
closeness_centrality <- closeness(g, mode = "all")
print(closeness_centrality)

# Network density
network_density <- edge_density(g)
print(network_density)

# Network diameter
network_diameter <- diameter(g)
print(network_diameter)

library(arules)

# Apriori association rule mining
uprules <- apriori(gg, parameter=list(support=.0001, confidence=.1, maxlen=2))
inspect(subset(uprules, subset = lhs %in% "Chris Segal"))

# Convert rules to data frame
df_rules <- as(uprules, "data.frame")
print(df_rules)