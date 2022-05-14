nodes = read.csv("nodes1.csv")
edges = read.csv("final_data.csv")

library(igraph)
G = graph_from_data_frame(edges,directed = F,vertices = nodes)
E(G)$weight <- edges$Weight
is_weighted(G)

# Degree Distribution
degree.distribution = degree.distribution(G, mode = "all", cumulative = FALSE)


# A function to plot the degree distribution
plot_degree_distribution = function(graph) {
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       col = 1, main = "Degree Distribution")
}


plot_degree_distribution(G)


# Clustering
# Count the number of degree for each node:
deg=degree(G, mode="all")

#layouts
a <- layout_randomly(G)
b <- layout_in_circle(G)
c <- layout_with_fr(G)
d <- layout_with_lgl(G)

# Plot
plot.igraph(G,layout = b,vertex.size=deg*1.3,edge.width=E(G)$weight)
plot.igraph(G,vertex.size=deg*1.3,layout = b)


fast_greedy = cluster_fast_greedy(graph = G, modularity = T, weights = E(G)$weight)
plot(fast_greedy,G)
membership(fast_greedy)
sizes(fast_greedy)
modularity(fast_greedy)

leading_eigen = cluster_leading_eigen(graph = G,weights = E(G)$weight )
plot(leading_eigen ,G)
membership(leading_eigen)
sizes(leading_eigen)


louvain = cluster_louvain(graph = G,weights = E(G)$weight )
plot(louvain,G)
membership(louvain)
sizes(louvain)


walktrap = cluster_walktrap(graph = G,weights = E(G)$weight, modularity = T,steps = 5)
plot(walktrap,G)
membership(walktrap)
sizes(walktrap)
modularity(walktrap)
