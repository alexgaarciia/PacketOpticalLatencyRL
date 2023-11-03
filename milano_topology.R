################################################################################
#                                MILANO TOPOLOGY
################################################################################
# Call the file that contains all the functions that solve topologies:
source("topology_solver.R")

# Define some general variables:
num_states <- 52
num_paths <- 1
alpha <- 0.5
gamma <- 0.9
epsilon <- 0.1
num_episodes <- 2000

# The csv shows connections between routers but with an integer between 0 and 1.
# Since the entire algorithm works with 0 and 1, whenever there is a number that 
# is not 0, it is assumed to be a connection and hence, it is changed to 1.
adj_matrix <- as.matrix(read.csv("crossMatrix_Milano.csv", sep=";", header = FALSE))
for (i in 1:length(adj_matrix)){
  if (adj_matrix[i]!= 1 & adj_matrix[i]!=0){
    adj_matrix[i] = 1
  }
}

# STEP 1: Generate the paths.
generate_random_values(num_states, num_paths, adj_matrix)

# STEP 2: Select the best paths based on lowest costs.
# Beware, in our case, "crossMatrix_Milano" already contains the distances that
# must be considered, so we replace "distance_values" with these new ones.
distance_values <- array(as.matrix(read.csv("crossMatrix_Milano.csv", sep=";", header = FALSE)), dim = c(52, 52, 1))
distance_values[distance_values == 0] <- NA

# Select the best paths:
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)


# STEP 3: Plot the topology.
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# STEP 4: Use Q-learning to explore the environment.
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# STEP 5: Obtain the path from every node to every other node.
# Create the graph using the adjacency matrix and considering the values of the
# Q-table as weights
create_graph_from_adj_matrix(adj_matrix, Q_table)

# Change the name of the nodes
nodes_mat <- read.csv("nodesLabeling_Milano.csv", header = FALSE, sep = ";") 
names(nodes_mat)[1]<-paste("nodes")
V(graph)$name = nodes_mat$nodes

for (i in 1:num_states){
  for (j in 1:num_states){
    if (i != j){
      get_best_path_after_learning(graph, start_node = i, end_node = j)
      cat("\n")
    }
  }
}



################################################################################
#                        DEGRADING LINKS 1-6, 6-7, 7-22
################################################################################
# Visualize the best path from 22-5 before degradation:
get_best_path_after_learning(graph, start_node = 22, end_node = 5)
visualize_best_path(22,5, graph)

# Degrade paths 1-6, 6-7, 7-22:
ber_values[1,6,1] = 1e-04; ber_values[6,1,1] = 1e-04
ber_values[6,7,1] = 1e-04; ber_values[7,6,1] = 1e-04
ber_values[22,7,1] = 1e-04; ber_values[7,22,1] = 1e-04

# Select the best paths based on lowest costs.
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)

# Plot the topology.
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# Use Q-learning to explore the environment.
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# Obtain the path from every node to every other node.
create_graph_from_adj_matrix(adj_matrix, Q_table)

for (i in 1:num_states){
  for (j in 1:num_states){
    if (i != j){
      get_best_path_after_learning(graph, start_node = i, end_node = j)
      cat("\n")
    }
  }
}

# Visualize the best path from 22-5 after degradation:
get_best_path_after_learning(graph, start_node = 22, end_node = 5)
visualize_best_path(22,5, graph)

