################################################################################
#                                TOKYO TOPOLOGY
################################################################################
# Call the file that contains all the functions that solve topologies:
set.seed(123)
source("topology_solver.R")

# Define some general variables:
num_states <- 23
num_paths <- 1
alpha <- 0.5
gamma <- 0.9
epsilon <- 0.1
num_episodes <- 2000

# The csv shows connections between routers but with an integer between 0 and 1.
# Since the entire algorithm works with 0 and 1, whenever there is a number that 
# is not 0, it is assumed to be a connection and hence, it is changed to 1.
adj_matrix <- as.matrix(read.csv("crossMatrix_Tokyo.csv", sep=";", header = FALSE))
for (i in 1:length(adj_matrix)){
  if (adj_matrix[i]!= 1 & adj_matrix[i]!=0){
    adj_matrix[i] = 1
  }
}

# STEP 1: Generate the paths.
generate_random_values(num_states, num_paths, adj_matrix, BeR_random = 0)

# STEP 2: Select the best paths based on lowest costs.
# Beware, in our case, "crossMatrix_Tokyo" already contains the distances that
# must be considered, so we replace "distance_values" with these new ones.
distance_values <- array(as.matrix(read.csv("crossMatrix_Tokyo.csv", sep=";", header = FALSE)), dim = c(23, 23, 1))
distance_values[distance_values == 0] <- NA

# Select the best paths:
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)


# STEP 3: Plot the topology.
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# STEP 4: Use Q-learning to explore the environment.
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# STEP 5: Create the graph using the adjacency matrix and considering the values
# of the Q-table as weights
create_graph_from_adj_matrix(adj_matrix, Q_table)

# Change the name of the nodes
nodes_mat <- read.csv("nodesLabeling_Tokyo.csv", header = FALSE, sep = ";") 
names(nodes_mat)[1] <- paste("nodes")
V(graph)$name = nodes_mat$nodes


# Step 6: Visualize the best path from 1-22 before degradation
get_best_path_after_learning(graph, start_node = 1, end_node = 22)
visualize_best_path(1, 22, graph)

# Visualize the best path from 4-7 before degradation
get_best_path_after_learning(graph, start_node = 4, end_node = 7)
visualize_best_path(4, 7, graph)

# Visualize the best path from 4-11 before degradation
get_best_path_after_learning(graph, start_node = 4, end_node =11)
visualize_best_path(4, 11, graph)

# Visualize the best path from 1-19 before degradation
get_best_path_after_learning(graph, start_node = 1, end_node = 19)
visualize_best_path(1, 19, graph)



################################################################################
#                        DEGRADING LINKS 1-6, 1-4, 10-11
################################################################################
# Step 1: Degrade paths 1-6, 1-4, 10-11
ber_values[1,6,1] = 1e-03; ber_values[6,1,1] = 1e-03
ber_values[1,4,1] = 1e-03; ber_values[4,1,1] = 1e-03
ber_values[10,11,1] = 1e-03; ber_values[11,10,1] = 1e-03

# Step 2: Select the best paths
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)

# STEP 3: Plot the topology
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# STEP 4: Use Q-learning to explore the environment
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# STEP 5: Obtain the path from every node to every other node.
# Create the graph using the adjacency matrix and considering the values of the
# Q-table as weights
create_graph_from_adj_matrix(adj_matrix, Q_table)

# Change the name of the nodes
nodes_mat <- read.csv("nodesLabeling_Tokyo.csv", header = FALSE, sep = ";") 
names(nodes_mat)[1] <- paste("nodes")
V(graph)$name = nodes_mat$nodes


# Step 6: Visualize the best path from 1-22 after degradation
get_best_path_after_learning(graph, start_node = 1, end_node = 22)
visualize_best_path(1, 22, graph)

# Visualize the best path from 4-7 after degradation
get_best_path_after_learning(graph, start_node = 4, end_node = 7)
visualize_best_path(4, 7, graph)

# Visualize the best path from 4-11 after degradation
get_best_path_after_learning(graph, start_node = 4, end_node =11)
visualize_best_path(4, 11, graph)

# Visualize the best path from 1-19 after degradation
get_best_path_after_learning(graph, start_node = 1, end_node = 19)
visualize_best_path(1, 19, graph)

