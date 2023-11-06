################################################################################
#                               CASE 1: 5 ROUTERS
################################################################################
# Call the file that contains all the functions that solve topologies:
source("topology_solver.R")

# Define some general variables:
num_states <- 5
num_paths <- 2
alpha <- 0.5
gamma <- 0.9
epsilon <- 0.1
num_episodes <- 1000

# Define the adjacency matrix. 1 means nodes are connected, 0 means they aren't.
adj_matrix <- matrix(c(
  0, 1, 1, 0, 0, 
  1, 0, 1, 1, 0, 
  1, 1, 0, 0, 1, 
  0, 1, 0, 0, 1, 
  0, 0, 1, 1, 0  
), nrow=num_states, byrow=TRUE)


# STEP 1: Generate the paths.
generate_random_values(num_states, num_paths, adj_matrix)

# STEP 2: Select the best paths based on lowest costs.
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)

# STEP 3: Plot the topology.
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# STEP 4: Use Q-learning to explore the environment.
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# STEP 5: Create the graph using the adjacency matrix and considering the values
# of the Q-table as weights
create_graph_from_adj_matrix(adj_matrix, Q_table)

# Step 6: Visualize the best path from 1-5 before degradation
get_best_path_after_learning(graph, start_node = 1, end_node = 5)
visualize_best_path(1, 5, graph)



################################################################################
#                           DEGRADING LINKS 1-3, 3,5
################################################################################
# Step 1: Degrade paths 1-3 and 1-5:
ber_values[1,3,1] = 1e-04; ber_values[3,1,1] = 1e-04; ber_values[1,3,2] = 1e-04; ber_values[3,1,2] = 1e-04
ber_values[3,5,1] = 1e-04; ber_values[5,3,1] = 1e-04; ber_values[3,5,2] = 1e-04; ber_values[5,3,2] = 1e-04

# Step 2: elect the best paths based on lowest costs.
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)

# Step 3: Plot the topology.
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# Step 4: Use Q-learning to explore the environment.
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# STEP 5: Create the graph using the adjacency matrix and considering the values
# of the Q-table as weights
create_graph_from_adj_matrix(adj_matrix, Q_table)

# Step 6: Visualize the best path from 1-5 after degradation
get_best_path_after_learning(graph, start_node = 1, end_node = 5)
visualize_best_path(1, 5, graph)

