################################################################################
#                             CASE 3: 60 ROUTERS
################################################################################
# Call the file that contains all the functions that solve topologies:
source("topology_solver.R")

# Define some general variables:
num_states <- 60
num_paths = 2
alpha = 0.5
gamma = 0.9
epsilon = 0.1
num_episodes = 5000

# Define the adjacency matrix. 1 means nodes are connected, 0 means they aren't.
adj_matrix <- matrix(0, nrow=num_states, ncol=num_states)
adj_matrix[1,2] = 1; adj_matrix[2,1] = 1;
adj_matrix[2,3] = 1; adj_matrix[3,2] = 1;
adj_matrix[3,4] = 1; adj_matrix[4,3] = 1;
adj_matrix[4,5] = 1; adj_matrix[5,4] = 1; 
adj_matrix[5,6] = 1; adj_matrix[6,5] = 1;
adj_matrix[6,7] = 1; adj_matrix[7,6] = 1;
adj_matrix[7,8] = 1; adj_matrix[8,7] = 1;
adj_matrix[8,9] = 1; adj_matrix[9,8] = 1;
adj_matrix[9,10] = 1; adj_matrix[10,9] = 1;
adj_matrix[10,11] = 1; adj_matrix[11,10] = 1
adj_matrix[4,10] = 1; adj_matrix[10,4] = 1
adj_matrix[11,12] = 1; adj_matrix[12,11] = 1; 
adj_matrix[12,13] = 1; adj_matrix[13,12] = 1;
adj_matrix[13,14] = 1; adj_matrix[14,13] = 1;
adj_matrix[14,15] = 1; adj_matrix[15,14] = 1;
adj_matrix[15,16] = 1; adj_matrix[16,15] = 1;
adj_matrix[16,4] = 1; adj_matrix[4,16] = 1
adj_matrix[15,17] = 1; adj_matrix[17,15] = 1
adj_matrix[17,18] = 1; adj_matrix[18,17] = 1
adj_matrix[18,1] = 1; adj_matrix[1,18] = 1
adj_matrix[10,19] = 1; adj_matrix[19,10] = 1
adj_matrix[16,19] = 1; adj_matrix[19,16] = 1
adj_matrix[17,20] = 1; adj_matrix[20,17] = 1
adj_matrix[20,21] = 1; adj_matrix[21,20] = 1
adj_matrix[21,22] = 1; adj_matrix[22,21] = 1
adj_matrix[15,22] = 1; adj_matrix[22,15] = 1
adj_matrix[22,23] = 1; adj_matrix[23,22] = 1
adj_matrix[23,24] = 1; adj_matrix[24,23] = 1
adj_matrix[13,24] = 1; adj_matrix[24,13] = 1
adj_matrix[24,25] = 1; adj_matrix[25,24] = 1
adj_matrix[25,26] = 1; adj_matrix[26,25] = 1
adj_matrix[26,27] = 1; adj_matrix[27,26] = 1
adj_matrix[27,28] = 1; adj_matrix[28,27] = 1
adj_matrix[28,29] = 1; adj_matrix[29,28] = 1
adj_matrix[29,30] = 1; adj_matrix[30,29] = 1
adj_matrix[30,31] = 1; adj_matrix[31,30] = 1
adj_matrix[31,32] = 1; adj_matrix[32,31] = 1
adj_matrix[28,32] = 1; adj_matrix[32,28] = 1
adj_matrix[32,33] = 1; adj_matrix[33,32] = 1
adj_matrix[33,34] = 1; adj_matrix[34,33] = 1
adj_matrix[34,35] = 1; adj_matrix[35,34] = 1
adj_matrix[35,36] = 1; adj_matrix[36,35] = 1
adj_matrix[24,36] = 1; adj_matrix[36,24] = 1
adj_matrix[36,37] = 1; adj_matrix[37,36] = 1
adj_matrix[37,38] = 1; adj_matrix[38,37] = 1
adj_matrix[21,38] = 1; adj_matrix[38,21] = 1
adj_matrix[38,39] = 1; adj_matrix[39,38] = 1
adj_matrix[39,40] = 1; adj_matrix[40,39] = 1
adj_matrix[33,40] = 1; adj_matrix[40,33] = 1
adj_matrix[35,40] = 1; adj_matrix[40,35] = 1
adj_matrix[39,41] = 1; adj_matrix[41,39] = 1
adj_matrix[41,42] = 1; adj_matrix[42,41] = 1
adj_matrix[20,42] = 1; adj_matrix[42,20] = 1
adj_matrix[42,43] = 1; adj_matrix[43,42] = 1
adj_matrix[43,44] = 1; adj_matrix[44,43] = 1
adj_matrix[44,45] = 1; adj_matrix[45,44] = 1
adj_matrix[39,45] = 1; adj_matrix[45,39] = 1
adj_matrix[45,46] = 1; adj_matrix[46,45] = 1
adj_matrix[44,45] = 1; adj_matrix[45,44] = 1
adj_matrix[33,46] = 1; adj_matrix[46,33] = 1
adj_matrix[33,47] = 1; adj_matrix[47,33] = 1
adj_matrix[47,48] = 1; adj_matrix[48,47] = 1
adj_matrix[46,48] = 1; adj_matrix[48,46] = 1
adj_matrix[48,49] = 1; adj_matrix[49,48] = 1
adj_matrix[48,49] = 1; adj_matrix[49,48] = 1
adj_matrix[49,50] = 1; adj_matrix[50,49] = 1
adj_matrix[50,51] = 1; adj_matrix[51,50] = 1
adj_matrix[51,52] = 1; adj_matrix[52,51] = 1
adj_matrix[52,53] = 1; adj_matrix[53,52] = 1
adj_matrix[53,54] = 1; adj_matrix[54,53] = 1
adj_matrix[53,58] = 1; adj_matrix[58,53] = 1
adj_matrix[54,55] = 1; adj_matrix[55,54] = 1
adj_matrix[55,56] = 1; adj_matrix[56,55] = 1
adj_matrix[56,57] = 1; adj_matrix[57,56] = 1
adj_matrix[44,57] = 1; adj_matrix[57,44] = 1
adj_matrix[59,58] = 1; adj_matrix[58,59] = 1
adj_matrix[57,59] = 1; adj_matrix[59,57] = 1
adj_matrix[57,59] = 1; adj_matrix[59,57] = 1
adj_matrix[59,60] = 1; adj_matrix[60,59] = 1
adj_matrix[46,60] = 1; adj_matrix[60,46] = 1


# STEP 1: Generate the paths.
generate_random_values(num_states, num_paths, adj_matrix)

# STEP 2: Select the best paths based on lowest costs.
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)

# STEP 3: Plot the topology.
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)

# STEP 4: Use Q-learning to explore the environment.
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# STEP 5: Obtain the path from every node to every other node.
create_graph_from_adj_matrix(adj_matrix, Q_table)

for (i in 1:num_states){
  for (j in 1:num_states){
    if (i != j){
      get_best_path_after_learning(graph, start_node = i, end_node = j)
      cat("\n")
    }
  }
}



################################################################################
#   DEGRADING LINKS 10-19, 16-19, 17-20, 20-21, 21-22, 15-22, 22-23, 23-24
################################################################################
# Visualize the best path from 1-60 before degradation:
get_best_path_after_learning(graph, start_node = 1, end_node = 60)
visualize_best_path(1, 60, graph)

# Degrade paths 10-19, 16-19, 17-20, 20-21, 21-22, 15-22, 22-23, 23-24:
ber_values[10,19,1] = 1e-04; ber_values[19,10,1] = 1e-04; ber_values[10,19,2] = 1e-04; ber_values[19,10,2] = 1e-04
ber_values[16,19,1] = 1e-04; ber_values[19,16,1] = 1e-04; ber_values[16,19,2] = 1e-04; ber_values[19,16,2] = 1e-04
ber_values[17,20,1] = 1e-04; ber_values[20,17,1] = 1e-04; ber_values[17,20,2] = 1e-04; ber_values[20,17,2] = 1e-04
ber_values[20,21,1] = 1e-04; ber_values[21,20,1] = 1e-04; ber_values[20,21,2] = 1e-04; ber_values[21,20,2] = 1e-04
ber_values[15,22,1] = 1e-04; ber_values[22,15,1] = 1e-04; ber_values[15,22,2] = 1e-04; ber_values[22,15,2] = 1e-04
ber_values[22,23,1] = 1e-04; ber_values[23,22,1] = 1e-04; ber_values[22,23,2] = 1e-04; ber_values[23,22,2] = 1e-04
ber_values[23,24,1] = 1e-04; ber_values[24,23,1] = 1e-04; ber_values[23,24,2] = 1e-04; ber_values[24,23,2] = 1e-04

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

# Visualize the best path from 1-60 after degradation:
get_best_path_after_learning(graph, start_node = 1, end_node = 60)
visualize_best_path(1, 60, graph)

