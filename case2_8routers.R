################################################################################
#                            SCENARIO 1: RANDOM VALUES
################################################################################
# Call the file that contains all the functions that solve topologies:
source("topology_solver.R")

num_states <- 8
num_paths = 2
alpha = 0.5
gamma = 0.9
epsilon = 0.1
num_episodes = 1000

# Define the adjacency matrix. 1 means nodes are connected, 0 means they aren't.
adj_matrix <- matrix(c(
  #A #B #C #D #E #F #G #H
  0, 1, 0, 1, 0, 0, 0, 0, #A
  1, 0, 1, 0, 1, 0, 0, 0, #B
  0, 1, 0, 1, 0, 0, 0, 0, #C
  1, 0, 1, 0, 0, 0, 0, 1, #D
  0, 1, 0, 0, 0, 1, 0, 0, #E
  0, 0, 0, 0, 1, 0, 1, 0, #F
  0, 0, 0, 0, 0, 1, 0, 1, #G
  0, 0, 0, 1, 0, 0, 1, 0  #H
), nrow=num_states, byrow=TRUE)

create_states(num_states)
generate_random_values(num_states, num_paths, adj_matrix)
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

# Obtain the path to from every node to every other node:
for (i in 1:num_states){
  for (j in 1:num_states){
    if (i != j){
      get_best_path_after_learning(Q_table, start_node = i, end_node = j, adj_matrix)
      cat("\n")
    }
  }
}


################################################################################
#                            SCENARIO 2: GIVEN VALUES
################################################################################
# Call the file that contains all the functions that solve topologies:
source("topology_solver.R")
