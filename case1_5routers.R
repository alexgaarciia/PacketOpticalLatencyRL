source("5routers_topology.R")

################################################################################
#                            SCENARIO 1: RANDOM VALUES
################################################################################
num_states <- 5
num_paths = 2
alpha = 0.5
gamma = 0.9
epsilon = 0.1
num_episodes = 1000

# Define the adjacency matrix. 1 means nodes are connected, 0 means they aren't.
adj_matrix <- matrix(c(
  0, 1, 1, 0, 0, 
  1, 0, 1, 1, 0, 
  1, 1, 0, 0, 1, 
  0, 1, 0, 0, 1, 
  0, 0, 1, 1, 0  
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
# Define some general variables:
num_states <- 5
num_paths = 2
alpha = 0.5
gamma = 0.9
epsilon = 0.1
num_episodes = 1000

# Define the adjacency matrix. 1 means nodes are connected, 0 means they aren't.
adj_matrix <- matrix(c(
  0, 1, 1, 0, 0, 
  1, 0, 1, 1, 0, 
  1, 1, 0, 0, 1, 
  0, 1, 0, 0, 1, 
  0, 0, 1, 1, 0  
), nrow=num_states, byrow=TRUE)
create_states(num_states)

# Define the matrices that will be used for the project:
distance_values <- array(NA, dim = c(num_states, num_states, num_paths))
load_values <- array(NA, dim = c(num_states, num_states, num_paths))
ber_values <- array(NA, dim = c(num_states, num_states, num_paths))
chosen_distance <- matrix(NA, nrow = num_states, ncol = num_states)
chosen_ber <- matrix(NA, nrow = num_states, ncol = num_states)
chosen_load <- matrix(NA, nrow = num_states, ncol = num_states)
cost_matrix <- matrix(-Inf, nrow = num_states, ncol = num_states)
Q_table <- matrix(0, nrow = num_states, ncol = num_states)

# Define the distance values between routers:
distance_values[1,2,1] = 6; distance_values[2,1,1] <- 6
distance_values[1,3,1] = 1; distance_values[3,1,1] <- 1
distance_values[2,3,1] = 7; distance_values[3,2,1] <- 7
distance_values[2,4,1] = 5; distance_values[4,2,1] <- 5
distance_values[3,5,1] = 2; distance_values[5,3,1] <- 2
distance_values[4,5,1] = 10; distance_values[5,4,1] <- 10

# Define the load values between routers:
load_values[1,2,1] = 0.7; load_values[2,1,1] = 0.7
load_values[1,3,1] = 0.5; load_values[3,1,1] = 0.5
load_values[2,3,1] = 0.6; load_values[3,2,1] = 0.6
load_values[2,4,1] = 0.1; load_values[4,2,1] = 0.1
load_values[3,5,1] = 0.2; load_values[5,3,1] = 0.2
load_values[4,5,1] = 0.9; load_values[5,4,1] = 0.9

# Define the BeR values between routers:
ber_values[1,2,1] = 1; ber_values[2,1,1] = 1
ber_values[1,3,1] = 10^-4; ber_values[3,1,1] = 10^-4
ber_values[2,3,1] = 10^-4; ber_values[3,2,1] = 10^-4
ber_values[2,4,1] = 10^-6; ber_values[4,2,1] = 10^-6
ber_values[3,5,1] = 0; ber_values[5,3,1] = 0
ber_values[4,5,1] = 10^-6; ber_values[5,4,1] = 10^-6

# Solve the scenario:
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)

for (i in 1:num_states){
  for (j in 1:num_states){
    if (i != j){
      get_best_path_after_learning(Q_table, start_node = i, end_node = j, adj_matrix)
    }
  }
}

