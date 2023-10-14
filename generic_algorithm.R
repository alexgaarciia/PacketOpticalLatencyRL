################################################################################
#                      DEFINITION OF GENERIC ALGORITHM
################################################################################
# Define a framework based on RL to decide which is the best possible route
# based on rewards. It will take into account the quality of transmission, 
# load and other metrics that could be added to the model.


################################################################################
#                       GENERATION OF RANDOM ROUTERS
################################################################################
# Load libraries that will be used for the entire project:
library(igraph)
rm(list=ls())

create_states <- function(){
  "This is a function designed to generate a random topology"
  # Generate a random number of states:
  num_states = sample(6:20, 1)
  
  # Creation of states:
  states <- paste0("s", 1:num_states)
  
  # Return generated values:
  assign("num_states", num_states, envir = .GlobalEnv)
  assign("states", states, envir = .GlobalEnv)
}


################################################################################
#        GENERATION OF RANDOM VALUES OF DISTANCE, LOAD AND Bit Error Rate
################################################################################
generate_random_values <- function(num_states, num_paths){
  "This is a function used to generate random values of distance, load and BeR"
  # Create an empty adjacency matrix:
  adj_matrix <- matrix(0, nrow = num_states, ncol = num_states)
  
  # Fill the adjacency matrix with random values (1 or 0) to represent connections
  # between routers:
  for (i in 1:num_states) {
    for (j in 1:num_states) {
      if (i != j) {
        connection <- sample(c(0, 1), 1)
        adj_matrix[i, j] <- connection
        adj_matrix[j, i] <- connection
      }
    }
  }
  
  # Create the 3D arrays for each of the variables:
  distance_values <- array(NA, dim = c(num_states, num_states, num_paths))
  load_values <- array(NA, dim = c(num_states, num_states, num_paths))
  ber_values <- array(NA, dim = c(num_states, num_states, num_paths))
  
  # Fill the arrays:
  for (i in 1:(num_states-1)) {  # Adjusted to avoid going out of bounds
    for (j in (i+1):num_states) {  
      if (adj_matrix[i,j] == 1) {  
        for (k in 1:num_paths){
          km <- runif(1, min = 1, max = 20)
          load <- runif(1, min = 0, max = 1)
          BeR <- runif(1, min = 0, max = 1)
          
          # Assign values to the arrays for both connections due to symmetry
          distance_values[i, j, k] <- km; distance_values[j, i, k] <- km
          load_values[i, j, k] <- load; load_values[j, i, k] <- load
          ber_values[i, j, k] <- BeR; ber_values[j, i, k] <- BeR
        }
      }
    }
  }
  
  # Return generated values:
  assign("adj_matrix", adj_matrix, envir = .GlobalEnv)
  assign("distance_values", distance_values, envir = .GlobalEnv)
  assign("load_values", load_values, envir = .GlobalEnv)
  assign("ber_values", ber_values, envir = .GlobalEnv)
  
  
  # Check generated values:
  cat("Distance values:\n")
  print(distance_values)
  cat("\n")
  cat("Load values:\n")
  print(load_values)
  cat("\n")
  cat("BeR values:\n")
  print(ber_values)
  
}


################################################################################
#                 SELECTION OF BEST PATHS (BASED ON PENALTIES)
################################################################################
calculate_total_cost <- function(distance_km, load, BeR) {
  "This is a function used to compute the total cost of a certain path based on
  distance, load and BeR values"
  propagation_delay = 5 * distance_km  # 5us for each km in the fiber.
  tranmission_queue_delay = 1/(1-load)  # 1us x (1/(1-load))
  
  # Penalization of BeR:
  if (BeR >= 10^-4 && BeR <= 1){
    ber_penalty = 1000
  } else if (BeR > 10^-5 && BeR < 10^-4){
    ber_penalty = 50
  } else {
    ber_penalty = 0
  }
  
  # Sum of penalties:
  return (propagation_delay + tranmission_queue_delay + ber_penalty)
}

select_best_paths <- function(num_states, num_paths, adj_matrix,
                              distance_values, load_values, ber_values){
  # Construct 3 matrices that will store the values of the shortest paths:
  chosen_distance <- matrix(NA, nrow = num_states, ncol = num_states)
  chosen_ber <- matrix(NA, nrow = num_states, ncol = num_states)
  chosen_load <- matrix(NA, nrow = num_states, ncol = num_states)
  
  # Construct a weight matrix for the graph edges based on the cost
  cost_matrix <- matrix(-Inf, nrow = num_states, ncol = num_states)
  
  for (i in 1:num_states) {
    for (j in 1:num_states) {
      if (adj_matrix[i,j] == 1) {
        min_cost <- Inf
        chosen_path_k <- NA  # Track the path 'k' that gave the minimum cost
        for (k in 1:num_paths) {
          # Get the values for this dimension
          km_value <- distance_values[i, j, k]
          load_value <- load_values[i, j, k]
          ber_value <- ber_values[i, j, k]
          
          # Check if any of the values is NA
          if (!anyNA(c(km_value, load_value, ber_value))) {
            # Calculate the minimum cost
            cost <- calculate_total_cost(km_value, load_value, ber_value)
            
            if (cost < min_cost) {
              min_cost <- cost
              chosen_path_k <- k  # update the chosen path
            }
          }
        }
        
        cost_matrix[i, j] <- -min_cost
        # If we found a chosen path 'k', store the respective km, ber, and load values
        if (!is.na(chosen_path_k)) {
          chosen_distance[i, j] <- distance_values[i, j, chosen_path_k]
          chosen_ber[i, j] <- ber_values[i, j, chosen_path_k]
          chosen_load[i, j] <- load_values[i, j, chosen_path_k]
        }
      }
    }
  }
  
  # Return generated values:
  assign("chosen_distance", chosen_distance, envir = .GlobalEnv)
  assign("chosen_ber", chosen_ber, envir = .GlobalEnv)
  assign("chosen_load", chosen_load, envir = .GlobalEnv)
  assign("cost_matrix", cost_matrix, envir = .GlobalEnv)
  
  
  # Check generated values:
  cat("Cost matrix:\n")
  print(cost_matrix)
  cat("\n")
  cat("Distance of the cheapest paths:\n")
  print(chosen_distance)
  cat("\n")
  cat("Loads of the cheapest paths:\n")
  print(chosen_load)
  cat("\n")
  cat("BeR of the cheapest paths:\n")
  print(chosen_ber)
  cat("\n")
}


################################################################################
#                             REPRESENT THE TOPLOGY
################################################################################
plot_topology <- function(adj_matrix, chosen_distance, chosen_load, chosen_ber){
  "This is a function used to create the topology of the current environment"
  # Create the graph
  g <- graph_from_adjacency_matrix(adjmatrix =adj_matrix, mode="undirected", weighted=NULL, diag=FALSE)
  
  # Create some example edge labels
  edge_labels = c()
  for (i in 1:(num_states-1)){
    for (j in (i+1):num_states){
      if (adj_matrix[i,j] == 1){
        label <- paste(
          "Distance:", round(chosen_distance[i,j], 4), 
          "\nLoad:", round(chosen_load[i,j], 4), 
          "\nBeR:", chosen_ber[i,j],
          "\n"
        )
        edge_labels = c(edge_labels, label)
      }
    }
  }
  
  # Plot the graph with edge labels
  plot(g, 
       layout = layout_with_fr,
       vertex.color = "lightblue", 
       vertex.size = 20, 
       vertex.label.color = "black",
       vertex.label.cex = 1,
       edge.label.cex = 0.8,
       edge.label = edge_labels,
       main = "Router Topology") 
}


################################################################################
#             SOLVE THE PROPOSED SCENARIO USING Q-LEARNING
################################################################################
solve_scenario_qlearning <- function(num_states, adj_matrix, alpha, gamma,
                                     epsilon, num_episodes, cost_matrix){
  "This is a function that has the goal of solving a specific scenario using 
  Q-learning"
  # Define the Q-table and fill it with -Inf in case there are no connections.
  Q_table <- matrix(0, nrow = num_states, ncol = num_states)
  Q_table[adj_matrix == 0] <- -Inf
  
  # Explore the environment and learn:
  for (episode in 1:num_episodes) {
    # Randomly choose a starting state.
    state <- sample(1:num_states, 1)  
    
    # This list will keep track of all nodes the agent visits in this episode to
    # avoid loops.
    visited_nodes <- c(state)
    
    while (TRUE) {  
      possible_actions <- which(adj_matrix[state, ] == 1)
      if (runif(1) < epsilon) {
        # If the agent is exploring (as per the epsilon-greedy strategy), it 
        # chooses a random action from the valid_actions which excludes the nodes
        # it has already visited. If there are no valid actions left, the episode
        # ends (breaks out of the while loop).
        valid_actions <- setdiff(possible_actions, visited_nodes)
        if (length(valid_actions) == 0) {
          break
        }
        action <- sample(valid_actions, 1)
      } else {
        # If the agent is not exploring, it's exploiting. This means it's choosing
        # the action (directly connected node/router) that has the maximum Q-value
        # from the Q-table, considering only the nodes it hasn't visited yet.
        valid_actions <- setdiff(possible_actions, visited_nodes)
        if (length(valid_actions) == 0) {
          break
        }
        action_values <- Q_table[state, valid_actions]
        action_index <- which.max(action_values)
        action <- valid_actions[action_index]
      }
      
      visited_nodes <- c(visited_nodes, action)
      
      reward <- cost_matrix[state, action] # agent tries to maximize its reward by minimizing the cost.
      future_reward <- gamma * max(Q_table[action, ])
      Q_table[state, action] <- (1-alpha)*Q_table[state, action] + alpha*(reward + future_reward)
      
      state <- action  
    }
  }
  
  # Return generated values:
  assign("Q_table", Q_table, envir = .GlobalEnv)
  
  # Check generated values:
  cat("Q-Table values:\n")
  print(Q_table)
}


################################################################################
#                                   BEST PATH
################################################################################
get_best_path_after_learning <- function(Q_table, start_node, end_node, adj_matrix) {
  "Gives the best route from a starting node to a destination node. Beware, since 
  the code does not guarantee to generate connected graphs, it may not return
  a path"
  current_node <- start_node
  path <- c(start_node)
  visited <- c()
  
  while (current_node != end_node) {
    # Sort Q-values for the current node in descending order
    sorted_nodes <- order(Q_table[current_node, ], decreasing = TRUE)
    
    # Find the best unvisited node that is also connected to the current node
    next_node <- NULL
    for (node in sorted_nodes) {
      if (!node %in% visited && adj_matrix[current_node, node] == 1) {
        next_node <- node
        break
      }
    }
    
    # If we can't find any valid unvisited nodes, break
    if (is.null(next_node)) {
      cat("No valid unvisited nodes connected to the current node. Exiting...\n")
      break
    }
    
    visited <- c(visited, current_node)  # Add current node to visited list
    path <- c(path, next_node)
    current_node <- next_node
  }
  
  # Given your printed Q-table and the derived path, the agent believes that
  # this path has the highest cumulative reward. This doesn't mean that it's the
  # shortest path or has the lowest cost, but rather, based on the training
  # episodes and the Q-learning parameters, it's the path that the agent has
  # learned to be most rewarding.
  
  # Return generated values:
  assign("path", path, envir = .GlobalEnv)
  
  # Check generated values:
  cat("Path:\n")
  print(path)
}


################################################################################
#                 INVOKE THE FUNCTIONS AND SOLVE RANDOM SCENARIO
################################################################################
num_paths = 2
alpha = 0.5
gamma = 0.9
epsilon = 0.1
num_episodes = 1000

create_states()
generate_random_values(num_states = num_states, num_paths)
select_best_paths(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values)
plot_topology(adj_matrix, chosen_distance, chosen_load, chosen_ber)
solve_scenario_qlearning(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix)
get_best_path_after_learning(Q_table, start_node = 1, end_node = 2, adj_matrix)

