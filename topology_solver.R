rm(list=ls())
library(igraph)


################################################################################
#                           GENERATION OF ROUTERS
################################################################################
create_states <- function(num_states){
  "This is a function designed to generate a random topology"
  
  # Create the states:
  states <- paste0("s", 1:num_states)
  
  # Return generated states:
  assign("states", states, envir = .GlobalEnv)
}


################################################################################
#        GENERATION OF RANDOM VALUES OF DISTANCE, LOAD AND Bit Error Rate
################################################################################
generate_random_values <- function(num_states, num_paths, adj_matrix){
  "This is a function used to generate random values of distance, load and BeR"
  
  # Create the 3D arrays for each of the variables (the dimensions are 
  # num_states x num_states x num_paths).
  distance_values <- array(NA, dim = c(num_states, num_states, num_paths))
  load_values <- array(NA, dim = c(num_states, num_states, num_paths))
  ber_values <- array(NA, dim = c(num_states, num_states, num_paths))
  
  # Fill the arrays:
  for (i in 1:(num_states-1)) {  
    for (j in (i+1):num_states) { 
      
      # If condition to check whether there is a connection between router i and j.
      if (adj_matrix[i,j] == 1) {  
        
        # This loop iterates through each possible path between the nodes i and j.
        for (k in 1:num_paths){
          
          # For each path, it generates random values for distance, load and BeR.
          km <- runif(1, min = 1, max = 20)
          load <- runif(1, min = 0, max = 1)
          BeR <- runif(1, min = 0, max = 1)
          
          # Assign values to the arrays for both connections due to symmetry:
          distance_values[i, j, k] <- km; distance_values[j, i, k] <- km
          load_values[i, j, k] <- load; load_values[j, i, k] <- load
          ber_values[i, j, k] <- BeR; ber_values[j, i, k] <- BeR
        }
      }
    }
  }
  
  # Return generated values:
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
  
  propagation_delay = 5 * distance_km  # 5us for each km in the fiber
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

select_best_paths <- function(num_states, num_paths, adj_matrix, distance_values, load_values, ber_values){
  "The main goal of this function is to go through all the possible paths from
  one router to another and select the one with the lowest cost"
  
  # Construct 4 matrices:
  # 1. cost_matrix: it will store the cost of the shortest paths.
  cost_matrix <- matrix(-Inf, nrow = num_states, ncol = num_states)
  
  # 2. chosen_distance: it will store the distance between routers that gave the
  # lowest cost.
  chosen_distance <- matrix(NA, nrow = num_states, ncol = num_states)
  
  # 3. chosen_load: it will store the load between routers that gave the
  # lowest cost.
  chosen_load <- matrix(NA, nrow = num_states, ncol = num_states)
  
  # 4. chosen_ber: it will store the BeR between routers that gave the
  # lowest cost.
  chosen_ber <- matrix(NA, nrow = num_states, ncol = num_states)
 
  # Traverse through every possible combination of nodes:
  for (i in 1:num_states) {
    for (j in 1:num_states) {
      
      # Check if routers i and j are connected:
      if (adj_matrix[i,j] == 1) {
        min_cost <- Inf
        chosen_path_k <- NA  # Track the path 'k' that gave the minimum cost
        
        # Iterate through each possible path:
        for (k in 1:num_paths) {
          
          #For each path k, this code retrieves its distance, load, and Bit
          # error rate (BeR) values from the 3D arrays:
          km_value <- distance_values[i, j, k]
          load_value <- load_values[i, j, k]
          ber_value <- ber_values[i, j, k]
          
          # Check if any of the values is NA:
          if (!anyNA(c(km_value, load_value, ber_value))){
            
            # Compute the cost of the given path:
            cost <- calculate_total_cost(km_value, load_value, ber_value)
            
            # If the computed cost of the path k is less than the current min_cost,
            # then min_cost is updated to this new value and the index k of this
            # path is stored in chosen_path_k:
            if (cost < min_cost) {
              min_cost <- cost
              chosen_path_k <- k 
            }
          }
        }
        
        # Assign the cost to the path and store the respective km, load, and BeR values:
        cost_matrix[i, j] <- -min_cost
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
  "This is a function used to plot the topology of the current environment"
  
  # Create the graph:
  g <- graph_from_adjacency_matrix(adjmatrix=adj_matrix, mode="undirected", weighted=NULL, diag=FALSE)
  
  # Create the edge labels to show in the graph the distance, load and BeR values
  # of each connection between each router:
  edge_labels = c()
  for (i in 1:(num_states-1)){
    for (j in (i+1):num_states){
      if (adj_matrix[i,j] == 1){
        label <- paste(
          "Distance:", round(chosen_distance[i,j], 4), 
          "\nLoad:", round(chosen_load[i,j], 4), 
          "\nBeR:", round(chosen_ber[i,j], 4),
          "\n"
        )
        edge_labels = c(edge_labels, label)
      }
    }
  }
  
  # Plot the graph:
  plot(g, 
       layout = layout_with_fr,
       vertex.color = "lightblue", 
       vertex.size = 20, 
       vertex.label.color = "black",
       vertex.label.cex = 1,
       vertex.label.dist = 0.5,
       edge.label.cex = 0.8,
       edge.label = edge_labels,
       main = "Router Topology") 
}


################################################################################
#                 SOLVE THE PROPOSED SCENARIO USING Q-LEARNING
################################################################################
solve_scenario_qlearning <- function(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix){
  "This is a function that has the goal of solving a specific scenario using Q-learning"
  
  # Initialize a Q-table with all zeros. Then, any entry in the Q-table that
  # corresponds to a pair of nodes that aren't directly connected is set to
  # negative infinity to ensure those actions are not chosen.
  Q_table <- matrix(0, nrow = num_states, ncol = num_states)
  Q_table[adj_matrix == 0] <- -Inf

  # Start a loop over a specified number of episodes. In each episode, the agent
  # will navigate through the network to learn the optimal path:
  for (episode in 1:num_episodes) {
    
    # Randomly choose a starting state:
    state <- sample(1:num_states, 1)  
    
    # This list will keep track of all nodes the agent visits in this episode to
    # avoid loops.
    visited_nodes <- c(state)
    
    while (TRUE) {  
      possible_actions <- which(adj_matrix[state, ] == 1)
      
      # Epsilon-greedy Strategy: with probability epsilon, the agent chooses a
      # a random action from the available ones (exploration), and with probability
      # 1-epsilon, the agent chooses the action with the highest current Q-value
      # (exploitation).
      
      # A random number between 0 and 1 is generated: is it less than epsilon?
      # Then explore; otherwise? Exploit.
      if (runif(1) < epsilon) {
        # Exploration:
        valid_actions <- setdiff(possible_actions, visited_nodes)
        if (length(valid_actions) == 0) {
          break
        }
        action <- sample(valid_actions, 1)
        
        # Exploitation:
      } else {
        valid_actions <- setdiff(possible_actions, visited_nodes)
        if (length(valid_actions) == 0) {
          break
        }
        action_values <- Q_table[state, valid_actions]
        action_index <- which.max(action_values)
        action <- valid_actions[action_index]
      }
      
      # Update visited nodes:
      visited_nodes <- c(visited_nodes, action)
      
      # Finally: 
      # 1. Get a reward for the chosen action from the cost_matrix:
      reward <- cost_matrix[state, action] # agent tries to maximize its reward by minimizing the cost
      
      # 2. Calculate the expected reward by considering the maximum Q-value from
      # the next state (action), discounted by gamma.
      future_reward <- gamma * max(Q_table[action, ])
      
      # 3. Use the Q-learning formula to update the Q-value in the Q-table for
      # the current state and action.
      Q_table[state, action] <- (1-alpha)*Q_table[state, action] + alpha*(reward + future_reward)
      
      # The agent moves to the next state, which corresponds to the action it chose:
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
  "Gives the best route from a starting node to a destination node based on the highest cumulative reward (lowest negative reward)"
  
  # Obtain the number of states using the Q-table:
  num_states <- nrow(Q_table)
  
  # Initialize distance, previous node and visited variables:
  # 1. distance: It represents the shortest known distance (or, in this case,
  #   the highest cumulative reward) from the start_node to every other node.
  distance <- rep(Inf, num_states)
  
  # 2. previous: This vector keeps track of the previous node on the best path
  #   from the start_node to every other node. It's used for backtracking to find
  #   the best path once Dijkstra's algorithm completes.
  previous <- rep(0, num_states)
  
  # 3. visited: This vector is a boolean flag for each state/node indicating
  #   if it has been visited (considered) in Dijkstra's algorithm. It ensures we
  #   don't reconsider nodes we've already processed.
  visited <- rep(FALSE, num_states)
  
  # Start node distance is 0:
  distance[start_node] <- 0
  
  # Find the shortest path using Dijkstra's algorithm with negative rewards:
  for (i in 1:num_states) {
    # Find the node with the minimum distance among unvisited nodes:
    min_distance <- Inf
    min_index <- -1
    
    for (v in 1:num_states) {
      # The inner loop iterates over all nodes to find the node with the minimum 
      # distance (highest reward in our case) that hasn't been visited. This node
      # is represented by min_index.
      if (!visited[v] && distance[v] < min_distance) {
        min_distance <- distance[v]
        min_index <- v
      }
    }
    
    if (min_index == -1) {
      # If min_index remains as -1, it indicates that there are no more reachable
      # nodes left to consider. Thus, we can break out of the outer loop.
      break
    }
    
    # Mark node as visited:
    visited[min_index] <- TRUE
    
    for (v in 1:num_states) {
      # This second inner loop checks each neighboring node v of the current
      # min_index node.
      
      if (!visited[v] && adj_matrix[min_index, v] == 1) {
        # This condition ensures that the neighbor v has not been visited and
        # is directly connected to the current node.
        
        alt <- distance[min_index] + (-Q_table[min_index, v])
        # For each valid neighbor, the algorithm calculates an alternate distance (alt),
        # which is the sum of the current node's distance and the negative Q-value
        # (negative to transform reward into cost).
        
        if (alt < distance[v]) {
          # If this alternate distance is less than the currently known distance
          # to the neighbor, then:
          #   1. The distance for the neighboring node v is updated with this new value.
          #   2. The previous vector for the neighbor v is set to the current node (min_index),
          #     indicating the current node is the best predecessor for the neighbor
          #     on the shortest path from the start node.
          distance[v] <- alt
          previous[v] <- min_index
        }
      }
    }
  }
  
  # Reconstruct the path from end_node to start_node. The path is initialized 
  # with the end_node because we're going to backtrack from our destination
  # (end_node) to our starting point (start_node). 
  path <- c(end_node)
  current_node <- end_node
  while (current_node != start_node) {
    # The loop continues as long as the current_node isn't the start_node, which
    # means we haven't reached the beginning of our path yet.
    
    # This line looks up the previous array to find the predecessor of the
    # current_node. The previous array contains the best predecessor for each
    # node as determined by the Dijkstra's algorithm:
    current_node <- previous[current_node]
    
    # Add node to path:
    path <- c(current_node, path)
  }
  
  # Return generated values:
  assign("path", path, envir = .GlobalEnv)
  
  # Check generated values:
  cat("Path from ", start_node, " to ", end_node, " (based on highest cumulative reward):\n")
  print(path)
}
