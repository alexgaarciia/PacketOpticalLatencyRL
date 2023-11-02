rm(list=ls())
install.packages("igraph")


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
          BeR <- 10^-(sample(3:8, 1))
          
          # Assign values to the arrays for both connections due to symmetry:
          distance_values[i, j, k] <- km; distance_values[j, i, k] <- km
          load_values[i, j, k] <- load; load_values[j, i, k] <- load
          ber_values[i, j, k] <- BeR; ber_values[j, i, k] <- BeR
        }
      }
    }
  }
  
  # Take generated values to the environment:
  assign("distance_values", distance_values, envir = .GlobalEnv)
  assign("load_values", load_values, envir = .GlobalEnv)
  assign("ber_values", ber_values, envir = .GlobalEnv)
}


################################################################################
#                 SELECTION OF BEST PATHS (BASED ON PENALTIES)
################################################################################
calculate_total_cost <- function(distance_km, load, BeR) {
  "This is a function used to compute the total cost of a certain path based on
  distance, load and BeR values"
  
  # Penalization of propagation delay: we penalize 5us for each km in the fiber.
  propagation_delay = 5 * distance_km
  
  # Penalization of transmission queue delay: we penalize 1 us x (1/(1-load)).
  tranmission_queue_delay = 1/(1-load)
  
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
  
  # 2. chosen_distance: it will store the distance between routers of the path
  # that gave the lowest cost.
  chosen_distance <- matrix(NA, nrow = num_states, ncol = num_states)
  
  # 3. chosen_load: it will store the load between routers of the path
  # that gave the lowest cost.
  chosen_load <- matrix(NA, nrow = num_states, ncol = num_states)
  
  # 4. chosen_ber: it will store the BeR between routers of the path
  # that gave the lowest cost.
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
  
  # Take generated values to the environment:
  assign("cost_matrix", cost_matrix, envir = .GlobalEnv)
  assign("chosen_distance", chosen_distance, envir = .GlobalEnv)
  assign("chosen_ber", chosen_ber, envir = .GlobalEnv)
  assign("chosen_load", chosen_load, envir = .GlobalEnv)
}


################################################################################
#                             REPRESENT THE TOPOLOGY
################################################################################
plot_topology <- function(adj_matrix, chosen_distance, chosen_load, chosen_ber){
  "This is a function used to plot the topology of the current environment"
  
  # Call the "igraph" library:
  library(igraph)
  
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
          "\nBeR:", chosen_ber[i,j],
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
get_convergence_epsiode <- function(all_q_tables){
  "This is a function that has the main goal of returning the episode in which
  Q-learning converges"
  
  # Definition of general variables:
  threshold <- 0.01
  convergence_episode <- NA
  
  # Start the process from Q-table number 21, since we consider the 20 previous
  # Q-tables for checking convergence (explained in detail below):
  for (i in 21:length(all_q_tables)){
    current <- all_q_tables[[i]]
    
    # Obtain the 20 previous Q-tables:
    prev_20_q <- all_q_tables[(i-20):(i-1)]
    
    # Initially declare the sum as 0:
    total_sum = 0
    
    # Go through the 20 previous Q-tables and sum them:
    for (j in 1:length(prev_20_q)){
      mat_i <- prev_20_q[[j]]
      total_sum <- total_sum + sum(mat_i[is.finite(mat_i)])
    }
    
    # Obtain the mean:
    prev_20_mean <- total_sum/20
    
    # We consider it has converged when the square sum of the current episode
    # and the mean of the 20 previous ones is less than the threshold.
    sum_current_one <- sum(current[is.finite(current)])
    differ <- (abs(sum(sum_current_one - prev_20_mean)))^2
    if (differ <= 0.01)
      break
  }
  
  # Show the convergence episode:
  cat("Convergence detected at episode:", i, "\n")
}

solve_scenario_qlearning <- function(num_states, adj_matrix, alpha, gamma, epsilon, num_episodes, cost_matrix){
  "This is a function that has the goal of solving a specific scenario using Q-learning"
  
  # Initialize a Q-table with all zeros. Then, any entry in the Q-table that
  # corresponds to a pair of nodes that aren't directly connected is set to
  # negative infinity to ensure those actions are not chosen.
  Q_table <- matrix(0, nrow = num_states, ncol = num_states)
  Q_table[adj_matrix == 0] <- -Inf
  
  # Create two variables "q_table_differences" and "previous_q_table". The former
  # will track the squared difference between Q-tables of consecutive episodes;
  # the latter, on the other side, will help us maintain the previous Q-table.
  q_table_differences <- numeric(num_episodes)
  previous_q_table <- matrix(0, nrow = num_states, ncol = num_states)
  previous_q_table[adj_matrix == 0] <- -Inf  # Initialize with -Inf for invalid actions
  
  # List to store all Q-tables for every episode:
  all_q_tables <- list()  
  
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
    
    # Track the squared difference for finite values:
    differences <- (abs(Q_table - previous_q_table))^2
    
    # By excluding the "Inf" values when calculating the error, we ensure that
    # we get a more representative measure of how much the Q-table has truly
    # changed in areas that can learn and change. It's a way to focus on the 
    # aspects of the Q-table that are relevant to the learning process.
    q_table_differences[episode] <- sum(differences[is.finite(differences)])
    previous_q_table <- Q_table
    all_q_tables[[episode]] <- Q_table
  }
  
  # Take generated values to the environment:
  assign("Q_table", Q_table, envir = .GlobalEnv)
  assign("q_table_differences", q_table_differences, envir = .GlobalEnv)
  assign("all_q_tables", all_q_tables, envir = .GlobalEnv)
  
  # Show generated values:
  plot(q_table_differences, type="l", xlab="Episodes",
       ylab="Squared Difference of Q-Values Between Episodes",
       main="Q-learning for Optimal Path with Convergence Monitoring")
  get_convergence_epsiode(all_q_tables)
}


################################################################################
#                                   BEST PATH
################################################################################
create_graph_from_adj_matrix <- function(adj_matrix, Q_table) {
  "This function is designed to transform an adjacency matrix representation of
  a directed graph and a Q-table into an igraph object with edge weights based
  on the Q-table values"
  
  # Convert Q_table to costs
  costs <- -Q_table
  
  # Create a graph from the adjacency matrix
  graph <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
  
  # Get edge list from graph
  edge_list <- as.matrix(get.edgelist(graph))
  
  # Map node names to node numbers
  node_names <- unique(c(edge_list[, 1], edge_list[, 2]))
  V(graph)$name <- node_names
  node_numbers <- match(node_names, V(graph)$name)
  link_sources <- match(edge_list[,1], V(graph)$name)
  link_destination <- match(edge_list[,2], V(graph)$name)
  
  # Set the edge weights to the costs from the Q_matrix
  for (i in seq_along(link_sources)) {
    E(graph)$weight[i] <- costs[link_sources[i], link_destination[i]]
  }
  
  # Take generated graph to the environment:
  assign("graph", graph, envir = .GlobalEnv)
}

get_best_path_after_learning <- function(graph, start_node, end_node) {
  "Gives the best route from a starting node to a destination node based on the
  highest cumulative reward (lowest negative reward)"
  
  # Use Dijkstra's algorithm to find the shortest path
  shortest_path <- shortest_paths(graph, from = start_node, to = end_node, mode = "out", output = "both")$vpath
  
  # Extract the node indices from the shortest path
  path <- as.numeric(shortest_path[[1]])
  cat("Path from ", start_node, " to ", end_node, " (based on highest cumulative reward):\n")
  
  # Return the path
  print(path)
}


################################################################################
#               VISUALIZE BEST PATH FROM A SPECIFIC NODE TO ANOTHER
################################################################################
visualize_best_path <- function(start_node, destination_node, graph){
  "This is a function that will allows us to visualize the path from a starting
  node to a destination node"
  
  # Display all vertices of the graph in grey
  V(graph)$color <- "grey"
  
  # Display all edges of the graph in grey
  E(graph)$color <- "grey"
  
  # By executing this line, only the start and destination vertices are set to
  # be displayed in yellow, while all other vertices remain grey
  V(graph)$color[c(start_node, destination_node)] <- "yellow"
  
  # Get the path from source to destination
  path <- shortest_paths(graph, from = start_node, to = destination_node, output  = "both")$epath[[1]]
  
  # Color the edges
  E(graph)$color[path] <- "red"
  
  # Plot the graph
  l <- layout.auto(graph)
  plot(graph, edge.arrow.size = 0.5, vertex.label = V(graph)$name, edge.curved = 0.5, layout = l)
}

