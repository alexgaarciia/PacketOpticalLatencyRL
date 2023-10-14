################################################################################
#                      DEFINITION OF GENERIC ALGORITHM
################################################################################
# Define a framework based on RL to decide which is the best possible route
# based on rewards. It will take into account the quality of transmission, 
# load and other metrics that could be added to the model.


################################################################################
#                           GENERATION OF SCENARIO
################################################################################
# Define the necessary libraries that will be used.
library(ReinforcementLearning)
library(igraph)
library(MDPtoolbox) 
rm(list=ls())

# Select 5 routers and create the connexions.
num_states = 5

# 1. Creation of states:
states <- paste0("s", 1:num_states)


################################################################################
#        GENERATION OF RANDOM VALUES OF DISTANCE, LOAD AND Bit Error Rate
################################################################################
num_paths <- 2  # Adjust this according to the number of available paths
num_nodes <- 5  # This represents the number of routers

# Define the adjacency matrix. 1 means nodes are connected, 0 means they aren't.
adj_matrix <- matrix(c(
  0, 1, 1, 0, 0, 
  1, 0, 1, 1, 0, 
  1, 1, 0, 0, 1, 
  0, 1, 0, 0, 1, 
  0, 0, 1, 1, 0  
), nrow=num_nodes, byrow=TRUE)

# Create the 3D arrays for each of the variables:
distance_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
load_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
ber_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))

# Fill the arrays:
for (i in 1:(num_nodes-1)) {  # Adjusted to avoid going out of bounds
  for (j in (i+1):num_nodes) {  
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

# Check generated values:
print(distance_values)
print(load_values)
print(ber_values)


################################################################################
#                 SELECTION OF BEST PATHS (BASED ON PENALTIES)
################################################################################
# Defined a function called "calculate_total_cost" to compute the cost of a certain
# path based on km, load and BeR values.
calculate_total_cost <- function(distance_km, load, BeR) {
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

# Construct 3 matrices that will store the values of the shortest paths:
chosen_distance <- matrix(NA, nrow = num_nodes, ncol = num_nodes)
chosen_ber <- matrix(NA, nrow = num_nodes, ncol = num_nodes)
chosen_load <- matrix(NA, nrow = num_nodes, ncol = num_nodes)

# Construct a weight matrix for the graph edges based on the cost
cost_matrix <- matrix(-Inf, nrow = num_nodes, ncol = num_nodes)

for (i in 1:num_nodes) {
  for (j in 1:num_nodes) {
    if (adj_matrix[i,j] == 1) {
      min_cost <- Inf
      chosen_path_k <- NA  # Track the path 'k' that gave the minimum cost
      for (k in 1:num_paths) {
        km_value <- distance_values[i, j, k]
        load_value <- load_values[i, j, k]
        ber_value <- ber_values[i, j, k]
        if (!anyNA(c(km_value, load_value, ber_value))) {
          cost <- calculate_total_cost(km_value, load_value, ber_value)
          min_cost <- min(min_cost, cost)
          chosen_path_k <- k  # update the chosen path
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

print(cost_matrix)
print(chosen_distance)
print(chosen_load)
print(chosen_ber)

################################################################################
#                             REPRESENT THE TOPLOGY
################################################################################
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
      cat(label)
      edge_labels = c(edge_labels, label)
    }
  }
}

# Plot the graph with edge labels
plot(g, 
     vertex.color = "lightblue", 
     vertex.size = 50, 
     edge.arrow.size = 0.5, 
     vertex.label.color = "black",
     main = "5-Router Topology",
     edge.label = edge_labels,
     vertex.label.cex = 0.7,  # Adjusts the vertex label size
     edge.label.cex = 0.8)    # Adjusts the edge label size)


################################################################################
#             SOLVE THE PROPOSED SCENARIO USING Q-LEARNING
################################################################################
# Define the Q-table and fill it with -Inf in case there are no connections.
Q_table <- matrix(0, nrow = num_nodes, ncol = num_nodes)
Q_table[adj_matrix == 0] <- -Inf

# Define the parameters for Q-learning.
alpha <- 0.5  # learning rate
gamma <- 0.9  # discount factor
epsilon <- 0.1  # exploration rate

# Explore the environment and learn:
# num_episodes: this sets the number of episodes the Q-learning algorithm will
# run. An episode is one complete journey from a starting state to an end or a
# terminal state.
num_episodes <- 1000 
for (episode in 1:num_episodes) {
  # Randomly choose a starting state.
  state <- sample(1:num_nodes, 1)  
  
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

print(Q_table)


################################################################################
#                                   BEST PATH
################################################################################
# After learning, we can use the Q-table to take decisions:
start_node <- 1
end_node <- 5
current_node <- start_node
path <- c(start_node)
visited <- c()

# Get the best path:
while (current_node != end_node) {
  # Sort Q-values for the current node in descending order
  sorted_nodes <- order(Q_table[current_node, ], decreasing = TRUE)
  
  # Find the best unvisited node
  next_node <- NULL
  for (node in sorted_nodes) {
    if (!node %in% visited) {
      next_node <- node
      break
    }
  }
  
  # If we can't find any unvisited nodes, break
  if (is.null(next_node)) {
    cat("All potential paths from the current node have been visited. Exiting...\n")
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
print(path)


################################################################################
distance_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
load_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
ber_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
chosen_distance <- matrix(NA, nrow = num_nodes, ncol = num_nodes)
chosen_ber <- matrix(NA, nrow = num_nodes, ncol = num_nodes)
chosen_load <- matrix(NA, nrow = num_nodes, ncol = num_nodes)
cost_matrix <- matrix(-Inf, nrow = num_nodes, ncol = num_nodes)
Q_table <- matrix(0, nrow = num_nodes, ncol = num_nodes)

#Distance
distance_values[1,2,1] = 6
distance_values[1,3,1] = 1
distance_values[2,3,1] = 7
distance_values[2,4,1] = 5
distance_values[3,5,1] = 2
distance_values[4,5,1] = 10

distance_values[2,1,1] <- 6
distance_values[3,1,1] <- 1
distance_values[3,2,1] <- 7
distance_values[4,2,1] <- 5
distance_values[5,3,1] <- 2
distance_values[5,4,1] <- 10

# BeR
ber_values[1,2,1] = 1
ber_values[1,3,1] = 10^-4
ber_values[2,3,1] = 10^-4
ber_values[2,4,1] = 10^-6
ber_values[3,5,1] = 1
ber_values[4,5,1] = 10^-6

ber_values[2,1,1] = 1
ber_values[3,1,1] = 10^-4
ber_values[3,2,1] = 10^-4
ber_values[4,2,1] = 10^-6
ber_values[5,3,1] = 1
ber_values[5,4,1] = 10^-6

# Load
load_values[1,2,1] = 0.7
load_values[1,3,1] = 0.5
load_values[2,3,1] = 0.6
load_values[2,4,1] = 0.1
load_values[3,5,1] = 0.2
load_values[4,5,1] = 0.9

load_values[2,1,1] = 0.7
load_values[3,1,1] = 0.5
load_values[3,2,1] = 0.6
load_values[4,2,1] = 0.1
load_values[5,3,1] = 0.2
load_values[5,4,1] = 0.9

