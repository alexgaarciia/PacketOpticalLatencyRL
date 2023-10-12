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

# 2. Create the graph edges based on the scenario
edges_vector <- c("s1", "s2",
                  "s1", "s3",
                  "s2", "s1",
                  "s2", "s3",
                  "s2", "s4",
                  "s3", "s1",
                  "s3", "s2",
                  "s3", "s5",
                  "s4", "s2",
                  "s4", "s5",
                  "s5", "s3",
                  "s5", "s4")

# 3. Create the graph
g <- graph(edges_vector, directed = FALSE)

# 4. Plot the graph
plot(g, 
     vertex.color = "lightblue", 
     vertex.size = 50, 
     edge.arrow.size = 0.5, 
     vertex.label.color = "black",
     main = "Scenario Graph")


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
        BeR <- runif(1, min = 0, max = 10^-6)
        
        # Assign values to the arrays for both connections due to symmetry
        distance_values[i, j, k] <- km
        distance_values[j, i, k] <- km
        load_values[i, j, k] <- load
        load_values[j, i, k] <- load
        ber_values[i, j, k] <- BeR
        ber_values[j, i, k] <- BeR
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
  } else if (BeR > 10^-5){
    ber_penalty = 50
  } else {
    ber_penalty = 0
  }
  
  # Sum of penalties:
  return (propagation_delay + tranmission_queue_delay + ber_penalty)
}

# Construct a weight matrix for the graph edges based on the cost
cost_matrix <- matrix(Inf, nrow = num_nodes, ncol = num_nodes)

for (i in 1:num_nodes) {
  for (j in 1:num_nodes) {
    if (adj_matrix[i,j] == 1) {
      min_cost <- Inf
      for (k in 1:num_paths) {
        km_value <- distance_values[i, j, k]
        load_value <- load_values[i, j, k]
        ber_value <- ber_values[i, j, k]
        if (!anyNA(c(km_value, load_value, ber_value))) {
          cost <- calculate_total_cost(km_value, load_value, ber_value)
          min_cost <- min(min_cost, cost)
        }
      }
      cost_matrix[i, j] <- -min_cost
    }
  }
}

print(cost_matrix)


################################################################################
#             SOLVE THE PROPOSED SCENARIO USING Q-LEARNING
################################################################################
# Defin the Q-table and fill it with -Inf in case there are no connections.
Q_table <- matrix(0, nrow = num_nodes, ncol = num_nodes)
Q_table[adj_matrix == 0] <- -Inf

# Define the parameters for Q-learning.
alpha <- 0.1  # learning rate
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
    
    reward <- -cost_matrix[state, action] # agent tries to maximize its reward by minimizing the cost.
    future_reward <- gamma * max(Q_table[action, ])
    Q_table[state, action] <- (1-alpha)*Q_table[state, action] + alpha*(reward + future_reward)
    
    state <- action  
    
  }
}


################################################################################
#                                   BEST PATH
################################################################################
# After learning, we can use the Q-table to take decisions:
start_node <- 2
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

print(path)


################################################################################
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
ber_values[1,2,1] = 10^-5
ber_values[1,3,1] = 10^-5
ber_values[2,3,1] = 10^-4
ber_values[2,4,1] = 10^-6
ber_values[3,5,1] = 10^-6
ber_values[4,5,1] = 10^-6

ber_values[2,1,1] = 10^-5
ber_values[3,1,1] = 10^-5
ber_values[3,2,1] = 10^-4
ber_values[4,2,1] = 10^-6
ber_values[5,3,1] = 10^-6
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

