################################################################################
#                      DEFINITION OF GENERIC ALGORITHM
################################################################################
# Define a framework based on RL to decide which is the best possible route
# based on rewards. It will take into account the quality of transmission, 
# load and other metrics that could be added to the model.


################################################################################
#                       GENERATION OF RANDOM SCENARIO
################################################################################
# Select 5 routers and create the connexions.
library(ReinforcementLearning)
library(igraph)
library(MDPtoolbox) 
rm(list=ls())
num_states = 5

# Creation of states:
states <- paste0("s", 1:num_states)

# Create the graph edges based on the scenario
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

# Create the graph
g <- graph(edges_vector, directed = FALSE)

# Plot the graph
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
#                         SOLVE THE PROPOSED SCENARIO
################################################################################
# Generate the transition probabilities randomly.
# This approach ensures that the transition probability matrix complies with the
# fundamental property that the sum of transition probabilities from a given
# state for a specific action is equal to 1.
Probs <- array(0, c(num_states, num_states, num_states))

for (i in 1:num_states) {
  for (j in 1:num_states) {
    for (k in 1:num_actions) {
      Probs[i, j, k] <- runif(1)
      
      # Make sure the sum of the probabilities is 1 for each state and action
      Probs[i, , k] <- Probs[i, , k] / sum(Probs[i, , k])
    }
  }
}

# Define the reward matrix:
# Initialize the reward matrix with zeros
Reward <- matrix(0, nrow = num_states, ncol = num_actions)

# Iterate over each router and action
for (i in 1:num_states) {
  for (j in 1:num_actions) {
    # Get the minimum cost associated with this router-action pair
    destination = as.numeric(substr(ns[states[i], actions[j]], 2, nchar(ns[states[i], actions[j]])))
    entry_name <- paste0("e", i, destination)
    min_cost <- min_cost_list[[entry_name]]
    
    # Assign the reward
    Reward[i, j] <- -min_cost
  }
}

# Print the reward matrix
print(Reward)

# Solve the MDP
dataF = list(P=Probs, R=Reward)# $P = Probs; dataF$R=Reward
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V)
print(solver$policy)
print(solver$time)

