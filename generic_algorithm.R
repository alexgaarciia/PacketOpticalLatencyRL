################################################################################
#                      DEFINITION OF GENERIC ALGORITHM
################################################################################
# Define a framework based on RL to decide which is the best possible route
# based on rewards. It will take into account the quality of transmission, 
# load and other metrics that could be added to the model.


################################################################################
#                       GENERATION OF RANDOM SCENARIO
################################################################################
# Select a random number of routers (states) between 4 and 10. Plus,
# create the states s1, s2...
library(ReinforcementLearning)
library(igraph)
library(MDPtoolbox) 
rm(list=ls())
num_states = sample(5:10, 1)

# Creation of states:
states <- c()
for(i in 1:num_states){
  router <- paste("s", i,sep="")
  states <- c(states, router)
}

# Define the possible actions.
actions <- c("right", "left", "up", "down")

# Define the Q-table and randomly define where each router arrives
# with each action.
ns <- as.data.frame(matrix(0,nrow=num_states,ncol=4));
colnames(ns) <- c("right","left","up","down"); rownames(ns)=c(states)

for (router in states) {
  # THIS CODE WAS MODIFIED SO THAT WE CANNOT REACH THE SAME NODE THROUGH DIFFERENT ACTIONS #
  available_states <- setdiff(states, router)  # Exclude the current router from available destinations
  sample_destinations <- sample(available_states, 4)  # Randomly select distinct destinations for each action
  ns[router, actions] <- sample_destinations
}

# Draw the topology
# 1. Define all the edges:
edges = c()
for (i in 1:num_states){
  source = as.numeric(substr(rownames(ns)[i], 2, nchar(rownames(ns)[i])))
  for (j in actions){
    destination = as.numeric(substr(ns[states[i], j], 2, nchar(ns[states[i], j])))
    edges = c(edges, source, destination)
  }
  }
topologia_red <- graph(edges = edges, n = num_states, directed = TRUE)

# 2. Assign names to nodes (routers)
V(topologia_red)$name <- states

# 3. Define the corresponding directions:
arrow_labels = c()
for (i in states){
  # We add the directions for each router. Hence, add 4 directions per router.
  arrow_labels <- c(arrow_labels, "right", "left", "up", "down")
}

# 4. Plot the graph:
plot(topologia_red, vertex.label = V(topologia_red)$name, 
     edge.label = arrow_labels, edge.arrow.size = 0.5, edge.curved = 0.2)


################################################################################
#        GENERATION OF RANDOM VALUES OF DISTANCE, LOAD AND Bit Error Rate
################################################################################
# Define the dimensions of the 3D array (num_nodes x num_nodes x num_paths):
num_nodes <- length(states)
num_paths <- 2  # You can adjust this according to the number of available paths

# Create the 3D arrays for each of the variables:
distance_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
load_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
ber_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))

# Fill the arrays:
for (i in 1:nrow(ns)) {
  source = i
  for (j in 1:ncol(ns)) {
    if (ns[i, j] != rownames(ns)[i]) {
      destination = as.numeric(substr(ns[i, j], 2, nchar(ns[i, j])))
      for (k in 1:num_paths){
        km = runif(1, min = 1, max = 20)
        load = runif(1, min = 0, max = 1)
        BeR <- runif(1, min = 0, max = 10^-6)
        distance_values[source, destination, k] = km
        load_values[source, destination, k] = load
        ber_values[source, destination, k] = BeR
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
# Creation of a function used to compute the total cost of a given path:
calculate_total_cost <- function(distance_km, load, BeR) {
  propagation_delay = 5 * distance_km  # 5us for each km in the fiber.
  tranmission_queue_delay = 1/(1-load)  # 1us x (1/(1-carga))
  
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

# Initialize the lists to store the minimum cost and corresponding values
min_cost_list <- list()
distance <- list()
loads <- list()
nhops <- list()

# Iterate through each source (router) and corresponding destinations
for (source in states) {
  for (dest_action in actions) {
    destination <- ns[source, dest_action]
    
    # Skip if destination is NA (no connection)
    if (is.na(destination)){
      next
    }
    
    # Construct the entry name in the format eXY
    entry_name <- paste0("e", substr(source, 2, nchar(source)), substr(destination, 2, nchar(destination)))
    min_cost_dim <- Inf
    min_km_dim <- NULL
    min_load_dim <- NULL
    min_ber_dim <- NULL
    
    for (k in 1:num_paths) {
      # Get the values for this dimension
      km_value <- distance_values[as.numeric(substr(source, 2, nchar(source))), 
                                  as.numeric(substr(destination, 2, nchar(destination))), k]
      load_value <- load_values[as.numeric(substr(source, 2, nchar(source))), 
                                as.numeric(substr(destination, 2, nchar(destination))), k]
      ber_value <- ber_values[as.numeric(substr(source, 2, nchar(source))), 
                              as.numeric(substr(destination, 2, nchar(destination))), k]
      
      # Check if any of the values is NA
      if (!anyNA(km_value) && !anyNA(load_value) && !anyNA(ber_value)) {
        # Calculate the minimum cost
        cost <- calculate_total_cost(km_value, load_value, ber_value)
        
        if (cost < min_cost_dim) {
          min_cost_dim <- cost
          min_km_dim <- km_value
          min_load_dim <- load_value
          min_ber_dim <- ber_value
        }
      }
    }
    
    # Store the minimum cost and corresponding values
    min_cost_list[[entry_name]] <- min_cost_dim
    distance[[entry_name]] <- min_km_dim
    loads[[entry_name]] <- min_load_dim
    nhops[[entry_name]] <- min_ber_dim
  }
}

# Print the lists with the minimum cost and corresponding values:
print(min_cost_list)
print(distance)
print(loads)
print(nhops)


################################################################################
#                         SOLVE THE PROPOSED SCENARIO
################################################################################
# Generate the transition probabilities randomly.
# This approach ensures that the transition probability matrix complies with the
# fundamental property that the sum of transition probabilities from a given
# state for a specific action is equal to 1.
num_actions <- 4
Probs <- array(0, c(num_states, num_states, num_actions))

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

