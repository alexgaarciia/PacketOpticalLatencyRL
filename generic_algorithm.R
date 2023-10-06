################################################################################
#                      DEFINITION OF GENERIC ALGORITHM
################################################################################
# Define a framework based on RL to decide which is the best possible route
# based on rewards. It will take into account the quality of transmission, 
# load and other metrics that could be added to the model.


################################################################################
#                       GENERATION OF RANDOM SCENARIO
################################################################################
# Step 1: Select a random number of routers (states) between 4 and 10. Plus,
# create the states s1, s2...
library(ReinforcementLearning)
library(igraph)
rm(list=ls())
num_states = sample(4:10, 1)

# Creation of states:
states <- c()
for(i in 1:num_states){
  router <- paste("s", i,sep="")
  states <- c(states, router)
}

# Step 2: Define the possible actions.
actions <- c("right", "left", "up", "down")

# Step 3: Define the Q-table and randomly define where each router arrives
# with each action.
ns <- as.data.frame(matrix(0,nrow=num_states,ncol=4));
colnames(ns) <- c("right","left","up","down"); rownames(ns)=c(states)

for (router in states) {
  # THIS CODE WAS MODIFIED SO THAT WE CANNOT REACH THE SAME NODE THROUGH DIFFERENT ACTIONS #
  available_states <- setdiff(states, router)  # Exclude the current router from available destinations
  sample_destinations <- sample(available_states, 4)  # Randomly select distinct destinations for each action
  ns[router, actions] <- sample_destinations
}

# Step 4: Draw the topology
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
num_paths <- 2  # Puedes ajustar esto según la cantidad de caminos posibles

# Create the 3D arrays for each of the variables:
distance_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
load_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
ber_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))


# Fill the lists:
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

# Check generate values:
print(distance_values)
print(load_values)
print(ber_values)


################################################################################
#                           CREACIÓN DE ESCENARIOS
################################################################################
# Generar las probabilidades de transición:
# Definir dimensiones de la matriz Probs (sustituye num_states y num_actions por los valores reales)
num_actions <- 4
Probs <- array(0, c(num_states, num_states, num_actions))

# Generar probabilidades de transición aleatorias
for (i in 1:num_states) {
  for (j in 1:num_states) {
    for (k in 1:num_actions) {
      # Generar una probabilidad de transición aleatoria en el rango [0, 1]
      Probs[i, j, k] <- runif(1)
      
      # Asegurarse de que la suma de las probabilidades sea 1 para cada estado y acción
      Probs[i, , k] <- Probs[i, , k] / sum(Probs[i, , k])
    }
  }
}

