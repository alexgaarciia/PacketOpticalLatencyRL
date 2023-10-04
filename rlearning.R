################################################################################
#                         MAIN GOAL OF THE PROJECT
################################################################################
# Define a framework based on RL to decide which is the best possible route
# based on rewards. It will take into account the quality of transmission, 
# load and other metrics that could be added to the model.


################################################################################
#                       DEFINITION OF THE MAIN SCENARIO
# the rewards are immediate and are useful to guide the learning process
###############################################################################
# 1. Installation of necessary libraries:
library(ReinforcementLearning)
library(igraph)
rm(list=ls())

# 2. Definition of states and possible actions:
states <- c("s1", "s2", "s3", "s4")
actions <- c("right", "left", "up", "down")

# 3. Definition of the environment:
env <- function(state, action) {
  # With a dataframe maybe, otherwise it is too difficult...
  rr = as.data.frame(matrix(-1,nrow=4,ncol=4)); # fill with -1 a 4x4 matrix
  colnames(rr) = c("right","left","up","down"); rownames(rr)=c("s1","s2","s3","s4")
  rr["s1","right"] = 1/1; rr["s1","down"] = 1/10
  rr["s2","right"] = 1/10; rr["s2","down"] = 1/1; rr["s2","left"] = 1/1;
  rr["s3","right"] = 1/1; rr["s3","up"] = 1/1; rr["s3","left"] = 1/10;
  rr["s4","right"] = 10; rr["s3","up"] = 10; rr["s3","left"] = 1/10; rr["s4","down"] = 1/1;
  
  # Q-table is defined
  ns = as.data.frame(matrix(0,nrow=4,ncol=4));
  colnames(ns) = c("right","left","up","down"); rownames(ns)=c("s1","s2","s3","s4")
  ns["s1","up"] = state("s1"); ns["s1","down"] = state("s3");
  ns["s1","right"] = state("s2"); ns["s1","left"] = state("s1");
  
  ns["s2","up"] = state("s2"); ns["s2","down"] = state("s3");
  ns["s2","right"] = state("s4"); ns["s2","left"] = state("s1");
  
  ns["s3","up"] = state("s2"); ns["s3","down"] = state("s3");
  ns["s3","right"] = state("s4"); ns["s3","left"] = state("s1");
  
  ns["s4","up"] = state("s4"); ns["s4","down"] = state("s3");
  ns["s4","right"] = state("s4"); ns["s4","left"] = state("s2");
  
  next_state <- state
  Inff = -100 
  
  # Here is where the rewards are defined:
  # State s1:
  if (state == state("s1") ) {
    if (action == "down") {
      next_state = state("s3")
      reward = -10 
    }
    if (action == "right") {
      next_state = state("s2")
      reward = -1 
    } else {
      next_state = state("s1")
      reward = Inff
    }
  }
  
  # State s2:
  if (state == state("s2")) {
    if (action == "right") {
      next_state = state("s4")
      reward = -1 # it was wrongly set as -10, corrected 16/09/2023
    }
    if (action == "left") {
      next_state = state("s1") # it was wrongly set as s4, corrected 08/09/2023
      reward = -10   
    }
    if (action == "down") {
      next_state = state("s3")
      reward = -1 
    } else {
      next_state = state("s2")
      reward = Inff
    }
  }
  
  # State s3:
  if (state == state("s3")) {
    if (action == "up") {
      next_state = state("s2")
      reward = -1 
    }
    if (action == "left") {
      next_state = state("s1")
      reward = -10 
    }
    if (action == "right") {
      next_state = state("s4")
      reward = -1 
    } else {
      next_state = state("s3")
      reward = Inff
    }
  }
  
  # State 4:
  if (state == state("s4")) {
    next_state = state("s4")
    reward = Inff
  }
  
  
  if (next_state == state("s4") && state != state("s4")) {
    next_state = state("s4")
    reward <- 100
  }
  
  out <- list(NextState = next_state, Reward = reward)
  return(out)
  
}

# In order to plot the topology, we need to know the table "ns":
ns = as.data.frame(matrix(0,nrow=4,ncol=4));
colnames(ns) = c("right","left","up","down"); rownames(ns)=c("s1","s2","s3","s4")
ns["s1","up"] = state("s1"); ns["s1","down"] = state("s3");
ns["s1","right"] = state("s2"); ns["s1","left"] = state("s1");

ns["s2","up"] = state("s2"); ns["s2","down"] = state("s3");
ns["s2","right"] = state("s4"); ns["s2","left"] = state("s1");

ns["s3","up"] = state("s2"); ns["s3","down"] = state("s3");
ns["s3","right"] = state("s4"); ns["s3","left"] = state("s1");

ns["s4","up"] = state("s4"); ns["s4","down"] = state("s3");
ns["s4","right"] = state("s4"); ns["s4","left"] = state("s2");

# Create the graph
topologia_red <- graph(edges = c(1, 2, 1, 1, 1, 1, 1, 3,
                                 2, 4, 2, 1, 2, 2, 2, 3,
                                 3, 4, 3, 1, 3, 2, 3, 3,
                                 4, 4, 4, 2, 4, 4, 4, 3), n = 4, directed = TRUE)

# Assign names to nodes (routers)
V(topologia_red)$name <- c("S1", "S2", "S3", "S4")

# Define the corresponding directions:
arrow_labels <- c("right", "left", "up", "down",
                  "right", "left", "up", "down",
                  "right", "left", "up", "down",
                  "right", "left", "up", "down")


# Define the layout in a big square
layout <- matrix(c(-2, -2, 2, -2, 2, 2, -2, 2), ncol = 2, byrow = TRUE)

# Plot the graph in a big square
plot(topologia_red, layout = layout, vertex.label = V(topologia_red)$name, 
     edge.label = arrow_labels, edge.arrow.size = 0.5, edge.curved = 0.2)

# 4. Sample N = 1000 random sequences from the environment:
data <- sampleExperience(N = 1000,
                         env = env,
                         states = states,
                         actions = actions)
head(data)

# 5. Define reinforcement learning parameters:
control <- list(alpha = 1, gamma = 0.0005, epsilon = 0.1)

# 6. Perform reinforcement learning:
model <- ReinforcementLearning(data,
                               s = "State",
                               a = "Action",
                               r = "Reward",
                               s_new = "NextState",
                               control = control)

# Print policy: this will help me know the best direction to take from one
# router to another.
# If we want to determine the best way of going from S1 to S3, we just have to
# observe the optimal policy:
  # 1: Take right from S1 --> This will take us to S2.
  # 2: Take down from S2 --> This will take us to S3.
computePolicy(model)

# Print state-action function:
print(model)


################################################################################
#                                 SCENARIO 1
################################################################################
# Install tool use to solve and analyze MDP problems:
library(MDPtoolbox) 

# Scenario 1: Direct path works well QoT = 10^{-5}
QoT = list(e12 = 1e-5, e13=1e-5, e32=1e-5, e14=1e-5, e42=1e-5)
load = list(e12 = 0.3, e13=0.5, e32=0.5, e14=0.1, e42=0.7)
nhops = list(e12 = 1, e13=2, e32=1, e14=2, e42=1)

K=3

# Define the transition probability matrix:
Probs <- array(0, c(4,4,3));  
Probs[1,3,1] = 1; Probs[2,2,1] = 1; Probs[3,3,1]=1; Probs[4,2,1]=1 #NE
Probs[1,2,2] = 1; Probs[2,2,2] = 1; Probs[3,3,2]=1; Probs[4,4,2]=1 #E
Probs[1,4,3] = 1; Probs[2,2,3] = 1; Probs[3,2,3]=1; Probs[4,4,3]=1 #SE

Top = 11
Inff = -100

# Define the reward matrix:
# The rows correspond to the states of the environment:s1, s2, s3 y s4.
# The columns correspond to all the possible actions that can be taken; in this
# case NE, E, SE.
Reward = as.matrix(data.frame(R1=c(log10(1/QoT$e13)-log10(1-load$e13)-K*nhops$e13, # NE
                                   Inff,
                                   Inff,
                                   log10(1/QoT$e42)-log10(1-load$e42)-K*nhops$e42),
                              R2=c(log10(1/QoT$e12)-log10(1-load$e12)-K*nhops$e12, # East
                                   Inff,
                                   Inff,
                                   Inff),
                              R3=c(log10(1/QoT$e14)-log10(1-load$e14)-K*nhops$e14, # SE
                                   Inff,
                                   log10(1/QoT$e32)-log10(1-load$e32)-K*nhops$e32,
                                   Inff))) #SE
print(Reward)

# Solve using an MDP:
dataF = list(P=Probs, R=Reward)
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V) # V optimal value function
print(solver$policy) # policy optimal policy. Each element is an integer
# corresponding to an action which maximizes the value function.
print(solver$time) # CPU time used to run the program


################################################################################
#                                 SCENARIO 2
################################################################################
# Scenario 2: Direct path degrades QoT = 10^{-2}
QoT = list(e12 = 1e-2, e13=1e-5, e32=1e-5, e14=1e-5, e42=1e-5)
load = list(e12 = 0.3, e13=0.5, e32=0.5, e14=0.1, e42=0.7)
nhops = list(e12 = 1, e13=2, e32=1, e14=2, e42=1)

# Again define the transition probability matrix:
Probs <- array(0, c(4,4,3));  
Probs[1,3,1] = 1; Probs[2,2,1] = 1; Probs[3,3,1]=1; Probs[4,2,1]=1 #NE
Probs[1,2,2] = 1; Probs[2,2,2] = 1; Probs[3,3,2]=1; Probs[4,4,2]=1 #E
Probs[1,4,3] = 1; Probs[2,2,3] = 1; Probs[3,2,3]=1; Probs[4,4,3]=1 #SE

# Define the reward matrix:
Reward = as.matrix(data.frame(R1=c(log10(1/QoT$e13)-log10(1-load$e13)-K*nhops$e13, # NE
                                   Inff,
                                   Inff,
                                   log10(1/QoT$e42)-log10(1-load$e42)-K*nhops$e42),
                              R2=c(log10(1/QoT$e12)-log10(1-load$e12)-K*nhops$e12, # East
                                   Inff,
                                   Inff,
                                   Inff),
                              R3=c(log10(1/QoT$e14)-log10(1-load$e14)-K*nhops$e14, # SE
                                   Inff,
                                   log10(1/QoT$e32)-log10(1-load$e32)-K*nhops$e32,
                                   Inff))) #SE

print(Reward)

# Solve the environment using MDP:
dataF = list(P=Probs, R=Reward)# $P = Probs; dataF$R=Reward
#solver = mdp_policy_iteration(P=dataF$P, R=dataF$R, discount=0.9995)
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V)
print(solver$policy)
print(solver$time)


################################################################################
# The main goal of this part is to include the possibility that there are
# multiple paths between routers.
# Trying to store values of QoT, load and nhops in 3D arrays and make the
# algorithm choose the best path in terms of QoT, load and nhops.
################################################################################

# Define the dimensions of the 3D array (num_nodes x num_nodes x num_paths):
num_nodes <- length(states)
num_paths <- 2  # Puedes ajustar esto según la cantidad de caminos posibles

# Create the 3D arrays for each of the variables:
QoT_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
load_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
nhops_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))

# Ahora, agrega los nombres de filas y columnas si es necesario
#rownames(QoT_values) <- colnames(QoT_values) <- states
#rownames(load_values) <- colnames(load_values) <- states
#rownames(nhops_values) <- colnames(nhops_values) <- states

# Define the values of QoT, load and nhops for each of the possible paths
# between states

#  S1-S2
QoT_values[1, 2, 1] <- 1e-5
load_values[1, 2, 1] <- 0.3
nhops_values[1, 2, 1] <- 1

QoT_values[1, 2, 2] <- 1e-5
load_values[1, 2, 2] <- 0.5
nhops_values[1, 2, 2] <- 3

# S1-S3
QoT_values[1, 3, 1] <- 1e-5
load_values[1, 3, 1] <- 0.4
nhops_values[1, 3, 1] <- 3

QoT_values[1, 3, 2] <- 1e-5
load_values[1, 3, 2] <- 0.6
nhops_values[1, 3, 2] <- 4

# S2-S1
QoT_values[2, 1, 1] <- 1e-5
load_values[2, 1, 1] <- 0.3
nhops_values[2, 1, 1] <- 1

QoT_values[2, 1, 2] <- 1e-5
load_values[2, 1, 2] <- 0.1
nhops_values[2, 1, 2] <- 4

# S2-S3
QoT_values[2, 3, 1] <- 1e-5
load_values[2, 3, 1] <- 0.5
nhops_values[2, 3, 1] <- 1

QoT_values[2, 3, 2] <- 1e-5
load_values[2, 3, 2] <- 0.1
nhops_values[2, 3, 2] <- 4

# S2-S4
QoT_values[2, 4, 1] <- 1e-5
load_values[2, 4, 1] <- 0.7
nhops_values[2, 4, 1] <- 1

QoT_values[2, 4, 2] <- 1e-5
load_values[2, 4, 2] <- 0.9
nhops_values[2, 4, 2] <- 2

# S3-S1
QoT_values[3, 1, 1] <- 1e-5
load_values[3, 1, 1] <- 0.5
nhops_values[3, 1, 1] <- 2

QoT_values[3, 1, 2] <- 1e-5
load_values[3, 1, 2] <- 0.5
nhops_values[3, 1, 2] <- 1

# S3-S2
QoT_values[3, 2, 1] <- 1e-5
load_values[3, 2, 1] <- 0.5
nhops_values[3, 2, 1] <- 1

QoT_values[3, 2, 2] <- 1e-5
load_values[3, 2, 2] <- 0.8
nhops_values[3, 2, 2] <- 1

# S3-S4
QoT_values[3, 4, 1] <- 1e-5
load_values[3, 4, 1] <- 0.5
nhops_values[3, 4, 1] <- 1

QoT_values[3, 4, 2] <- 1e-5
load_values[3, 4, 2] <- 0.8
nhops_values[3, 4, 2] <- 1

# S4-S2
QoT_values[4, 2, 1] <- 1e-5
load_values[4, 2, 1] <- 0.7
nhops_values[4, 2, 1] <- 1

QoT_values[4, 2, 2] <- 1e-5
load_values[4, 2, 2] <- 0.5
nhops_values[4, 2, 2] <- 8

# Creation of a function used to compute the total cost of a given path:
calculate_total_cost <- function(qot, carga, nhops) {
  return (qot + (1/(1-qot)) + carga + nhops)
}

# Create lists to store the minimum cost and corresponding values
min_cost_list <- list()
QoT <- list()
loads <- list()
nhops <- list()

# Useful variables
num_rows = length(states)
num_cols = length(actions)
num_layers = 2

# Iterating through the dimensions and calculating the minimum values
for (i in 1:num_rows) {
  for (j in 1:num_cols) {
    min_cost_dim <- Inf
    min_qot_dim <- NULL
    min_load_dim <- NULL
    min_nhops_dim <- NULL
    for (k in 1:num_layers) {
      # Get the values for this dimension
      qot_value <- QoT_values[i, j, k]
      load_value <- load_values[i, j, k]
      nhops_value <- nhops_values[i, j, k]
      
      # Check if any of the values is NA
      if (!anyNA(qot_value) && !anyNA(load_value) && !anyNA(nhops_value)) {
        # Calculate the minimum cost
        cost <- calculate_total_cost(qot_value, load_value, nhops_value)
        
        if (cost < min_cost_dim){
          min_cost_dim <- cost
          min_qot_dim <- qot_value
          min_load_dim <- load_value
          min_nhops_dim <- nhops_value
        }
        
        # Store the minimum cost
        entry_name <- paste0("e", i, j)
        min_cost_list[[entry_name]] <- min_cost_dim
        
        # Store the corresponding values in separate lists
        QoT[[entry_name]] <- min_qot_dim
        loads[[entry_name]] <- min_load_dim
        nhops[[entry_name]] <- min_nhops_dim
      }
    }
  }
}

# Print the lists with the minimum cost and corresponding values
print(min_cost_list)
print(QoT)
print(loads)
print(nhops)

# Create the transition probability matrix:
# Probs[i, j, k] represents the probability that, if the system is in state
# i (dimension 1), the agent takes action j (dimension 2), then the system
# will move to state k (dimension 3).
Probs <- array(0, c(4,4,4));  
Probs[1,3,1] = 1; Probs[2,2,1] = 1; Probs[3,3,1]=1; Probs[4,2,1]=1 #E
Probs[1,2,2] = 1; Probs[2,2,2] = 1; Probs[3,3,2]=1; Probs[4,4,2]=1 #O
Probs[1,4,3] = 1; Probs[2,2,3] = 1; Probs[3,2,3]=1; Probs[4,4,3]=1 #N
Probs[1,4,4] = 1; Probs[2,1,4] = 1; Probs[3,2,4]=1; Probs[4,4,4]=1 #S

K = 10
Inff = -100

# Definition of the reward matrix:
# R1: Right, este.
# R2: Left, oeste.
# R3: Up, norte.
# R4: Down, sur.
Reward = as.matrix(data.frame(
  R1 = c(log10(1/QoT$e12) - log10(1 - load$e12) - K * nhops$e12,log10(1/QoT$e42) - log10(1 - load$e42) - K * nhops$e42, Inff, Inff),
  R2 = c(Inff, log10(1/QoT$e12) - log10(1 - load$e12) - K * nhops$e12, log10(1/QoT$e13) - log10(1 - load$e13) - K * nhops$e13, log10(1/QoT$e42) - log10(1 - load$e42) - K * nhops$e42),
  R3 = c(Inff, Inff, log10(1/QoT$e32) - log10(1 - load$e32) - K * nhops$e32, Inff),
  R4 = c(log10(1/QoT$e13) - log10(1 - load$e13) - K * nhops$e13, log10(1/QoT$e32) - log10(1 - load$e32) - K * nhops$e32, Inff, Inff)
))

print(Reward)

# Solve the MDP
dataF = list(P=Probs, R=Reward)# $P = Probs; dataF$R=Reward
#solver = mdp_policy_iteration(P=dataF$P, R=dataF$R, discount=0.9995)
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V)
print(solver$policy)
print(solver$time)

