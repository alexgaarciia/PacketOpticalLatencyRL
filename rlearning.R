# Reinforcement learning
# Nuestro objetivo: definir un framework basado en RL para decidir encaminamiento
# (mejor ruta posible) basada en recompensas que tengan en cuenta calidad del
# enlace (en errores), carga del enlace en porcentaje (por ejmplo, si un enlace
# es a 100Gb/s y lleva 35Gbps de tráfico, entonces su carga es del 35%) y 
# otras métricas que se puedan añadir al modelo (enlaces de bajo consumo, etc).

# 1. Instalación de librería "ReinforcementLearning":
library(ReinforcementLearning)
rm(list=ls())

# 2. Definición de estados y posibles acciones:
states <- c("s1", "s2", "s3", "s4")
actions <- c("right", "left", "up", "down")

# 3. Definición del environment:
env <- function(state, action) {
  # with a dataframe maybe, otherwise it is too difficult...
  rr = as.data.frame(matrix(-1,nrow=4,ncol=4)); # llenamos de -1 una matrix de 4x4
  colnames(rr) = c("right","left","up","down"); rownames(rr)=c("s1","s2","s3","s4")
  rr["s1","right"] = 1/1; rr["s1","down"] = 1/10
  rr["s2","right"] = 1/10; rr["s2","down"] = 1/1; rr["s2","left"] = 1/1;
  rr["s3","right"] = 1/1; rr["s3","up"] = 1/1; rr["s3","left"] = 1/10;
  rr["s4","right"] = 10; rr["s3","up"] = 10; rr["s3","left"] = 1/10; rr["s4","down"] = 1/1;
  
  # definimos la q-table
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
  Inff = -100 # definimos Inff, que es una reward negativa.
  
  # Aquí comienza la definición de rewards:
  # 1. Estado s1:
  if (state == state("s1") ) {
    if (action == "down") {
      next_state = state("s3")
      reward = -10 # 10 ms
    }
    if (action == "right") {
      next_state = state("s2")
      reward = -1 # 1 ms  
    } else {
      next_state = state("s1")
      reward = Inff
    }
  }
  
  # 2. Estado s2:
  if (state == state("s2")) {
    if (action == "right") {
      next_state = state("s4")
      reward = -1 # 1 ms  # antes ponía -10, corregido el 16/09/2023
    }
    if (action == "left") {
      next_state = state("s1") # antes ponía s4, corregido el 08/09/2023
      reward = -10 # 10 ms  
    }
    if (action == "down") {
      next_state = state("s3")
      reward = -1 # 1 ms
    } else {
      next_state = state("s2")
      reward = Inff
    }
  }
  
  # 3. Estado s3:
  if (state == state("s3")) {
    if (action == "up") {
      next_state = state("s2")
      reward = -1 # 1 ms  
    }
    if (action == "left") {
      next_state = state("s1")
      reward = -10 # 10 ms
    }
    if (action == "right") {
      next_state = state("s4")
      reward = -1 # 10 ms
    } else {
      next_state = state("s3")
      reward = Inff
    }
  }
  
  # 4. Estado 4:
  if (state == state("s4")) {
    next_state = state("s4")
    reward = Inff
  }
  
  
  if (next_state == state("s4") && state != state("s4")) {
    next_state = state("s4")
    reward <- 100
    #} else {
    #  reward = 0
  }
  
  
  # if (next_state == state("s4") && state != state("s4")) {
  #    next_state = state("s4")
  #    reward <- 0
  #} else {
  #  reward = 0
  # }
  
  
  #print(state)
  #print(action)
  #print(reward)
  #print(next_state)
  
  #next_state = ns[state,action]
  #reward = rr[state,action]  
  
  #if (next_state == state("s4") && state != state("s4")) {
  #  next_state = state("s4")
  #  reward <- 100
  #}
  
  out <- list(NextState = next_state, Reward = reward)
  return(out)
}

# 4. Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000,
                         env = env,
                         states = states,
                         actions = actions)
head(data)

# 5. Define reinforcement learning parameters:
control <- list(alpha = 1, gamma = 0.0005, epsilon = 0.1)

# 6. Perform reinforcement learning
model <- ReinforcementLearning(data,
                               s = "State",
                               a = "Action",
                               r = "Reward",
                               s_new = "NextState",
                               control = control)

# Print policy
computePolicy(model)

# Print state-action function
print(model)

# 7. Instalamos herramienta que se utiliza para resolver y analizar problemas de MDP.
library(MDPtoolbox) 


################################################################################
#                                   SCENARIO 1:
################################################################################
# Scenario 1: Direct path works well QoT = 10^{-5}
QoT = list(e12 = 1e-5, e13=1e-5, e32=1e-5, e14=1e-5, e42=1e-5)
load = list(e12 = 0.3, e13=0.5, e32=0.5, e14=0.1, e42=0.7)
nhops = list(e12 = 1, e13=2, e32=1, e14=2, e42=1)

K=3

Probs <- array(0, c(4,4,3));  
Probs[1,3,1] = 1; Probs[2,2,1] = 1; Probs[3,3,1]=1; Probs[4,2,1]=1 #NE
Probs[1,2,2] = 1; Probs[2,2,2] = 1; Probs[3,3,2]=1; Probs[4,4,2]=1 #E
Probs[1,4,3] = 1; Probs[2,2,3] = 1; Probs[3,2,3]=1; Probs[4,4,3]=1 #SE

Top = 11
Inff = -100

# Las filas de la matriz "reward" corresponden con los estados del entorno:
# s1, s2, s3 y s4.

# Las columnas se refieren a todas las acciones que yo puedo tomar. En este caso,
# son NE, E, SE.
Reward = as.matrix(data.frame(R1=c(Top-1,Inff,Inff,Top-2), #NE
                              R2=c(Top-10,Inff,Inff,Inff), #E
                              R3=c(Top-2,Inff,Top-10,Inff))) #SE


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

dataF = list(P=Probs, R=Reward)# $P = Probs; dataF$R=Reward
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V) # V optimal value function
print(solver$policy) # policy optimal policy. Each element is an integer
# corresponding to an action which maximizes the value function.
print(solver$time) # CPU time used to run the program


################################################################################
#                                   SCENARIO 2:
################################################################################
# Scenario 2: Direct path degrades QoT = 10^{-2}
QoT = list(e12 = 1e-2, e13=1e-5, e32=1e-5, e14=1e-5, e42=1e-5)
load = list(e12 = 0.3, e13=0.5, e32=0.5, e14=0.1, e42=0.7)
nhops = list(e12 = 1, e13=2, e32=1, e14=2, e42=1)

Probs <- array(0, c(4,4,3));  
Probs[1,3,1] = 1; Probs[2,2,1] = 1; Probs[3,3,1]=1; Probs[4,2,1]=1 #NE
Probs[1,2,2] = 1; Probs[2,2,2] = 1; Probs[3,3,2]=1; Probs[4,4,2]=1 #E
Probs[1,4,3] = 1; Probs[2,2,3] = 1; Probs[3,2,3]=1; Probs[4,4,3]=1 #SE

#K=10
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
dataF = list(P=Probs, R=Reward)# $P = Probs; dataF$R=Reward
#solver = mdp_policy_iteration(P=dataF$P, R=dataF$R, discount=0.9995)
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V)
print(solver$policy)
print(solver$time)


################################################################################
# Trying to store values of QoT, load and nhops in 3D arrays and make the
# algorithm choose the best path in terms of QoT, load and nhops.
################################################################################

# Definición del array 3D con valores de QoT, carga (load) y nhops
# Supongamos que tienes los valores de QoT, carga y nhops para los caminos entre nodos.
# Los valores son ficticios, ajústalos según tus necesidades reales.

# Dimensiones del array 3D (num_nodes x num_nodes x num_paths)
num_nodes <- length(states)
num_paths <- 2  # Puedes ajustar esto según la cantidad de caminos posibles

# Supongamos que tenemos valores de QoT, carga y nhops para los caminos
# entre nodos s1, s2, s3 y s4.
# En este ejemplo, asignaremos valores ficticios a modo de ejemplo.

# Valores ficticios de QoT, carga (load) y nhops para los caminos entre nodos.
QoT_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
load_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))
nhops_values <- array(NA, dim = c(num_nodes, num_nodes, num_paths))

# Ahora, agrega los nombres de filas y columnas si es necesario
rownames(QoT_values) <- colnames(QoT_values) <- states
rownames(load_values) <- colnames(load_values) <- states
rownames(nhops_values) <- colnames(nhops_values) <- states

# Valores de QoT, carga (load) y nhops para los caminos entre nodos s1, s2, s3 y s4.
# Puedes ajustar estos valores según tus necesidades reales.
# En este ejemplo, utilizamos valores ficticios:
# Supongamos que hay tres caminos posibles entre cada par de nodos.

# Caminos entre s1 y s2
QoT_values[1, 2, 1] <- 1e-5
load_values[1, 2, 1] <- 0.3
nhops_values[1, 2, 1] <- 1

QoT_values[1, 2, 2] <- 1e-5
load_values[1, 2, 2] <- 0.5
nhops_values[1, 2, 2] <- 3


# Caminos entre s1 y s3
QoT_values[1, 3, 1] <- 1e-5
load_values[1, 3, 1] <- 0.4
nhops_values[1, 3, 1] <- 3

QoT_values[1, 3, 2] <- 1e-5
load_values[1, 3, 2] <- 0.6
nhops_values[1, 3, 2] <- 4

# Caminos entre s2 y s1:
QoT_values[2, 1, 1] <- 1e-5
load_values[2, 1, 1] <- 0.3
nhops_values[2, 1, 1] <- 1

QoT_values[2, 1, 2] <- 1e-5
load_values[2, 1, 2] <- 0.1
nhops_values[2, 1, 2] <- 4

# Caminos entre s2 y s3:
QoT_values[2, 3, 1] <- 1e-5
load_values[2, 3, 1] <- 0.5
nhops_values[2, 3, 1] <- 1

QoT_values[2, 3, 2] <- 1e-5
load_values[2, 3, 2] <- 0.1
nhops_values[2, 3, 2] <- 4

# Caminos entre s2 y s4:
QoT_values[2, 4, 1] <- 1e-5
load_values[2, 4, 1] <- 0.7
nhops_values[2, 4, 1] <- 1

QoT_values[2, 4, 2] <- 1e-5
load_values[2, 4, 2] <- 0.9
nhops_values[2, 4, 2] <- 2

# Caminos entre s3 y s1:
QoT_values[3, 1, 1] <- 1e-5
load_values[3, 1, 1] <- 0.5
nhops_values[3, 1, 1] <- 2

QoT_values[3, 1, 2] <- 1e-5
load_values[3, 1, 2] <- 0.5
nhops_values[3, 1, 2] <- 1

# Caminos entre s3 y s2:
QoT_values[3, 2, 1] <- 1e-5
load_values[3, 2, 1] <- 0.5
nhops_values[3, 2, 1] <- 1

QoT_values[3, 2, 2] <- 1e-5
load_values[3, 2, 2] <- 0.8
nhops_values[3, 2, 2] <- 1

# Caminos entre s3 y s4:
QoT_values[3, 4, 1] <- 1e-5
load_values[3, 4, 1] <- 0.5
nhops_values[3, 4, 1] <- 1

QoT_values[3, 4, 2] <- 1e-5
load_values[3, 4, 2] <- 0.8
nhops_values[3, 4, 2] <- 1

# Caminos entre s4 y s2:
QoT_values[4, 2, 1] <- 1e-5
load_values[4, 2, 1] <- 0.7
nhops_values[4, 2, 1] <- 1

QoT_values[4, 2, 2] <- 1e-5
load_values[4, 2, 2] <- 0.5
nhops_values[4, 2, 2] <- 8

# Crear una función para calcular el costo total de un camino dado
calculate_total_cost <- function(qot, carga, nhops) {
  return (qot + (1/(1-qot)) + carga + nhops)
}

# Create lists to store the minimum cost and corresponding values
min_cost_list <- list()
QoT <- list()
loads <- list()
nhops <- list()

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

# Crear la matriz de recompensa:
# Probs[i, j, k] representa la probabilidad de que, si el sistema se encuentra
# en el estado i (dimensión 1), el agente toma la acción j (dimensión 2), y 
# luego el sistema se moverá al estado k (dimensión 3).

Probs <- array(0, c(4,4,4));  
Probs[1,3,1] = 1; Probs[2,2,1] = 1; Probs[3,3,1]=1; Probs[4,2,1]=1 #E
Probs[1,2,2] = 1; Probs[2,2,2] = 1; Probs[3,3,2]=1; Probs[4,4,2]=1 #O
Probs[1,4,3] = 1; Probs[2,2,3] = 1; Probs[3,2,3]=1; Probs[4,4,3]=1 #N
Probs[1,4,4] = 1; Probs[2,1,4] = 1; Probs[3,2,4]=1; Probs[4,4,4]=1 #S

K = 10
Inff = -100

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
dataF = list(P=Probs, R=Reward)# $P = Probs; dataF$R=Reward
#solver = mdp_policy_iteration(P=dataF$P, R=dataF$R, discount=0.9995)
solver = mdp_LP(P=dataF$P, R=dataF$R, discount=0.95)
print(solver$V)
print(solver$policy)
print(solver$time)

