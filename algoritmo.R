################################################################################
#                      DEFINICIÓN DE ALGORITMO GENÉRICO
################################################################################
# Paso 1: Seleccionar un número aleatorio de routers (estados) entre 4 y 10 y
# creamos los estados s1, s2...
library(ReinforcementLearning)
rm(list=ls())
num_states = sample(4:10, 1)

# Creación de estados:
states <- c()
for(i in 1:num_states){
  router <- paste("s", i,sep="")
  states <- c(states, router)
}

# Paso 2: Definimos acciones
actions <- c("right", "left", "up", "down")

# Paso 3: Definimos la Q-table:
ns <- as.data.frame(matrix(0,nrow=num_states,ncol=4));
colnames(ns) <- c("right","left","up","down"); rownames(ns)=c(states)

# Paso 4: Definimos aleatoriamente dónde llega cada router con cada acción:
for (router in states){
  for (action in actions){
    ns[router, action] <- sample(states, 1) 
  }
}

# Paso 5: Definimos el objetivo:
goal_state = sample(states, 1)
Inff = -100

# Paso 6: Definición de environment
env <- function(state, action) {
  next_state <- ns[state, action] 
  
  if (state == next_state) {
    reward <- Inff
  } else if (next_state == goal_state) {
    reward <- -1
  } else {
    reward <- -10
  }
  
  out <- list(NextState = next_state, Reward = reward)
  return(out)
}


################################################################################
#                           OBTENCIÓN DE LA POLICY
################################################################################
# Paso 1: Sample N = 1000 random sequences from the environment
data <- sampleExperience(N = 1000,
                         env = env,
                         states = states,
                         actions = actions)
head(data)

# Paso 2: Define reinforcement learning parameters:
control <- list(alpha = 1, gamma = 0.0005, epsilon = 0.1)

# Paso 3: Perform reinforcement learning
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


################################################################################
#                           CREACIÓN DE ESCENARIOS
################################################################################
# Crear una lista para almacenar la QoT
QoT <- list()

# Conjunto para rastrear conexiones ya creadas
created_connections <- c()

# Calcular la QoT para cada par único de routers
for (router_from in states) {
  for (action in actions) {
    router_to <- ns[router_from, action]
    
    # Evitar duplicados y conexiones de un router a sí mismo
    if (router_from != router_to) {
      # Obtener los números de router sin la letra "s"
      router_from_number <- substr(router_from, 2, nchar(router_from))
      router_to_number <- substr(router_to, 2, nchar(router_to))
      
      # Generar el nombre de la conexión (asegurándose de que esté ordenado)
      connection_name <- paste("e", min(router_from_number, router_to_number), max(router_from_number, router_to_number), sep = "")
      
      # Verificar si la conexión inversa ya está presente en el conjunto
      if (!(connection_name %in% created_connections)) {
        # Agregar la conexión actual al conjunto
        created_connections <- c(created_connections, connection_name)
        
        # Calcular un valor aleatorio para la QoT
        qot_value <- 1e-5  # Puedes ajustar los valores mínimos y máximos según tus necesidades
        
        # Agregar la QoT al nombre correspondiente en la lista
        QoT[[connection_name]] <- qot_value
      }
    }
  }
}

# Crear una lista para almacenar la carga de los enlaces
load <- list()

# Conjunto para rastrear conexiones ya creadas
created_connections <- c()

# Calcular la carga para cada par único de routers
for (router_from in states) {
  for (action in actions) {
    router_to <- ns[router_from, action]
    
    # Evitar duplicados y conexiones de un router a sí mismo
    if (router_from != router_to) {
      # Obtener los números de router sin la letra "s"
      router_from_number <- substr(router_from, 2, nchar(router_from))
      router_to_number <- substr(router_to, 2, nchar(router_to))
      
      # Generar el nombre de la conexión (asegurándose de que esté ordenado)
      connection_name <- paste("e", min(router_from_number, router_to_number), max(router_from_number, router_to_number), sep = "")
      
      # Verificar si la conexión inversa ya está presente en el conjunto
      if (!(connection_name %in% created_connections)) {
        # Agregar la conexión actual al conjunto
        created_connections <- c(created_connections, connection_name)
        
        # Calcular un valor aleatorio para la carga (entre 0 y 1)
        load_value <- runif(1, min = 0, max = 1)
        
        # Agregar la carga al nombre correspondiente en la lista
        load[[connection_name]] <- load_value
      }
    }
  }
}


# Crear una lista para almacenar los valores de hops
nhops <- list()

# Conjunto para rastrear conexiones ya creadas
created_connections <- c()

# Calcular el número de hops para cada par único de routers
for (router_from in states) {
  for (action in actions) {
    router_to <- ns[router_from, action]
    
    # Evitar duplicados y conexiones de un router a sí mismo
    if (router_from != router_to) {
      # Obtener los números de router sin la letra "s"
      router_from_number <- substr(router_from, 2, nchar(router_from))
      router_to_number <- substr(router_to, 2, nchar(router_to))
      
      # Generar el nombre de la conexión (asegurándose de que esté ordenado)
      connection_name <- paste("e", min(router_from_number, router_to_number), max(router_from_number, router_to_number), sep = "")
      
      # Verificar si la conexión inversa ya está presente en el conjunto
      if (!(connection_name %in% created_connections)) {
        # Agregar la conexión actual al conjunto
        created_connections <- c(created_connections, connection_name)
        
        # Calcular un valor aleatorio para el número de nhops (valores entre 1 y 10)
        num_nhops <- sample(1:10, 1)
        
        # Agregar el número de nhops al nombre correspondiente en la lista
        nhops[[connection_name]] <- num_nhops
      }
    }
  }
}

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

