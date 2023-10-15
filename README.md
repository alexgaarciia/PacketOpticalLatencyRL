# PacketOpticalLatencyRL
The main goal of this project is to define a framework based on RL to decide
which is the best possible route based on rewards. It will take into account
variables along the lines of the distance, load and BeR (Bit Error Rate). 

## Files available in the repository
### Algorithms:
- rlearning: This file contains a 4-router topology. It has the main goal of 
testing several proposed scenarios. Firstly, we defined the main scenario by
using an environment, and then performing reinforcement learning on a number
of simulations. With this, we will obtain the immediate rewards, which will be 
useful to guide the learning process. After that, we solve some MDP (Markov Decision
Processes) with specific rewards.

- generic_algorithm: This file is used to generate and solve random topologies of
a number of routers between 6 and 20. Parameters that must be specified: number of paths,
alpha, gamma, epsilon and number of episodes.

- topology_solver: This constitutes one of the most important files in the repository.
By indicating parameters like the number of routers, number of paths (possible paths
between routers), alpha, gamma, epsilon, number of episodes and the adjacency matrix,
it can solve a given topology.

### Cases:
- case1_5routers: Invokes the file "topology_solver" and solves certain scenarios of
a 5-router topology.

- case2_8routers: Invokes the file "topology_solver" and solves certain scenarios of
a 8-router topology.

- case3_60routers: Invokes the file "topology_solver" and solves certain scenarios of
a 60-router topology.
