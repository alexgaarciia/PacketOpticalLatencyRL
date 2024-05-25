# PacketOpticalLatencyRL
The main goal of this project is to define a framework based on RL to decide
which is the best possible route based on rewards. It will take into account
variables along the lines of the distance, load and BeR (Bit Error Rate). 

The entire code is based on the combination of Dijkstra's algorithm and Q-learning.
This approach leverages the strengths of both techniques. Dijkstra's algorithm
ensures that the routing algorithm selects the best path according to the cumulative
reward, while Q-learning allows the algorithm to adapt and learn these rewards based
on dynamic network conditions and objectives.

## Files available in the repository
### Topology Generation and Solving Algorithms:
- generic_algorithm.R: This file is used to generate and solve random topologies of
a number of routers between 6 and 20. It contains a random scenario at the end
of the file to check its functionality. Parameters that must be specified: 
number of paths, alpha, gamma, epsilon and number of episodes.

- topology_solver.R: This constitutes one of the most important files in the repository.
By indicating parameters like the number of routers, number of paths (possible paths
between routers), alpha, gamma, epsilon, number of episodes and the adjacency matrix,
it can solve a given topology.

## Datasets: 
- crossMatrix_Milano.csv, nodesLabeling_Milano.csv: these files contain information needed to solve the scenario proposed in the file "milano_topology.R". The former is used to know connected routers and the distances between them, whilst the latter contains information related to the nodes.

- crossMatrix_Tokyo.csv, nodesLabeling_Milano.csv: these files contain information needed to solve the scenario proposed in the file "tokyo_topology.R". The former is used to know connected routers and the distances between them, whilst the latter contains information related to the nodes.

### Cases:
- case1_5routers.R: Invokes the file "topology_solver.R" and solves certain scenarios of
a 5-router topology.

- case2_8routers.R: Invokes the file "topology_solver.R" and solves certain scenarios of
a 8-router topology.

- case3_60routers.R: Invokes the file "topology_solver.R" and solves certain scenarios of
a 60-router topology.

- tokyo_topology.R: Invokes the file "topology_solver.R" and solves a Tokyo-based topology.

- milano_topology.R: Invokes the file "topology_solver.R" and solves a Milano-based topology.
