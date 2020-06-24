#### Code

First are two R functions to recover vertex 1 given the vertex degree and phi values:

1.  __[Single method algorithm](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Code/Root-finding%20Algorithm%20-%20Recover%20Vertex%201.R)__: to return the recovery rates when using either phi value or vertex degree as the (only) criteria
2.  __[Combination of using phi and vertex degree algorithm](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Code/Root-finding%20Algorithm%20-%20Recover%20Vertex%201%20-%20Phi%20on%20Degree.R)__: to return the recovery rates after a 2-step process: (1) select a small set of vertices with highest degrees, and (2) return vertices with the smallest phi values among those vertices from (1)

Second is a script to plot out the recovery rates:

3.  __[Convergence Analysis - Recover Vertex 1](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Code/Convergence%20Analysis%20-%20Recover%20Vertex%201.R)__

As explained in __[README](https://github.com/thn003/recovering_first_vertices/blob/master/README.ipynb)__, we started with looking at the phi values (the size of the largest connected component after removing each vertex) and the vertex degree individually, the code of which is in file (1), and then introduced a combination of them, which helped significantly reduce the computation cost without compromising on the accuracy, the code of which is in file (2).

In implementing the algorithms, we used the packages "igraph" most of the time: the package provides functions to compute vertex degree, taking subgraph and computing component sizes.

***

#### Results (Not exhaustive)

1.  The recovery rate if we return K = 20 vertices over a wide range of graph sizes:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Figures/Pref%20Attm%20-%20Recover%201%20-%20Phi%20-%20K%20%3D%2020.jpeg" width="60%" height="60%" class="center">

2.  The recovery rate over a range of K vertices returned on graph of 50,000 vertices:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Figures/Pref%20Attm%20-%20Recover%201%20-%20Phi%20-%20n%20%3D%2050%2C000.jpeg" width="60%" height="60%" class="center">

3.  Comparison of recovery rate when combining using phi and vertex degree to reduce time complexity:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Figures/Pref%20Attm%20-%20Comparison%20-%20Phi%20vs%20Phi%20on%20Degree%20-%20K%20%3D%201%2C2%2C3%2C4%2C5.jpeg" width="60%" height="60%" class="center">

For further results, please refer to the [Figures folder](https://github.com/thn003/recovering_first_vertices/tree/master/Recover%20Vertex%201/Figures).
