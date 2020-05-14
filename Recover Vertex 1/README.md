Here are two R functions to recover vertex 1

1.  __[Single method algorithm](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Root-finding%20Algorithm%20-%20Recover%20Vertex%201.R)__
2.  __[Combination of using phi and vertex degree algorithm](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20Vertex%201/Root-finding%20Algorithm%20-%20Recover%20Vertex%201%20-%20Phi%20on%20Degree.R)__

As explained in __[README](https://github.com/thn003/recovering_first_vertices/blob/master/README.ipynb)__, we started with looking at the phi values (the size of the largest connected component after removing each vertex) and the vertex degree individually, the code of which is in file (1), and then introduced a combination of them, which helped significantly reduce the computation cost without compromising on the accuracy, the code of which is in file (2).

In implementing the algorithms, we used the packages "igraph" most of the time: the package provides functions to compute vertex degree, taking subgraph and computing component sizes.
