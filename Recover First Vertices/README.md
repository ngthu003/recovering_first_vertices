Here are several R functions whose goal is to recover the first vertices in a preferential attachment model on tree, (in fact, the functions can take as input any tree graph and will output a set of vertices deemed most likely to be the first vertices):

1.  __[Sequential local search](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Root-finding%20Algorithm%20-%20Recover%20L%20Vertices%20-%20Local%20Search%20-%20Constant%20K.R)__
2.  __[Simple search](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Root-finding%20Algorithm%20-%20Recover%20L%20Vertices%20-%20Naive%20Approach.R)__

As explained in __[README](https://github.com/thn003/recovering_first_vertices/blob/master/README.ipynb)__, we proposed a *sequential local search*, in which we sequentially search, return most probable vertices, update and repeat for each of the L vertices to be returned.

We also compared that with the simple search, in which we simply return the vertices with the smallest phi values.

In implementing the algorithms, we used the packages "igraph" most of the time: the package provides functions to compute vertex degree, taking subgraph and computing component sizes.
