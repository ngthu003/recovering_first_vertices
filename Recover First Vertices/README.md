#### Code

Here are several R functions whose goal is to recover the first vertices in a preferential attachment model on tree, (in fact, the functions can take as input any tree graph and will output a set of vertices deemed most likely to be the first vertices):

1.  __[Get neighboring vertices](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Code/Get%20Neighboring%20Vertices.R)__: to return list of vertices adjacent to any particular vertex
2.  __[Sequential local search](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Root-finding%20Algorithm%20-%20Recover%20L%20Vertices%20-%20Local%20Search%20-%20Constant%20K.R)__: to iteratively conditioned on current knowledge look for the most probable vertices at each stage, corresponding to the Algorithm 3 below
3.  __[Simple search](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Root-finding%20Algorithm%20-%20Recover%20L%20Vertices%20-%20Naive%20Approach.R)__: to simply return vertices with the smallest phi values, corresponding to the Algorithm 1 below
4.  __[Local search with partial knowledge](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Root-finding%20Algorithm%20-%20Recover%20L%20Vertices%20-%20with%20Partial%20Knowledge.R)__: to recover a vertex i given knowledge of vertices preceding i (if we know all of the vertices {1,2,...,i-1}, this amounts to a local search with perfect knowledge)

Second is a script to plot out the recovery rates:

1.  __[Convergence Analysis - Recover L Vertices - 1](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Code/Convergence%20Analysis%20-%20Recover%20L%20Vertices%20-%201.R)__
1.  __[Convergence Analysis - Recover L Vertices - 2](https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Code/Convergence%20Analysis%20-%20Recover%20L%20Vertices%20-%202.R)__

As explained in __[README](https://github.com/thn003/recovering_first_vertices/blob/master/README.ipynb)__, we proposed a *sequential local search*, in which we sequentially search, return most probable vertices, update and repeat for each of the L vertices to be returned.

We also compared that with the simple search, in which we simply return the vertices with the smallest phi values. The third sciprt is to recover certain vertex assuming that we know some of previous vertices, for example recovering vertex 5 given that we know either vertices {1,2,3} or {1,2,4} or {1,3,6}, etc.

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Figures/Github%20Readme%20-%20Recover%20L%20-%20Algorithm%203.png" width="70%" height="60%" class="center">
>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Figures/Github%20Readme%20-%20Recover%201%20-%20Algorithm%201.png" width="70%" height="60%" class="center">


In implementing the algorithms, we used the packages "igraph" most of the time: the package provides functions to compute vertex degree, taking subgraph and computing component sizes.

***

#### Results (Not exhaustive)

1.  The recovery rate of the first 5 and first 10 vertices on graph of size 50,000:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Figures/Pref%20Attm%20-%20Recover%20L%20Vertices%20-%20Local%20Search%20-%20Phi%20-%20n%20%3D%2050%2C000.jpeg" width="70%" height="60%" class="center">

2.  The recovery rate of the first 5 vertices over a wide range of graph sizes:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Figures/Pref%20Attm%20-%20Recover%205%20Vertices%20-%20Local%20Search%20-%20Subgraphs%20-%20K%20%3D%2010%2C20%2C30%2C40%2C50.jpeg" width="70%" height="60%" class="center">

3.  Comparison of recovery rate between our *sequential local search* versus a *simple search*:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Figures/Pref%20Attm%20-%20Recover%205%20and%2010%20Vertices%20-%20Comparison%20-%20Local%20Search%20vs%20Simple%20Search.jpeg" width="70%" height="60%" class="center">

4.  The recovery rate of recovering vertex i given perfect knowledge of all vertices preceding i:

>  <img src="https://github.com/thn003/recovering_first_vertices/blob/master/Recover%20First%20Vertices/Figures/Recover%20First%20Vertices%20with%20Partial%20%26%20Perfect%20Knowledge/Pref%20Attm%20-%20Recover%20Vertices%20on%20Perfect%20Knowledge%20-%20Subgraphs%20-%20n%20%3D%2050%2C000.jpeg" width="70%" height="60%" class="center">

For further results, please refer to the [Figures folder](https://github.com/thn003/recovering_first_vertices/tree/master/Recover%20First%20Vertices/Figures) for the figures and the [Thesis](https://github.com/thn003/recovering_first_vertices/blob/master/%5BThu%20Nguyen%5D%20Math%20199H%20-%20Thesis.pdf) for the discussion.
