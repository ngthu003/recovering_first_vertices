Here are two R functions to generate the Uniform Attachment and Preferential Attachment models on trees, and a function to get the graph statistics (vertex degree and phi value):

1.  __[Uniform attachment](https://github.com/thn003/recovering_first_vertices/blob/master/Data%20Generation/Graph%20Generation%20-%20Unif%20Attm%20on%20Tree%20Simulations.R)__
2.  __[Preferential attachment](https://github.com/thn003/recovering_first_vertices/blob/master/Data%20Generation/Graph%20Generation%20-%20Pref%20Attm%20on%20Tree%20Simulations.R)__
3.  __[Get graph statistics](https://github.com/thn003/recovering_first_vertices/blob/master/Data%20Generation/Graph%20Generation%20-%20Get%20Degree%20and%20Phi.R)__

Some measures taken to improve the efficiency of data generation and minimizing the memory cost:
1.  Represent graphs as incidence list: 2 arrays for 2 end vertices of each edge
2.  Initialize 1 array with 2,3,4,...,n, where n is the graph size, representing the new vertex
3.  For Uniform attachment: at step t, sample from 1,2,...,t-1, and append the result to array 2
4.  For Preferential attachment: add a 3rd array which stores all vertices adjacent to any edge and sample from them uniformly
