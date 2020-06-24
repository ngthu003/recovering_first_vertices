#### Model specification

We first specify the growth rule of our model. Let G<sub>0</sub> be a singleton (a single vertex with no edges), and d<sub>t</sub>(u) be the degree of vertex u in the graph G(t). We construct G(t+1) as:

<img src="https://github.com/thn003/recovering_first_vertices/blob/master/Figures/Growth%20rule.png" width="70%" height="60%" class="center">

We note that when &alpha; = 0, we have the __uniform attachement__ model, and when &alpha; = 1 we have the __preferential attachment__ model.

***

#### Code

Here are two R functions to generate the Uniform Attachment and Preferential Attachment models on trees, and a function to get the graph statistics (vertex degree and phi value):

1.  __[Uniform attachment](https://github.com/thn003/recovering_first_vertices/blob/master/Data%20Generation/Graph%20Generation%20-%20Unif%20Attm%20on%20Tree%20Simulations.R)__
2.  __[Preferential attachment](https://github.com/thn003/recovering_first_vertices/blob/master/Data%20Generation/Graph%20Generation%20-%20Pref%20Attm%20on%20Tree%20Simulations.R)__
3.  __[Get graph statistics](https://github.com/thn003/recovering_first_vertices/blob/master/Data%20Generation/Graph%20Generation%20-%20Get%20Degree%20and%20Phi.R)__

Some measures taken to improve the efficiency of data generation and minimizing the memory cost:
1.  Represent graphs as incidence list: 2 arrays for 2 end vertices of each edge
2.  Initialize 1 array with 2,3,4,...,n, where n is the graph size, representing the new vertex
3.  For Uniform attachment: at step t, sample from 1,2,...,t-1, and append the result to array 2
4.  For Preferential attachment: add a 3rd array which stores all vertices adjacent to any edge and sample from them uniformly
