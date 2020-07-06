# Recovering the first vertices in a *preferential attachment* model


###### Background Information

> This repository summarizes the main results and findings from my work for the `Honors Thesis in Mathematics` at University of California, San Diego, under the guidance of Professor Ery Arias-Castro. I successfully presented the work in Spring 2020.

> If you are interested in the full findings or the theoretical results (on the asymptotic accuracy and algorithm complexity), please refer to the 
> __[presentation slides](https://github.com/thn003/recovering_first_vertices/blob/master/%5BThu%20Nguyen%5D%20Math%20199H%20-%20Thesis%20Presentation.pdf)__
> and the 
> __[thesis](https://github.com/thn003/recovering_first_vertices/blob/master/%5BThu%20Nguyen%5D%20Math%20199H%20-%20Thesis.pdf)__ for more details.

__Update 06/15/2020:__ The thesis received the __High Distinction Honors award__, the highest such award from the Department of Mathematics at UCSD for the academic year 2019-2020. More information is available [here](https://www.math.ucsd.edu/undergraduate/honors-program/honors-theses/index.html#2019-2020).

***

<br/> 

#### Contents

*  __[Motivation](#motivation)__
*  __[Abstract](#abstract)__
*  __[Results](#results)__
*  __[Repository Contents](#repository-contents)__
*  __[Code](#code)__
*  __[Reference](#reference)__

***

<br/> 

### Motivation

Connectivity is by nature dynamic. A network of such can both grow and shrink over time. Here we desire to know the sources of such network, ie. the very first vertices in that dynamic network. Below is an example of a graph in different forms: fully (and correctly) labeled versus its unlabeled copies.

<img src="https://github.com/thn003/recovering_first_vertices/blob/master/Figures/Labeled%20vs%20Unlabeled%20Graphs.png" width="75%" height="75%" class="center">

We observe that as a network grows (having a lot more vertices), plotting out such network can get infeasible. We thus need a more disciplined approach to the problem. We study a particular case of dynamic graph:

>  *preferential attachment model* on tree graphs, inspired by and often observed to highlight the "the rich get richer" phenomenon

Below is a snapshot of such graph with 50 vertices. Can you guess where vertex 1 is? Can you do better with vertices 1, 2, 3, and so on?

<center>
<img src="https://github.com/thn003/recovering_first_vertices/blob/master/Figures/Graph%20Example%20-%20Pref%20Attm.jpeg" width="60%" height="60%" class="center">
</center>

***

<br/> 

### Abstract

We study the problem of recovering the first vertices in a graph grown under the preferential attachment rules. We focus on tree graphs. We start with and build upon the algorithm introduced by Bubeck, Devroye, and Lugosi in [1] which looks at the largest connected component after removing any one vertex.

We first consider recovering only vertex 1. We provide a more detailed proof of the accuracy of the algorithm, which is taken from [1]. We test the algorithm with simulations over a wide range of graph sizes. Given the computation cost, we propose a two-step procedure which combine that with using the vertex degree to cut down the complexity without giving up much accuracy. We then consider recovering the first L vertices. We propose a sequential algorithm where at each stage we do a local search conditioned on what already know. We conjecture on the expectation of that algorithm and then test the algorithm on simulations.

We find that we can get significantly accurate when recovering vertex 1. However, the problem starts becoming much harder once we want to recover more vertices. In particular, it only takes until the fifth vertex to observe considerable drops in the accuracy. Nonetheless, it appears that it is possible to increase the accuracy greatly if we are willing to return a larger set of suspected vertices.

***

<br/> 

### Results

For an overview of the main results, please refer to this more complete __[README](https://nbviewer.jupyter.org/github/thn003/recovering_first_vertices/blob/master/Honors%20Thesis%20-%20README.ipynb#Motivation)__ page, which is a rendered ipynb file which has enhanced support for displaying math and figures.

***

<br/> 

### Repository Contents

This repository consists of 3 main folders:

1.  __[Data Generation](https://github.com/thn003/recovering_first_vertices/tree/master/Data%20Generation)__: model specification and data generation
2.  __[Recover Vertex 1](https://github.com/thn003/recovering_first_vertices/tree/master/Recover%20Vertex%201)__: explanation of the base and the modified algorithms and their results
3.  __[Recover First Vertices](https://github.com/thn003/recovering_first_vertices/tree/master/Recover%20First%20Vertices)__: explanation of the proposed algorithm and its results

In each folder, we tried to provide as much theoretical details as possible (for example: specifying the growth rule under general and special cases, or commenting on the algorithm theoretical accuracy). However, due to the limit of math writing on Github and Jupyter Notebook, we could only provide so much information. Please refer to the __[presentation slides](https://github.com/thn003/recovering_first_vertices/blob/master/%5BThu%20Nguyen%5D%20Math%20199H%20-%20Thesis%20Presentation.pdf)__ and the 
__[thesis](https://github.com/thn003/recovering_first_vertices/blob/master/%5BThu%20Nguyen%5D%20Math%20199H%20-%20Thesis.pdf)__ for a complete discussion of the problems and the algorithms.

***

<br/> 

### Code

The study was done in R. We have published the code used in the study. The data (which can take a while to generate) are available upon request.

***

<br/> 

### Reference

1.  Sebastien Bubeck, Luc Devroye, and Gabor Lugosi. Finding adam in random growing trees. *Random Structures Algorithms*, 50(2), 2016.
