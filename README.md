Illustrating Statistical Procedures


Procedure 5.1

# Introduction

This is a bare-bones introduction to [ggplot2](http://had.co.nz/ggplot2/), a visualization package in R. It assumes no knowledge of R. 

There is also a literate programming version of this tutorial in [`ggplot2-tutorial.R`](https://github.com/echen/ggplot2-tutorial/blob/master/ggplot2-tutorial.R).

# Preview

Let's start with a preview of what ggplot2 can do.

Given Fisher's [iris](http://en.wikipedia.org/wiki/Iris_flower_data_set) data set and one simple command...

    qplot(Sepal.Length, Petal.Length, data = iris, color = Species)
    
...we can produce this plot of sepal length vs. petal length, colored by species.

[![Sepal vs. Petal, Colored by Species](http://dl.dropbox.com/u/10506/blog/r/ggplot2/sepal-vs-petal-specied.png)](http://dl.dropbox.com/u/10506/blog/r/ggplot2/sepal-vs-petal-specied.png)

# Installation

You can download R [here](http://cran.opensourceresources.org/). After installation, you can launch R in interactive mode by either typing `R` on the command line or opening the standard GUI (which should have been included in the download).

# R Basics

## Vectors

Vectors are a core data structure in R, and are created with `c()`. Elements in a vector must be of the same type.

	numbers = c(23, 13, 5, 7, 31)
	names = c("edwin", "alice", "bob")
		
Elements are indexed starting at 1, and are accessed with `[]` notation.

	numbers[1] # 23
	names[1] # edwin