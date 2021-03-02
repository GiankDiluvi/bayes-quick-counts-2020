A general-purpose Bayesian model for estimating election results in Mexico
=================================================
Gian Carlo Diluvi \
Department of Statistics \
Universisty of British Columbia \
STAT 520A: Topics in Bayesian Analysis and Decision Theory
--------------------------------------------------------------------------------

Latest version of the project: [Final version](https://github.com/GiankDiluvi/bayes-quick-counts-2020/blob/master/doc/report/diluvi_520a_project.pdf)

### Abstract

In Mexico, estimating the results of the election the day it takes place is a tradition that gives certainty to the electoral process. For this purpose, electoral authorities organize a quick count, in which a random sample of polling stations is used to estimate the results of the election. Multiple Bayesian models have been used in Mexican quick counts since 2006. However, these models assume that candidates are independent, which has a negative impact on model accuracy. In this work, we propose a general-purpose model that takes care of these issues by forcing the proportions of votes to reside in the probabilistic simplex and assuming an arbitrary covariance structure. We discuss uninformative prior distributions for both the proportions in the simplex and the covariance matrix and compare the model with one of the models used in the 2018 Mexican presidential election.



### Directory roadmap

Each directory includes its own README file. In general:
* `data` is for saving data files.
* `doc` includes the written portions of the project.
* `misc` contains miscelaneous files.
* `src` includes the code developed for the project.
