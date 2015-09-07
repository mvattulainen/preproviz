[![Travis-CI Build Status](https://travis-ci.org/mvattulainen/preproviz.svg?branch=master)](https://travis-ci.org/mvattulainen/preproviz)


# preproviz

Data quality issues such as missing values and outliers are often interdependent, 
which makes preprocessing both time-consuming and leads to suboptimal performance 
in knowledge discovery tasks. Package 'preproviz' supports preprocessing decision 
making by visualizing interdependent data quality issues through means of feature 
construction. The user can define his own application domain specific constructed 
features that express the quality of a data point such as number of missing values
in the point or use nine default features. The outcome can be explored with plot 
methods and the feature constructed data acquired with get methods.
