# PAWFI
R code for the People Are Weird Fit Index

People are Weird Fit Index (PAWFI)

Author: David Dueber, University of Kentucky
Current Version: 1.0, 10/27/2018 

Removes cases from the data set until the chi-squared test of model fit is  non-significant. The proportion of people remaning is the PAWFI, Input is a fitted lavaan model, and the output is the PAWFI and the list of people removed (row numbers of data set). 
  
List of people with largest change in chi-squared changes drastically from step to step, so I have not been able to come up with any time-saving tricks like only checking a small number of cases at each step 
       
Finally, please note that there is no guarantee that the solution found by this greedy algorithm is optimal. The PAWFI computed herein is only an upper bound for the true PAWFI    
