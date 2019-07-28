require(lavaan)
require(parallel)


## To-Do List
## 1. Manage Warnings
## 2. Error Handling
## 3. Fix stopping rule and output for lack of success


########################################################
## People are Weird Fit Index (PAWFI)                 ##
##                                                    ##
## Author: David Dueber, University of Kentucky       ##
## Current Version: 1.0, 10/27/2018                   ##
##                                                    ##
## Removes cases from the data set until the          ##
## chi-squared test of model fit is  non-significant. ##
## The proportion of people remaning is the PAWFI,    ##
## Input is a fitted lavaan model, and the output is  ##
## the PAWFI and the list of people removed (row      ##
## numbers of data set).                              ##
##                                                    ##
## List of people with largest change in chi-squared  ##
## changes drastically from step to step, so I have   ##
## not been able to come up with any time-saving      ##
## tricks like only checking a small number of cases  ##
## at each step                                       ##
##                                                    ##
## Finally, please note that there is no guarantee    ##
## that the solution found by this greedy algorithm is##
## optimal. The PAWFI computed herein is only a lower ##
## bound for the true PAWFI                           ##
########################################################


###########################
## PAWFI Example: HS1939 ##
###########################
{
  # specify the model
  HS.model <- ' visual  =~ x1 + x2 + x3 + x9      
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
x3 ~~ x5'

Fit_HS <- cfa(model = HS.model, data = HolzingerSwineford1939)

system.time(HS_Results <- PAWFI(Fit_HS))
system.time(par_HS_Results <- parPAWFI(Fit_HS))

## Ssee the results... they are the same, Yay!
HS_Results$PAWFI
HS_Results$RemoveList

par_HS_Results$PAWFI
par_HS_Results$RemoveList



## And now let's use a robust estimator just for fun
Fit_HS <- cfa(model = HS.model, data = HolzingerSwineford1939, estimator = "MLR")
HS_Results_Scaled <- parPAWFI(Fit_HS)
HS_Results_Scaled$PAWFI
HS_Results_Scaled$RemoveList
}

#########################################
## People are Weird Fit Index (PAWFI)  ##
##                                     ##   
## Remove people one at a time until   ##
## chi-square is non-significant       ##
#########################################

PAWFI <- function(Fitted, scaled = TRUE) {
  ## Decide whether to use chisq or chisq.scaled (and pvalue or pvalue.scaled) to assess fit
  if (scaled & !is.na(lavInspect(Fitted, "fit")["chisq.scaled"])) {
    TestStat <- "chisq.scaled"
    pval <- "pvalue.scaled"
  } else {
    TestStat <- "chisq"
    pval <- "pvalue"
  }
  
  ## If chi-square is non-significant, we output PAWFI=1 and exit
  if (lavInspect(Fitted, "fit")[pval] > .05) {
    return(list(PAWFI = 0))
  }
  
  ## Grab the data and give the columns names (so that <ordered = lavNames(Fitted, type = "ov.ord"> works right)
  Data <- as.matrix(lavInspect(Fitted, "data"))
  colnames(Data) <- lavNames(Fitted)
  ## Stick an index column at the end of Data so that we can keep track of people according to the original rows of the data
  Data <- cbind(Data, c(1:nrow(Data)))
  ## Initialize the removal vector (of row numbers in initial data set that we need to remove)
  RemoveList <- vector()
  ## Initialize chisq  
  Results <- update(Fitted, data = Data, ordered = lavNames(Fitted, type = "ov.ord"))
  Current_ChiSq <- lavInspect(Results, "fit")[TestStat]
  
  ## Remove people one at a time with JackknifeFit until chisq is non-significant
  ## We are also checking to make sure that chi square is changing fast enough to be worth it
  while (lavInspect(Results, "fit")[pval] < .05) {
    
    ## Let's get a list of people with the lowest chi-square after removal
    FitList <- sapply (1:nrow(Data), JackknifeFit, 
                            J_Fitted = Fitted, 
                            J_Data = Data, 
                            TestStat = TestStat, 
                            OrderedList = lavNames(Fitted, type = "ov.ord"))
    ## Now, let's grab the person with the largest drop in chi-square so we can remove that person
    PersonToRemove <- FitList[1,order(FitList[2,])][1]
    
    ## add the new person to remove to RemoveList
    RemoveList <- c(RemoveList, PersonToRemove[1])
    ## Now keep everyone except the person with the lowest Chi-square after removal
    Data <- Data[Data[,ncol(Data)] != PersonToRemove[1],]
    Results <- update(Fitted, data = Data, ordered = lavNames(Fitted, type = "ov.ord"))
    
    ## Exit the loop if chi squared is not decreasing fast enough (what does that mean??)
    if((lavInspect(Results, "fit")[TestStat]-Current_ChiSq)/Current_ChiSq > (1 - 2/nrow(Data))) {
      break
    }
    ## Update Current_ChiSq
    Current_ChiSq <- lavInspect(Results, "fit")[TestStat]
    
    ## Now ending the "while" loop    
  }  
  
  ## If p >.05 then all is well
  if (lavInspect(Results, "fit")[pval] > .05) {
    ## Shut down the cluster, Return PAWFI and RemoveList
    PAWFI <- length(RemoveList)/(nrow(Data)+length(RemoveList))
    return(list(PAWFI = PAWFI, RemoveList = RemoveList))
  } else {
    ## Things have gone horribly wrong and the user needs to know this.
    return("Something went horribly wrong!")
  }
}

#####################
## End of PAWFI    ##
#####################



#########################################
## People are Weird Fit Index (PAWFI)  ##
## Parallelized version                ##
##                                     ##   
## Remove people one at a time until   ##
## chi-square is non-significant       ##
#########################################

parPAWFI <- function(Fitted, scaled = TRUE, ncores = detectCores(logical = FALSE)) {
  ## Decide whether to use chisq or chisq.scaled (and pvalue or pvalue.scaled) to assess fit
  if (scaled & !is.na(lavInspect(Fitted, "fit")["chisq.scaled"])) {
    TestStat <- "chisq.scaled"
    pval <- "pvalue.scaled"
  } else {
    TestStat <- "chisq"
    pval <- "pvalue"
  }
  
  ## If chi-square is non-significant, we output PAWFI=1 and exit
  if (lavInspect(Fitted, "fit")[pval] > .05) {
    return(list(PAWFI = 0))
  }

  ## Grab the data and give the columns names (so that <ordered = lavNames(Fitted, type = "ov.ord"> works right)
  Data <- as.matrix(lavInspect(Fitted, "data"))
  colnames(Data) <- lavNames(Fitted)
  ## Stick an index column at the end of Data so that we can keep track of people according to the original rows of the data
  Data <- cbind(Data, c(1:nrow(Data)))
  ## Initialize the removal vector (of row numbers in initial data set that we need to remove)
  RemoveList <- vector()
  ## Initialize chisq  
  Results <- update(Fitted, data = Data, ordered = lavNames(Fitted, type = "ov.ord"))
  Current_ChiSq <- lavInspect(Results, "fit")[TestStat]

  ## Lets make a cluster  
  cl <- makeCluster(ncores, type = "PSOCK")
  clusterEvalQ(cl, library(lavaan))


  ## Remove people one at a time with JackknifeFit until chisq is non-significant
  ## We are also checking to make sure that chi square is changing fast enough to be worth it
  while (lavInspect(Results, "fit")[pval] < .05) {
    
    ## Let's get a list of people with the lowest chi-square after removal
    FitList <- parSapplyLB (cl, 1:nrow(Data), JackknifeFit, 
                                     J_Fitted = Fitted, 
                                     J_Data = Data, 
                                     TestStat = TestStat, 
                                     OrderedList = lavNames(Fitted, type = "ov.ord"))
    ## Now, let's grab the person with the largest drop in chi-square so we can remove that person
    PersonToRemove <- FitList[1,order(FitList[2,])][1]
    
    ## add the new person to remove to RemoveList
    RemoveList <- c(RemoveList, PersonToRemove[1])
    ## Now keep everyone except the person with the lowest Chi-square after removal
    Data <- Data[Data[,ncol(Data)] != PersonToRemove[1],]
    Results <- update(Fitted, data = Data, ordered = lavNames(Fitted, type = "ov.ord"))
 
    ## Exit the loop if chi squared is not decreasing fast enough (what does that mean??)
    if((lavInspect(Results, "fit")[TestStat]-Current_ChiSq)/Current_ChiSq > (1 - 2/nrow(Data))) {
      break
    }
    ## Update Current_ChiSq
    Current_ChiSq <- lavInspect(Results, "fit")[TestStat]
    
  ## Now ending the "while" loop    
  }  
  
  ## If p >.05 then all is well
  if (lavInspect(Results, "fit")[pval] > .05) {
    ## Shut down the cluster, Return PAWFI and RemoveList
    PAWFI <- length(RemoveList)/(nrow(Data)+length(RemoveList))
    stopCluster(cl)
    return(list(PAWFI = PAWFI, RemoveList = RemoveList))
  } else {
    ## Things have gone horribly wrong and the user needs to know this. Also, shut down then cluster
    stopCluster(cl)
    return("Something went horribly wrong!")
  }
}

#####################
## End of parPAWFI ##
#####################

########################################
##           Jackknife Fit            ##
##                                    ##
## Look through people one at a time  ##
## and return the list of chi-squared ##
## with person removed                ##
########################################

JackknifeFit <- function(i, J_Fitted, J_Data, TestStat, OrderedList) {
  J_Data_Current <- J_Data[-c(i),]
  CurrentResults <- update(J_Fitted, data = J_Data_Current, ordered = OrderedList)
  c(J_Data[i, ncol(J_Data)], lavInspect(CurrentResults, "fit")[TestStat])
}

##########################
## End of Jackknife Fit ##
##########################
