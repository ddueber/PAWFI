## Example 1 investigates the ability of PAWFI to detect members of misfitting groups
## Example 2 investgates the performance of PAWFI with correctly fitting models
## Example 3 compares the performance of PAWFI to other approximate fit indices



#################################################################
## Example 1a:                                                 ##
##                                                             ##
## One factor, 8 item model with 2 groups:                     ##
## Group 1: N = 375 (75%), all factor loadings = 0.7071        ## 
## Group 2: N = 125 (25%), Items 5-8 are uncorrelated with 1-4 ##
#################################################################
{
  ## set random seed for replicability
  set.seed(23776545)
  Example_1a_Group_1_Population_Model <- "
    LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8
  "

  Example_1a_Group_2_Population_Model <- "
    LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 0*x5 + 0*x6 + 0*x7 + 0*x8
  " 

  Example_1a_Analysis_Model <- "
    LF =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  "

  Example_1a_Results <- data.frame(matrix(ncol=3, nrow=0))

  for (i in 1:100) {
    Example_1a_Group_1_Data <- simulateData(Example_1a_Group_1_Population_Model, sample.nobs=375, std.lv = TRUE)
    Example_1a_Group_2_Data <- simulateData(Example_1a_Group_2_Population_Model, sample.nobs=125, std.lv = TRUE)
    
    Example_1a_Data    <- rbind(Example_1a_Group_1_Data, Example_1a_Group_2_Data)
    Example_1a_Fitted  <- cfa(Example_1a_Analysis_Model, Example_1a_Data)
    Example_1a_PAWFI   <- parPAWFI(Example_1a_Fitted)
    Example_1a_Results <- rbind(Example_1a_Results, c(Example_1a_PAWFI$PAWFI, 
                                                      sum(Example_1a_PAWFI$RemoveList > 375), 
                                                      length(Example_1a_PAWFI$RemoveList)))
  }

  names(Example_1a_Results) <- c("PAWFI", "Group_2_Removed", "Total_Removed")
  rm(Example_1a_Group_1_Data, Example_1a_Group_2_Data, Example_1a_Data, Example_1a_Fitted, Example_1a_PAWFI)
  write.csv(Example_1a_Results, "Example_1a_Results.csv")
  ## now we needto do some analysis

}

##############################################################
## Example 1b:                                              ##
##                                                          ##
## One factor, 8 item model with 2 groups:                  ##
## Group 1: N = 320 (80%), 1 factor model                   ## 
## Group 2: N = 80  (20%), orthogonal 2 factor model        ##
##############################################################
{
## set random seed for replicability
  set.seed(19559342)
  
  
## Note that when lavaan simulates data, the residual variance is always 1
## So factor loadings are rescaled to make standardized loadings = 1
  
  Example_1b_Group_1_Population_Model <- "
  LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8
  "

  Example_1b_Group_2_Population_Model <- "
  LF1 =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 
  LF2 =~ 1*x5 + 1*x6 + 1*x7 + 1*x8
  LF1 ~~ 0*LF2
  " 

  Example_1b_Analysis_Model <- "
  LF =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  "
  Example_1b_Results <- data.frame(matrix(ncol=3, nrow=0))
  
  for (i in 1:100) {
    Example_1b_Group_1_Data <- simulateData(Example_1b_Group_1_Population_Model, sample.nobs=320, std.lv = TRUE)
    Example_1b_Group_2_Data <- simulateData(Example_1b_Group_2_Population_Model, sample.nobs=80, std.lv = TRUE)
    
    Example_1b_Data    <- rbind(Example_1b_Group_1_Data, Example_1b_Group_2_Data)
    Example_1b_Fitted  <- cfa(Example_1b_Analysis_Model, Example_1b_Data)
    Example_1b_PAWFI   <- parPAWFI(Example_1b_Fitted)
    Example_1b_Results <- rbind(Example_1b_Results, c(Example_1b_PAWFI$PAWFI, 
                                                      sum(Example_1b_PAWFI$RemoveList > 320), 
                                                      length(Example_1b_PAWFI$RemoveList)))
  }
  
  names(Example_1b_Results) <- c("PAWFI", "Group_2_Removed", "Total_Removed")
  rm(Example_1b_Group_1_Data, Example_1b_Group_2_Data, Example_1b_Data, Example_1b_Fitted, Example_1b_PAWFI)
  ## now we needto do some analysis
  write.csv(Example_1b_Results, "Example_1b_Results.csv")
  
}

##############################################################
## Example 2:                                               ##
##                                                          ##
## Distribution of PAWFI when model fits. N = 250, 500      ##
## Example 2a,b: 1 factor model with 12 items               ##
## Example 2c,d: 2 factor model with 8+8 items              ##
##############################################################

## Example 2a
{ 
  ## set random seed for replicability (separate seed for each Example)
  set.seed(77556914)
  
  Example_2a_Population_Model <- "
  LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8 + 1*x9 + 1*x10 + 1*x11 + 1*x12
  "
  Example_2a_Analysis_Model <- "
  LF =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  "
  
  Example_2a_Results <- data.frame(matrix(ncol=6, nrow=0))

  for (i in 1:100) {
    Example_2a_Data    <- simulateData(Example_2a_Population_Model, sample.nobs=250, std.lv = TRUE)
    Example_2a_Fitted  <- cfa(Example_2a_Analysis_Model, Example_2a_Data)
    Example_2a_PAWFI   <- parPAWFI(Example_2a_Fitted)
    Example_2a_Results <- rbind(Example_2a_Results, c(Example_2a_PAWFI$PAWFI,
                                                      lavInspect(Example_2a_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_2a_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_2a_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_2a_Fitted, "fit")["tli"],
                                                      lavInspect(Example_2a_Fitted, "fit")["srmr"]))
  }
  names(Example_2a_Results) <- c("PAWFI", "chisq", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_2a_Data, Example_2a_Fitted, Example_2a_PAWFI)
  write.csv(Example_2a_Results, "Example_2a_Results.csv")
  
}

## Example 2b
{ 
  ## set random seed for replicability (separate seed for each Example)
  set.seed(70204105)
  
  Example_2b_Population_Model <- "
  LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8 + 1*x9 + 1*x10 + 1*x11 + 1*x12
  "
  Example_2b_Analysis_Model <- "
  LF =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  "
  
  Example_2b_Results <- data.frame(matrix(ncol=6, nrow=0))
  
  for (i in 1:100) {
    Example_2b_Data    <- simulateData(Example_2b_Population_Model, sample.nobs=500, std.lv = TRUE)
    Example_2b_Fitted  <- cfa(Example_2b_Analysis_Model, Example_2b_Data)
    Example_2b_PAWFI   <- parPAWFI(Example_2b_Fitted)
    Example_2b_Results <- rbind(Example_2b_Results, c(Example_2b_PAWFI$PAWFI,
                                                      lavInspect(Example_2b_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_2b_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_2b_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_2b_Fitted, "fit")["tli"],
                                                      lavInspect(Example_2b_Fitted, "fit")["srmr"]))
  }
  names(Example_2b_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_2b_Data, Example_2b_Fitted, Example_2b_PAWFI)
  write.csv(Example_2b_Results, "Example_2b_Results.csv")
}

## Example 2c
{ 
  ## set random seed for replicability (separate seed for each Example)
  set.seed(59472800)
  
  Example_2c_Population_Model <- "
  LF_1 =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8
  LF_2 =~ 1*x9 + 1*x10 + 1*x11 + 1*x12 + 1*x13 + 1*x14 + 1*x15 + 1*x16
  LF_1 ~~ 0.2*LF_2
  "
  Example_2c_Analysis_Model <- "
  LF_1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  LF_2 =~ x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16
  LF_1 ~~ LF_2
  "
  
  Example_2c_Results <- data.frame(matrix(ncol=6, nrow=0))
  
  for (i in 1:100) {
    Example_2c_Data    <- simulateData(Example_2c_Population_Model, sample.nobs=250, std.lv = TRUE)
    Example_2c_Fitted  <- cfa(Example_2c_Analysis_Model, Example_2c_Data)
    Example_2c_PAWFI   <- parPAWFI(Example_2c_Fitted)
    Example_2c_Results <- rbind(Example_2c_Results, c(Example_2c_PAWFI$PAWFI,
                                                      lavInspect(Example_2c_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_2c_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_2c_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_2c_Fitted, "fit")["tli"],
                                                      lavInspect(Example_2c_Fitted, "fit")["srmr"]))
  }
  colnames(Example_2c_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_2c_Data, Example_2c_Fitted, Example_2c_PAWFI)
  write.csv(Example_2c_Results, "Example_2c_Results.csv")
}

## Example 2d
{ 
  ## set random seed for replicability (separate seed for each Example)
  set.seed(27328171)
  
  Example_2d_Population_Model <- "
  LF_1 =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8
  LF_2 =~ 1*x9 + 1*x10 + 1*x11 + 1*x12 + 1*x13 + 1*x14 + 1*x15 + 1*x16
  LF_1 ~~ 0.2*LF_2
  "
  Example_2d_Analysis_Model <- "
  LF_1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  LF_2 =~ x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16
  LF_1 ~~ LF_2
  "
  
  Example_2d_Results <- data.frame(matrix(ncol=6, nrow=0))
  
  for (i in 1:100) {
    Example_2d_Data    <- simulateData(Example_2d_Population_Model, sample.nobs=500, std.lv = TRUE)
    Example_2d_Fitted  <- cfa(Example_2d_Analysis_Model, Example_2d_Data)
    Example_2d_PAWFI   <- parPAWFI(Example_2d_Fitted)
    Example_2d_Results <- rbind(Example_2d_Results, c(Example_2d_PAWFI$PAWFI,
                                                      lavInspect(Example_2d_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_2d_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_2d_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_2d_Fitted, "fit")["tli"],
                                                      lavInspect(Example_2d_Fitted, "fit")["srmr"]))
  }
  colnames(Example_2d_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_2d_Data, Example_2d_Fitted, Example_2d_PAWFI)
  write.csv(Example_2d_Results, "Example_2d_Results.csv")
}

#################################################################
## Example 3:                                                  ##
##                                                             ##
## Distribution of PAWFI compared to other AFIs, N = 250, 500  ##
## Example 3a,b: 1 factor model with 12 items,                 ##
##               residual correlation = 0.5                    ##
## Example 2c,d: 2 factor model with 8+8 items,                ##
##               item 8 loads at .577 on both factors          ##
#################################################################

## Example 3a
{ 
  ## set random seed for replicability (separate seed for each Example)
  set.seed(66308320)
  
  Example_3a_Population_Model <- "
  LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8 + 1*x9 + 1*x10 + 1*x11 + 1*x12
  x1 ~~ 0.5*x2
  "
  Example_3a_Analysis_Model <- "
  LF =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  "
  
  Example_3a_Results <- data.frame(matrix(ncol=6, nrow=0))
system.time({  
  for (i in 1:100) {
    Example_3a_Data    <- simulateData(Example_3a_Population_Model, sample.nobs=250, std.lv = TRUE)
    Example_3a_Fitted  <- cfa(Example_3a_Analysis_Model, Example_3a_Data)
    Example_3a_PAWFI   <- parPAWFI(Example_3a_Fitted)
    Example_3a_Results <- rbind(Example_3a_Results, c(Example_3a_PAWFI$PAWFI,
                                                      lavInspect(Example_3a_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_3a_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_3a_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_3a_Fitted, "fit")["tli"],
                                                      lavInspect(Example_3a_Fitted, "fit")["srmr"]))
  }
  names(Example_3a_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_3a_Data, Example_3a_Fitted, Example_3a_PAWFI)
  write.csv(Example_3a_Results, "Example_3a_Results.csv")})
}

## Example 3b
{ 
  ## set random seed for replicability (separate seed for each Example)
  set.seed(50902917)
  
  Example_3b_Population_Model <- "
  LF =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8 + 1*x9 + 1*x10 + 1*x11 + 1*x12
  x1 ~~ 0.5*x2
  "
  Example_3b_Analysis_Model <- "
  LF =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12
  "
  
  Example_3b_Results <- data.frame(matrix(ncol=6, nrow=0))
  
    for (i in 1:100) {
    Example_3b_Data    <- simulateData(Example_3b_Population_Model, sample.nobs=1000, std.lv = TRUE)
    Example_3b_Fitted  <- cfa(Example_3b_Analysis_Model, Example_3b_Data)
    Example_3b_PAWFI   <- parPAWFI(Example_3b_Fitted)
    Example_3b_Results <- rbind(Example_3b_Results, c(Example_3b_PAWFI$PAWFI,
                                                      lavInspect(Example_3b_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_3b_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_3b_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_3b_Fitted, "fit")["tli"],
                                                      lavInspect(Example_3b_Fitted, "fit")["srmr"]))
  }
  colnames(Example_3b_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_3b_Data, Example_3b_Fitted, Example_3b_PAWFI)
  write.csv(Example_3b_Results, "Example_3b_Results.csv")
}

## Example 3c
{ 
  ## set random see for replicability (separate seed for each Example)
  set.seed(60118469)
  
  Example_3c_Population_Model <- "
  LF_1 =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8
  LF_2 =~ 1*x8 + 1*x9 + 1*x10 + 1*x11 + 1*x12 + 1*x13 + 1*x14 + 1*x15 + 1*x16
  LF_1 ~~ 0.2*LF_2
  "
  Example_3c_Analysis_Model <- "
  LF_1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  LF_2 =~ x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16
  LF_1 ~~ LF_2
  "
  
  Example_3c_Results <- data.frame(matrix(ncol=6, nrow=0))
  
  for (i in 1:100) {
    Example_3c_Data    <- simulateData(Example_3c_Population_Model, sample.nobs=250, std.lv = TRUE)
    Example_3c_Fitted  <- cfa(Example_3c_Analysis_Model, Example_3c_Data)
    Example_3c_PAWFI   <- parPAWFI(Example_3c_Fitted)
    Example_3c_Results <- rbind(Example_3c_Results, c(Example_3c_PAWFI$PAWFI,
                                                   lavInspect(Example_3c_Fitted, "fit")["chisq"],
                                                   lavInspect(Example_3c_Fitted, "fit")["rmsea"],
                                                   lavInspect(Example_3c_Fitted, "fit")["cfi"],
                                                   lavInspect(Example_3c_Fitted, "fit")["tli"],
                                                   lavInspect(Example_3c_Fitted, "fit")["srmr"]))
  }
  names(Example_3c_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_3c_Data, Example_3c_Fitted, Example_3c_PAWFI)
  write.csv(Example_3c_Results, "Example_3c_Results.csv")
}

##Example 3d
{ 
  ## set random see for replicability (separate seed for each Example)
  set.seed(80588432)
  
  Example_3d_Population_Model <- "
  LF_1 =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6 + 1*x7 + 1*x8
  LF_2 =~ 1*x8 + 1*x9 + 1*x10 + 1*x11 + 1*x12 + 1*x13 + 1*x14 + 1*x15 + 1*x16
  LF_1 ~~ 0.2*LF_2
  "
  Example_3d_Analysis_Model <- "
  LF_1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
  LF_2 =~ x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16
  LF_1 ~~ LF_2
  "
  
  Example_3d_Results <- data.frame(matrix(ncol=6, nrow=0))
  
  for (i in 1:100) {
    Example_3d_Data    <- simulateData(Example_3d_Population_Model, sample.nobs=1000, std.lv = TRUE)
    Example_3d_Fitted  <- cfa(Example_3d_Analysis_Model, Example_3d_Data)
    Example_3d_PAWFI   <- parPAWFI(Example_3d_Fitted)
    Example_3d_Results <- rbind(Example_3d_Results, c(Example_3d_PAWFI$PAWFI,
                                                      lavInspect(Example_3d_Fitted, "fit")["chisq"],
                                                      lavInspect(Example_3d_Fitted, "fit")["rmsea"],
                                                      lavInspect(Example_3d_Fitted, "fit")["cfi"],
                                                      lavInspect(Example_3d_Fitted, "fit")["tli"],
                                                      lavInspect(Example_3d_Fitted, "fit")["srmr"]))
  }
  names(Example_3d_Results) <- c("PAWFI", "CHISQ", "RMSEA", "CFI", "TLI", "SRMR")
  rm(Example_3d_Data, Example_3d_Fitted, Example_3d_PAWFI)
  write.csv(Example_3d_Results, "Example_3d_Results.csv")
}
