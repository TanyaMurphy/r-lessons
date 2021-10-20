
# DRY: Make changes in one place only!
#   Working at home or the office? 
#   See "Specify data folder path" after library() commands

# In the spirit of the Don't Repeat Yourself golden rule in programming, here
# are the options and packages that are common to most of my analysis
# projects

options(show.signif.stars = FALSE, scipen = 7, stringsAsFactors = FALSE,
        knitr.kable.NA = '')
library(kableExtra)
# Data wrangling packages ----------------------------------
# update repo data periodically
options(repos = c(CRAN = "https://mran.revolutionanalytics.com/snapshot/2018-09-01"))
# list last to give tidyverse package functions priority

# Graphing and other output packages -----------------------
library(knitr, quietly = TRUE)
# library(cowplot)
# library(grid)
# library(gridExtra)
# library(eulerr)
library(officer, quietly = TRUE)
library(flextable, quietly = TRUE)
# library(tableone)
# Latent trait analysis packages ----------------------------
library(psych, quietly = TRUE)
library(lavaan, quietly = TRUE)
#library(lavaan.survey, quietly = TRUE)
library(semTools, quietly = TRUE)
#library(semPlot)
library(nFactors, quietly = TRUE)
# library(mvtnorm)
# library(ltm)
library(GPArotation, quietly = TRUE)
library(DAAG, quietly = TRUE)
# Data wrangling packages ----------------------------------
#library(R.utils)
library(forcats, quietly = TRUE)
library(data.table, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(stringr, quietly = TRUE)
#library(purrrlyr, quietly = TRUE)
#library(plyr) # if used, must be loaded before dplyr (in tidyverse)
library(glue)
library(tidyverse, quietly = TRUE)

#(.packages())
# Specify data folder path ----------------------------------
datapath <- "../../../HardDriveOnly/Data"
reportdir <- "../../../Dropbox/_projects/cheat-cfa/tables and figures/"

# Functions -------------
## Some wrapper functions for sig.figs and estimate with SE or CI

# format estimate into a string with "%.xf" sig.figs, 
# with or without concatenated SE or 95% CI in parentheses
FormEst <- function(df, Est = "est", SE = "se", CIL = "ci.lower", 
                    CIU = "ci.upper", whichEst = c("est2", "estSE", "estCI"), 
                    Form = "%.2f"){
  if(whichEst == "est2"){
    x <-  as.character(sprintf(Form, df[, Est]))
  }
  
  if(whichEst == "estSE"){
    x <- paste0(sprintf(Form, df[, Est]), " (", sprintf(Form, df[, SE]), ")")
  }
  if(whichEst == "estCI"){
    x <- paste0(sprintf(Form, df[, Est]), " (", sprintf(Form, df[, CIL]), 
                ", ", sprintf(Form, df[, CIU]), ")")
  }
  d <- c(Est, SE, CIL, CIU)
  y <- as.data.frame(cbind(df[, !(names(df) %in% d)], x))
  names(y)[ncol(y)] <- whichEst 
  return(y)
}  

#FormEst(cfa.pars, whichEst = "estSE")
# Individual functions
FormEstCI <- function(Form = "%.2f", Est = est,
                      CIL = ci.lower, CIU = ci.upper){
  paste0(sprintf(Form, Est), " (", sprintf(Form, CIL), ", ",
         sprintf(Form, CIU), ")")
}

FormEstSE <- function(Form = "%.2f", Est = est, SE = se){
  paste0(sprintf(Form, Est), " (", sprintf(Form, SE), ")")
}

# Gather a type of parameter from lavaan CFA output
CFAParEstims <- function(the.model, the.op){
  x <- parameterEstimates(the.model)  %>% 
    filter(op == the.op & se != 0.000) %>% select(par.cols)
}


