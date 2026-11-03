This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor). 

The problem set "Credit Allocation and Macroeconomic Fluctuations" was developed as part of my master's thesis at Ulm University. It introduces the empirical approach of the paper "Credit Allocation and Macroeconomic Fluctuations" by Karsten Müller and Emil Verner (2024) and explores how lending to tradable sectors, non-tradable sectors, and households relates to macroeconomic outcomes. Through interactive exercises in R, users explore the data, construct key variables, conduct descriptive analyses, and replicate selected results using panel regressions and local projections.

The original paper is available [here](https://doi.org/10.1093/restud/rdad112). The authors' data and Stata replication code can be found [here](https://zenodo.org/records/8347045).

## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
install.packages("RTutor",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))

if (!require(devtools))
  install.packages("devtools")

devtools::install_github("DariaPalitzsch/RTutorCreditAllocation")
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(RTutorCreditAllocation)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/RTutorCreditAllocation")
# Adapt your user name
run.ps(user.name="Jon Doe", package="RTutorCreditAllocation",
       auto.save.code=TRUE, clear.user=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
