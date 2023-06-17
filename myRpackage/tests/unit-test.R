library(testthat)
library(usethis)
library(devtools)
library(here)

#this script is for unit test of the package
## Importing
source("~/Documents/GitHub/group-project-team-13/final-report/mypackage/R/functions.R")

df <- getData()
df_clean <- RDPrepro(df)


## Visualizations
CADPlot(df_clean)
CFPlot(df_clean)
TRCLPlot(df_clean)
CSYPlot(df_clean)
CSPlot(df_clean)
ICTSPlot(df_clean)

## Predictive Analysis
df_new <- CDPrepro(df_clean)
TCMat(df_new)
DP(df_new)
train <- DP(df_new)[[1]]
test <- DP(df_new)[[2]]

glm_fit <- LRFit(train)
predicted_classes <- LRPred(glm_fit, test)
LRCMat(predicted_classes, test)


##shiny
run_app()
