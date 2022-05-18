rm(list=ls())
library("plyr")
library("dplyr")
library("ggplot2")
library("pacman")
library("rlist")
p_load(
  # tidyverse packages
  "plyr", "tidyverse", "broom", "glue","modelr",
  # data set
  "gapminder"
)

#==============================================
# Description
#==============================================
# Run this script after running main.R
#  to create a table summarizing the results
#  across a set of instruction sets

#==============================================
# Directories
#==============================================
dirs <- list(
  Results = file.path("Results")
)

#==============================================
# Options
#==============================================
instructionSets<- c("Instructions_dag1",
                      "Instructions_dag2",
                      "Instructions_dag2_ixn",
                      "Instructions_dag3_M",
                      "Instructions_dag3_MM2",
                      "Instructions_dag4_Mboth",
                      "Instructions_dag4_CMboth",
                      "Instructions_dag4_Mboth_Cyounger",
                      "Instructions_dag4_Mboth_Colder")

results <- list()
for(instructionSet in instructionSets){
  results[[instructionSet]] <- read.csv(file.path(dirs$Results,instructionSet,"estimate_performance.csv"),
                                        stringsAsFactors = FALSE)
  print(colnames(results[[instructionSet]]))
}

z <- list.rbind(results)
write.csv(z, file.path("Results","allRuns_02122022.csv"))

