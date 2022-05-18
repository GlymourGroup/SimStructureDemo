rm(list=ls())
library("plyr")
library("dplyr")
library("ggplot2")
library("pacman")
p_load(
  # tidyverse packages
  "plyr", "tidyverse", "broom", "glue","modelr",
  # data set
  "gapminder"
)
#==============================================
# Description
#==============================================
# This file takes instructions for a set of dags and runs each simulation, outputting results
#  to individual files for each DAG

#==============================================
# Options
#==============================================
instructionsNames<- c("Instructions_dag1",
                      "Instructions_dag2",
                      "Instructions_dag2_ixn",
                      "Instructions_dag3_M",
                      "Instructions_dag3_MM2",
                      "Instructions_dag4_Mboth",
                      "Instructions_dag4_CMboth",
                      "Instructions_dag4_Mboth_Cyounger",
                      "Instructions_dag4_Mboth_Colder")

for(instructionsName in instructionsNames){
  options <- list(nIter=5,
                  simulation=list())

  #==============================================
  # Directories
  #==============================================
  #Set up a list of directories
  dirs <- list(
    instructions = file.path("Instructions"),
    helper = file.path("HelperFns"),
    DAGs = file.path("DAGs"),
    SimFns = file.path("SimFns"),
    MeasFns = file.path("MeasurementFns"),
    CombFns = file.path("CombinationFns"),
    MatchQualFns = file.path("MatchQualityFns"),
    AnalysisFns = file.path("AnalysisFns"),
    AggregationFns = file.path("AggregationFns"),
    Results = file.path("Demo", instructionsName)
  )
  dir.create(dirs$Results)

  #==============================================
  # Load instructions
  #==============================================
  instructions <- dget(file.path(dirs$instructions,paste0(instructionsName,".R")))
  saveRDS(instructions,file.path(dirs$Results,"instructions.RDS"))

  #==============================================
  # Load helper functions
  #==============================================
  #Message logger
  msg <- dget(file.path(dirs$helper,"helper_messager.R"))()
  msg$setStatus(1) #Set to 0 to stop printing, 1+ for increasing printing detail

  #Load the helpers that load each type of simulation/analysis functions
  loadSimFns <- dget(file.path(dirs$helper,"helper_loadSimFns.R"))(dirs)
  loadMeasFns <- dget(file.path(dirs$helper,"helper_loadMeasurementFns.R"))(dirs)
  loadCombFns <- dget(file.path(dirs$helper,"helper_loadCombinationFns.R"))(dirs)
  loadMatchQualityFns <- dget(file.path(dirs$helper,"helper_loadMatchQualityFns.R"))(dirs)
  loadAnalysisFns <- dget(file.path(dirs$helper,"helper_loadAnalysisFns.R"))(dirs)

  #==============================================
  # Load simulation and analysis functions
  #==============================================
  #Load DAGs
  dags <- list()
  for(dagName in names(instructions$dags)){
    dags[[dagName]] <- dget(file.path(dirs$DAGs,paste0(dagName,".R")))
  }

  #Load simulation functions
  simFns <- loadSimFns(dags)

  #Load measurement functions
  measFns <- loadMeasFns(instructions)

  #Load combination functions
  combFns <- loadCombFns(instructions)

  #Load match quality filters
  loadMatchQualityFns <- dget(file.path(dirs$helper,
                                        "helper_loadMatchQualityFns.R"))(dirs)

  qualFns <- loadMatchQualityFns(instructions)

  #Load analysis functions
  analysisFns <- loadAnalysisFns(instructions)

  #Set up simulators for each DAG
  simulators <- list()
  for(dagName in names(dags)){
    msg$print(paste0("Simulation order for: ", dagName))
    simulators[[dagName]] <- dget("simulator.R")(options$simulation,
                                                 dags[[dagName]],
                                                 simFns,
                                                 msg)
  }

  #Load aggregation fns
  mseFn <- dget(file.path(dirs$AggregationFns,"MSE.R"))()

  #Load identifiability helper
  identify <- dget(file.path("identify.R"))(instructions$effect)


  #==============================================
  # Do-calculus based analysis
  #==============================================
  id_results <- list()
  id_latex_results <- ""
  for(dagName in names(dags)){
    id_result <- identify(dags[[dagName]])
    id_results[[dagName]] <- id_result
    id_latex_results <- paste0(id_latex_results,dagName,"\n",id_result$effect$P,"\n\n")
    plot(id_result$plot + ggtitle(dagName))
  }

  saveRDS(id_results,file=file.path(dirs$Results,"identifiability_results.rds"))
  write(id_latex_results,file=file.path(dirs$Results,"identifiability_latex.txt"))

  #==============================================
  # Simulation-based analysis
  #==============================================
  #Set up data frame to hold estimates
  estCols <- data.frame(dag=character(),
                        measurement=character(),
                        combining=character(),
                        analysis=character(),
                        qualFn=character(),
                        caliper=numeric())

  #Loop over each DAG, measurement function, and measurement
  #  function specified in the dag to set up results matrix
  for(dagName in names(instructions$dags)){
    dag <- instructions$dags[[dagName]]
    for(measurementName in names(dag$measurement)){
      dagQualFns <- instructions$dags[[dagName]]$matchQuality
      for(qualFnName in names(dagQualFns)){
        calipers <- dagQualFns[[qualFnName]]$options$calipers

        estCols <- rbind(estCols,
                         expand.grid(dag=dagName,
                                     measurement=measurementName,
                                     combining=dag$combining,
                                     analysis=dag$measurement[[measurementName]],
                                     stringsAsFactors=FALSE,
                                     qualFn=qualFnName,
                                     caliper=calipers))
      }
    }
  }

  #Set up data frame to hold estimates from each iteration of the simulation
  estimates <- data.frame(matrix(nrow=options$nIter,
                          ncol=nrow(estCols)))

  #THIS NEEDS FIXING: SCI NOTATION OF CALIPERS NOT WORKING IF CALIPERS CHANGE
  colnames(estimates) <- apply(estCols,1,paste0,collapse="_")

  simSpecs <- estCols %>%
    dplyr::select(-c(analysis,qualFn,caliper)) %>%
    unique()

  #Place to keep track of which simulations fail
  simFailures <- estimates #consider sparse matrix formulation if performance sucks
  ns <- estimates #consider sparse matrix formulation if performance sucks

  #Calculate Truths
  truths <- estimates[0,]
  truths[1,] <- NA
  rownames(truths) <- c("TrueBigPop")#,
  #These would have to be calculated per-iteration:
  #"EstInYounger","EstInOlder","EstInPooled",
  #"TrueInYounger","TrueInOlder","TrueInPooled")

  #Also record true strength of confounding, so copy the template for truths
  confoundingStrengths <- truths

  # Truth 1) What is the true causal effect in a large data set?
  for(dagName in names(simulators)){
    bigData <- simulators[[dagName]]$simulate(instructions$truth$n)
    bigData_v0 <- simulators[[dagName]]$simulate(instructions$truth$n,
                                                 interventions=list(V=0),
                                                 uMx=bigData$u)
    bigData_v1 <- simulators[[dagName]]$simulate(instructions$truth$n,
                                                 interventions=list(V=1),
                                                 uMx=bigData$u)
    truth <- mean(bigData_v1$d$Y) - mean(bigData_v0$d$Y)
    indices <- which(startsWith(colnames(truths),dagName))
    truths[1,indices] <- truth

    #Calculate true amount of confounding (linear dgs assumed)
    ## NB: This will need modification for different true DGSs
    if("C" %in% colnames(bigData$d)){
      mod_adj <- lm(Y~C+V,bigData$d)
      mod_unadj <- lm(Y~V,bigData$d)
      confoundingStrengths[1,indices] <- mod_unadj$coefficients[["V"]] - mod_adj$coefficients[["V"]]
    }
  }
  truths
  confoundingStrengths

  #Function to do an iteration of simulation across all simulation specs
  doIteration <- function(iter,returnData=FALSE,runAnalyses=TRUE){
    #Make a place to hold the results for this iteration
    results <- list(
      estimates = estimates[iter,],
      simFailures = simFailures[iter,],
      ns = ns[iter,]
    )

    if(returnData==TRUE){
      results$combinedData<-list()
      results$measuredData<-list()
    }
    for(simSpecIndex in 1:nrow(simSpecs)){
      simulatorName <- simSpecs[simSpecIndex,"dag"]
      measurementName <- simSpecs[simSpecIndex,"measurement"]
      combiningName <- simSpecs[simSpecIndex,"combining"]

      simulator <- simulators[[simulatorName]] #fn for how data are generated
      measuredData <- measFns[[measurementName]](simulator) #Creating the two cohorts
      combinedData <- combFns[[combiningName]](measuredData$cohorts) #"matching" step

      if(returnData==TRUE){
        results$combinedData[[simSpecIndex]]<-combinedData
        results$measuredData[[simSpecIndex]]<-measuredData
      }

      #Run analyses
      if(runAnalyses){
        qualAndAnalysisList <- estCols %>%
          right_join(simSpecs[simSpecIndex,]) %>%
          dplyr::select(analysis,qualFn,caliper)

        qualList <- qualAndAnalysisList %>% dplyr::select(qualFn) %>% unique()

        for(matchQualFnIndex in 1:nrow(qualList)){
          matchQualFnName <- qualList[matchQualFnIndex,"qualFn"]

          #Calculate match quality
          matchQual <- qualFns[[matchQualFnName]]$calculateMetrics(
            combinedData$bestMatches,
            measuredData
          )

          analysisList <- qualAndAnalysisList %>%
            filter(qualFn==matchQualFnName) %>%
            dplyr::select(analysis,caliper)

          msg$print(paste0("Iter #",iter,": simulator='",simulatorName,
                           "', measurement='",measurementName,
                           "', combining='",combiningName,
                           "', matchQualFn='",matchQualFnName))

          #Calculate match quality
          qualMetric <- instructions$matchQuality[[matchQualFnName]]$options$qualMetric

          for(analysisIndex in 1:nrow(analysisList)){
            tryCatch({
              #Do analyses
              analysisName <- analysisList[analysisIndex,"analysis"]
              caliper <- analysisList[analysisIndex,"caliper"]

              #Apply caliper to filter data set to matches within the caliper
              analysis_IDs <- qualFns[[matchQualFnName]]$filterPoorQual(matchQual,
                                                                      caliper,
                                                                      qualMetric) %>%
                dplyr::select(younger_ID, older_ID)
              msg$print(paste0("InstructionSet='",instructionsName,
                               "', iter=",iter,
                               " Analysis='",analysisName,
                               "', caliper=",caliper,
                               " n=",nrow(analysis_IDs)))

              analysis_data <- analysis_IDs %>%
                left_join(combinedData$d)

              #Run analyses
              result <- analysisFns[[analysisName]](analysis_data)
              estColName <- paste0(c(simSpecs[simSpecIndex,],analysisName,matchQualFnName,format(caliper,scientific=TRUE)),collapse="_")
              results$estimates[1,estColName] <- result$est
              msg$print(result$est)
              results$simFailures[1,estColName] <- FALSE
              results$ns[1,estColName] <- nrow(analysis_IDs)
            },
            error=function(e){
              msg$print(e)
              estColName <- paste0(c(simSpecs[simSpecIndex,],analysisName,matchQualFnName,format(caliper,scientific=TRUE)),collapse="_")
              results$simFailures[1,estColName] <<- TRUE
            })
          }
        }
      }
    }

    return(results)
  }

  #Do a trial run prior to starting the simulation iterations
  ## to evaluate whether matching caliper sizes need to be adjusted
  #temp
  qualFns <- loadMatchQualityFns(instructions)
  #\end temp
  iterResults <- doIteration(1,returnData=TRUE,runAnalyses=FALSE)

  for(matchQualFnName in names(qualFns)){
    for(specIndex in 1:length(iterResults$combinedData)){
      matchQual <- qualFns[[matchQualFnName]]$calculateMetrics(
        iterResults$combinedData[[specIndex]]$bestMatches,
        iterResults$measuredData[[specIndex]]
      )
      qualQuants <- apply(matchQual,2,quantile,c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))

      testCalipers <- instructions$matchQuality[[matchQualFnName]]$options$testCalipers
      qualMetric <-  instructions$matchQuality[[matchQualFnName]]$options$qualMetric

      qualSummary <- data.frame(Caliper=testCalipers,
                                n = rep(NA,length(testCalipers)),
                                maxQual=rep(NA,length(testCalipers)),
                                medQual=rep(NA,length(testCalipers)),
                                meanQual=rep(NA,length(testCalipers)))
      caliperIndex <- 1
      for(testCaliper in testCalipers){
        d_temp <- qualFns[[matchQualFnName]]$filterPoorQual(matchQual,
                                                            testCaliper,
                                                            qualMetric)
        qualSummary[caliperIndex,"n"] <- nrow(d_temp)
        qualSummary[caliperIndex,"maxQual"] <- max(d_temp$quality)
        qualSummary[caliperIndex,"medQual"] <- median(d_temp$quality)
        qualSummary[caliperIndex,"meanQual"] <- mean(d_temp$quality)

        caliperIndex <- caliperIndex + 1
      }
    }
  }

  #Simulation Iteration Loop
  for(iter in 1:options$nIter){
    iterResults <- doIteration(iter)
    estimates[iter,] <- iterResults$estimates
    simFailures[iter,] <- iterResults$simFailures
    ns[iter,] <- iterResults$ns
  }
  saveRDS(estimates,file.path(dirs$Results,"estimates.rds"))
  saveRDS(simFailures,file.path(dirs$Results,"simFailures.rds"))
  saveRDS(ns,file.path(dirs$Results,"ns.rds"))

  #Calcuate MSE
  estSummary <- mseFn(estimates,truths[1,],simFailures, ns)
  estSummary <- as.data.frame(t(estSummary))
  estSummary$key <- rownames(estSummary)
  rownames(estSummary) <- NULL
  estCols2 <- estCols
  estCols2$key <- apply(estCols2,1,paste0,collapse="_")
  estSummary <- left_join(estCols2,estSummary)
  rownames(estSummary) <- estSummary$key
  write.csv(estSummary %>% arrange(dag,measurement,analysis,qualFn,-caliper),
            file.path(dirs$Results,"estimate_performance.csv"))
  View(estSummary %>% arrange(dag,measurement,analysis,qualFn,-caliper))
}
