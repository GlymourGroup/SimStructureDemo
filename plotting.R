
#==============================================
#Plot
#==============================================
estSummary_probMatch <- estSummary %>% filter(combining=="probMatch")
write.csv(estSummary_probMatch,
          file.path(dirs$Results,"estimate_performance_probMatch.csv"))

plotData <- estSummary_probMatch

make_bias_plot <- function(d){
  ggplot(d,
         aes(x=bias,y=key)) +
    # plot the point estimate
    geom_point() +
    # plot the confidence intervals
    geom_errorbarh(aes(xmin = bias_lb, xmax = bias_ub)) +
    geom_vline(aes(xintercept = 0)) +
    ylab("dag/matching/analysis combination") +
    xlab("")#+
  #theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
}

g <- list()
g$bias_all <- make_bias_plot(plotData)
for(dagName in names(dags)){
  dagResults <- plotData %>% filter(dag==!!dagName)
  g[[paste0("bias_",dagName)]] <- make_bias_plot(dagResults)
}
for(graphName in names(g)){
  ggsave(g[[graphName]],
         file=file.path(dirs$Results,paste0(graphName,".png")),
         device="png")
}

#==============================================
# Latex Table Ouptuts
#==============================================
