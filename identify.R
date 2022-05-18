function(effect){
  library("igraph")
  library("causaleffect")
  library("ggplot2")
  library("ggnetwork")

  effect <- effect
  make_igraph <- function(dag){
    dag_df <- data.frame("source"=character(),
                         "dest"=character(),
                         stringsAsFactors=FALSE)
    i <- 1
    for(vName in names(dag)){
      for(pName in dag[[vName]]$parents){
        dag_df[i,] <- c(pName,vName)
        i <- i + 1
      }
    }
    print(dag_df)
    graph_from_edgelist(as.matrix(dag_df))
  }

  identify <- function(dag){
    g <- make_igraph(dag)
    g_plot <- ggplot(ggnetwork(g,arrow.gap=0.05),
                aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
      geom_nodes(color="white",size=10,aes(x, y)) +
      geom_nodetext(aes(label = name )) +
      theme_blank()

    effect <- causal.effect(y=effect$outcomes,
                          x=effect$do,
                          z=effect$cond|c(),
                          G=g,
                          simp=TRUE,steps=TRUE,expr=TRUE)
    return(list(effect=effect,g=g,plot=g_plot))
  }

  return(identify)
}