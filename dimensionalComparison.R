
dimCompare <- function(dim1, dim1Range, dim1ChartingRange = dim1Range,
                       dim2, dim2Range, dim2ChartingRange = dim2Range,
                       otherChanges = c(),otherChangesVals = c(),
                       chartingCols = c("infected", "recovered"),
                       smoothing = F, facetName1 = "", facetName2 = "",
                       isoquant = F, isoVar = "infected",
                       hospitalized = F){
  
  
  # dim1 <- "parameters$testingCadence"
  # dim1Range <- seq(1,14,1)
  # dim1ChartingRange <- c(3,7,14)
  # dim2 <- "externalInfectionVariation$amplitude"
  # dim2Range <- seq(0,20,.5)
  # dim2ChartingRange <- seq(2,20,6)
  # otherChanges <- c("externalInfectionVariation$period",
  #                   "parameters$startingInfections")
  # otherChangesVals <- c(20,9)
  # chartingCols <- c("infected", "recovered", "externalInfections")
  # smoothing <- T
  # facetName1 <- "TC"
  # facetName1 <- "A"
  # isoquant <- T
  # isoVar <- "infected"
  
  baseline <- baseScenario
  modelFun <- if(hospitalized){modelRunnerHosp}else{modelRunner}
  
  d1 <- strsplit(dim1, "\\$") %>%  unlist()
  d2 <- strsplit(dim2, "\\$") %>%  unlist()
  
  if(length(otherChanges) > 0){
    otherChangesSplit <- lapply(otherChanges, function(a){
      strsplit(a, "\\$") %>%  unlist()
    })
    
    for(a in 1:length(otherChanges)){
      baseline[[otherChangesSplit[[a]][1]]][[otherChangesSplit
                                             [[a]][2]]] <- otherChangesVals[a]
    }
    
    
  }
  
  scenarioList <- apply(
    X = expand.grid(dim1Range, dim2Range),
    MARGIN = 1,
    FUN = function(i){
      p <- baseline
      p[[d1[1]]][[d1[2]]] <- i[["Var1"]]
      p[[d2[1]]][[d2[2]]] <- i[["Var2"]]
      p
    })
  
  results <- lapply(
    scenarioList,
    function(s){
      h <- modelRunner(s)
      h$scenarioVar1 <- s[[d1[1]]][[d1[2]]]
      h$scenarioVar2 <- s[[d2[1]]][[d2[2]]]
      h
    })
  
  
  resultsDF <- bind_rows(results) %>% 
    pivot_longer(cols = -c(cycle, scenarioVar1, scenarioVar2 ))
  
  p <- ggplot(
    resultsDF  %>% 
      filter(
        name %in% chartingCols,
        scenarioVar1 %in% dim1ChartingRange,
        scenarioVar2 %in% dim2ChartingRange
      ),
    aes(x = cycle, y = value, color = name)) +
    geom_line(alpha = ifelse(smoothing,.5,1))
  
  if(smoothing){
    p <- p + geom_smooth(formula = y~x, method = "loess", se = F, size = .5)
  }
  
  p <- p + scale_color_discrete(name = "Status") +
    ylab("Compartment population") +
    xlab("8-hour cycles") + 
    facet_grid(
      c(~paste0(facetName1, ": ", sprintf("%02.0f",scenarioVar1)),
        ~paste0(facetName2, ": ", sprintf("%02.0f",scenarioVar2))
      )) +
    theme_classic()+
    theme(legend.position = "bottom")
  
  isoP <- if(isoquant){
    
    isoDF <- resultsDF %>%  
      group_by(name, scenarioVar1, scenarioVar2) %>% 
      summarize(
        mean = mean(value) %>%  round(2),
        max = max(value)%>%  round(2)
      )
    
    ggplot(
      isoDF %>%  filter(name == isoVar),
      aes(x = scenarioVar1, y = scenarioVar2, z = mean)) +
      geom_contour() +
      theme_classic() +
      xlab(stringr::str_to_title(d1[[2]])) +
      ylab(stringr::str_to_title(d2[[2]])) +
      ggtitle(paste0("Isoquants of Average ", stringr::str_to_title(isoVar)))
    
  } else{element_blank()}
  
  return(list(gridChart = p, isoquant = isoP))
  
}



