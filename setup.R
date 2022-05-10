
# this is a CSV where you can define transition equations
transitionsRaw <- read.csv("stateTransitionEqns.csv", fileEncoding="UTF-8-BOM") 

# this block of code takes the transition equations from the CSV and 
# prepares them for use
transitionEqList <- lapply(
  1:length(startingStates), function(a){
    stNm <- stateNames[a]
    inflowEq <- transitionsRaw %>% 
      filter(targetCompartment == stNm) %>% 
      select(-sourceCompartment, -targetCompartment) %>% 
      mutate(equation = paste0("(", equation, ")")) %>% 
      summarize(equation = paste(equation, collapse = " + ")) %>% 
      unlist()
    
    outflowEq <- transitionsRaw %>%
      filter(sourceCompartment == stNm) %>% 
      select(-sourceCompartment, -targetCompartment) %>% 
      mutate(equation = paste0("-(", equation, ")")) %>% 
      summarize(equation = paste(equation, collapse = " + ")) %>% 
      unlist()
    
    totalFlowEq <- paste(inflowEq, outflowEq)
    
  }
) %>%  unlist()  

names(transitionEqList) <- stateNames
