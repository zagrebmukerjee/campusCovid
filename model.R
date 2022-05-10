# Core function that runs the model logic
# takes a set of parameters. Returns a history
modelRunner <- function(paramsArg, startingStates, variant = nullVariant, infectionEvent = nullInfectionEvent){
  
  # initial conditiions
  startingStates$susceptible <-  startingStates$susceptible - paramsArg$startingInfections
  startingStates$infected <- startingStates$infected +  paramsArg$startingInfections
  
  history <- startingStates
  states <- startingStates
  stateNames <- colnames(startingStates)
  
  paramsArg$rho <- 1/paramsArg$recoveryTime #recovery rate
  paramsArg$theta <- 1/paramsArg$incubationTime #incubation rate
  paramsArg$sigma <- 1/paramsArg$immunityTime # rate of loss of post-infection immunity
  paramsArg$detectionRate <- 1/paramsArg$testingCadence 
  paramsArg$beta <- paramsArg$r0/paramsArg$recoveryTime # if each infection
  
  
  for (i in 1:nCycles){
    
    currentParameters <- paramsArg
    
    # change the parameters according to variant emergence
    if(i > variant$emergenceStart){
      # variant taking over
      vShare <- min((i - variant$emergenceStart)/
                      variant$emergenceTime, 1)
      currentParameters$r0 <- vShare*variant$r0 + (1-vShare)*paramsArg$r0
      currentParameters$externalInfections <- vShare*variant$externalInfections +
        (1-vShare)*paramsArg$externalInfections
    }
    
    if(i == infectionEvent$time){
      currentParameters$externalInfections <- currentParameters$externalInfections + 
        infectionEvent$externalInfections
    }
    
    changes <- lapply(1:length(states), function(s){
      eval(parse(text = transitionEqList[[s]]))
    }) %>% unlist()
    
    states <- states + changes
    history <- bind_rows(history, states)
  }
  
  history$cycle <- 1:nrow(history)
  history
}

