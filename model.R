# Core function that runs the model logic
# takes a set of parameters. Returns a history
modelRunner <- function(scenario = baseScenario){
  
  # different aspects of scenario
  paramsArg <- scenario$parameters
  startingStates <- scenario$startingStates
  stateNames <- scenario$stateNames
  variant <- scenario$variant
  infectionEvent <- scenario$infectionEvent
  externalInfectionVariation <- scenario$externalInfectionVariation
  # initial conditions
  startingStates$susceptible <-  startingStates$susceptible - paramsArg$startingInfections
  startingStates$infected <- startingStates$infected +  paramsArg$startingInfections
  nStates <- length(startingStates)
  
  paramsArg$rho <- 1/(paramsArg$recoveryTime*cyclesPerDay) #recovery rate
  paramsArg$theta <- 1/(paramsArg$incubationTime*cyclesPerDay) #incubation rate
  paramsArg$sigma <- 1/(paramsArg$immunityTime*cyclesPerDay) # rate of loss of post-infection immunity
  paramsArg$detectionRate <- 1/(paramsArg$testingCadence *cyclesPerDay)
  paramsArg$beta <- paramsArg$r0/(paramsArg$recoveryTime*cyclesPerDay) # if each infection causes r0 more, it has n days to do so
  paramsArg$externalInfections <- paramsArg$externalInfections/cyclesPerDay
  
  # what are we tracking
  history <- as.matrix(startingStates) %>% 
    rbind(matrix(rep(NA, nCycles*nStates),ncol = nStates))
  states <- history[1,]
  extInf <- c(paramsArg$externalInfections, rep(NA, nCycles))
  
  
  
  for (i in 1:nCycles){
    
    currentParameters <- paramsArg
    
    # change the parameters according to variant emergence
    if(i > variant$emergenceStart*cyclesPerDay){
      # variant taking over
      vShare <- min((i - variant$emergenceStart)/
                      (variant$emergenceTime*cyclesPerDay), 1)
      currentParameters$r0 <- vShare*variant$r0 + (1-vShare)*paramsArg$r0
      currentParameters$externalInfections <- vShare*variant$externalInfections +
        (1-vShare)*currentParameters$externalInfections
    }
    
    # change the parameter for superspreader events
    if(i == infectionEvent$time*cyclesPerDay){
      currentParameters$externalInfections <- currentParameters$externalInfections + 
        infectionEvent$externalInfections
    }
    
    # change the parameters for variation in external infection
    currentParameters$externalInfections <- currentParameters$externalInfections + 
      externalInfectionVariation$varFun(
        i, externalInfectionVariation$period*cyclesPerDay, externalInfectionVariation$amplitude
      )
    currentParameters$externalInfections <- max(currentParameters$externalInfections,0)
    
    changes <- lapply(1:length(states), function(s){
      eval(transitionEqList[[s]])
    }) %>% unlist()
    
    states <- states + changes
    history[i+1,] <- states
    extInf[i+1] <- currentParameters$externalInfections
    
  }
  
  history <- as.data.frame(history)
  history$cycle <- 1:nrow(history)
  history$externalInfections <- extInf
  history 
}

