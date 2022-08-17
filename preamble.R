# package loads

library(dplyr)
library(tidyr)
library(ggplot2)

# Define defaults for each aspect of a scenario
semesterLength <- 100 #days
cyclesPerDay <- 3
nCycles <- semesterLength*cyclesPerDay


nullVariant <- list(
  emergenceStart = 999, #round(1*nCycles/2), #which cycle it starts
  emergenceTime = 7, # how many days until it "takes over"
  r0 = 4.0,
  externalInfections = 10 #daily
)

# infection event. Example: spring break. 
nullInfectionEvent <- list(
  time = 999, #which cycle it happens
  externalInfections = 0#  #how many infections
)

nullExternalInfectionVariation <- list(
  period = 7, 
  amplitude = 0,
  varFun = function(cyc, pd, amp){
    amp*sin(2*pi*1/pd*cyc)
  }
)

baseScenario <- list()

# All parameters are in days/daily
baseScenario$parameters <- list(
  r0 = 2.5,
  incubationTime = 3,
  recoveryTime = 14,
  immunityTime = 14,
  hospitalizationRate = .1, # Cumulatively, X proportion of cases end up hospitalized
  vaccinationRate = .5, #this proportion are vaccinated
  vaccinationHospEffect = .5, # factor by which vaccines REDUCE rate of hospitalization
  externalInfections = 3,
  startingInfections = 10,
  testingCadence = 7, #entire population is tested every n days
  population= 1000
)

#TODO: refactor this whole thing? THe structure is immutable. 
baseScenario$startingStates <- data.frame(
  susceptible = baseScenario$parameters$population,
  exposed = 0,
  infected = 0,
  hospitalized = 0,
  isolated = 0,
  recovered = 0
)

baseScenario$stateNames <- colnames(baseScenario$startingStates)

baseScenario$variant <- nullVariant
baseScenario$infectionEvent <- nullInfectionEvent
baseScenario$externalInfectionVariation <- nullExternalInfectionVariation
# this is a CSV where you can define transition equations
transitionsRaw <- read.csv("stateTransitionEqns.csv", fileEncoding="UTF-8-BOM") 

# this block of code takes the transition equations from the CSV and 
# prepares them for use
transitionEqList <- lapply(
  1:length(baseScenario$startingStates), function(a){
    
    stNm <- baseScenario$stateNames[a]
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
    
    totalFlowEq <- parse(text = paste(inflowEq, outflowEq))
    
  }
)
