
library(dplyr)
library(tidyr)
library(ggplot2)

parameters <- list(
  r0 = 4.0,
  incubationTime = 3*3,
  recoveryTime = 14*3,
  immunityTime = 30*3,
  externalInfections = 0,
  startingInfections = 10,
  testingCadence = 21,
  population= 1000
)

parameters$rho <- 1/parameters$recoveryTime #recovery rate
parameters$theta <- 1/parameters$incubationTime #incubation rate
parameters$sigma <- 1/parameters$immunityTime # rate of loss of post-infection immunity
parameters$detectionRate <- 1/parameters$testingCadence
parameters$beta <- parameters$r0*(parameters$rho)

startingStates <- data.frame(
  susceptible = parameters$population - parameters$startingInfections,
  exposed = 0,
  infected = parameters$startingInfections,
  isolated = 0,
  recovered = 0
)
stateNames <- colnames(startingStates)

nCycles <- 300

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


history <- startingStates
states <- startingStates

for (i in 1:nCycles){
  changes <- lapply(1:length(states), function(s){
    eval(parse(text = transitionEqList[[s]]))
  }) %>% unlist()
  
  states <- states + changes
  history <- bind_rows(history, states)
}
history$cycle <- 1:nrow(history)

p <- ggplot(history) +
  geom_line(aes(x = cycle, y = susceptible, color = "Susceptible"))+ 
  geom_line(aes(x = cycle, y = infected, color = "Infected")) +
  scale_color_discrete(name = "Status") +
  theme_classic()

print(p)
