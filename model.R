
library(dplyr)
library(tidyr)
library(ggplot2)

# For now, define parameters here

nCycles <- 300

baseParameters <- list(
  r0 = 2.5,
  incubationTime = 3*3,
  recoveryTime = 14*3,
  immunityTime = 14*3,
  externalInfections = 1,
  startingInfections = 10,
  testingCadence = 9,
  population= 1000
)

baseParameters$rho <- 1/baseParameters$recoveryTime #recovery rate
baseParameters$theta <- 1/baseParameters$incubationTime #incubation rate
baseParameters$sigma <- 1/baseParameters$immunityTime # rate of loss of post-infection immunity
baseParameters$detectionRate <- 1/baseParameters$testingCadence 
baseParameters$beta <- baseParameters$r0/baseParameters$recoveryTime # if each infection

# set up variant emergence. For now just 1 
# a variant has an emergence time,  a growth rate, and
# differences in disease properties. Once the variant emerges,
# parameters will converge towards variant parameters at some speed
variant <- list(
    emergenceStart = round(1*nCycles/2), #which cycle it starts
    emergenceTime = 21, # how many cycles until it "takes over"
    r0 = 4.0,
    externalInfections = 4
)

# infection event. Example: spring break. 
infectionEvent <- list(
  time = round(nCycles*1/3), #which cycle it happens
  externalInfections = baseParameters$population*.2 #how many infections
)

# initial conditiions
startingStates <- data.frame(
  susceptible = baseParameters$population - baseParameters$startingInfections,
  exposed = 0,
  infected = baseParameters$startingInfections,
  isolated = 0,
  recovered = 0
)
stateNames <- colnames(startingStates)


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

# Core loop that runs the model logic
for (i in 1:nCycles){
  
  currentParameters <- baseParameters
  
  # change the parameters according to variant emergence
  if(i > variant$emergenceStart){
    # variant taking over
    vShare <- min((i - variant$emergenceStart)/
      variant$emergenceTime, 1)
    currentParameters$r0 <- vShare*variant$r0 + (1-vShare)*baseParameters$r0
    currentParameters$externalInfections <- vShare*variant$externalInfections +
      (1-vShare)*baseParameters$externalInfections
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

p <- ggplot(history) +
  geom_line(aes(x = cycle, y = susceptible, color = "Susceptible"))+ 
  geom_line(aes(x = cycle, y = infected, color = "Infected")) +
  geom_line(aes(x = cycle, y = isolated, color = "Isolated")) +
  geom_line(aes(x = cycle, y = recovered, color = "Recovered")) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  theme_classic()

print(p)
