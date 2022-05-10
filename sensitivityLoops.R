source("preamble.R")
source("model.R")


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

#TODO: refactor this whole thing. THe structure is immutable. 
startingStates <- data.frame(
  susceptible = baseParameters$population,
  exposed = 0,
  infected = 0,
  isolated = 0,
  recovered = 0
)
stateNames <- colnames(startingStates)
r0ScenariosList <- lapply(seq(1,4,1), function(i){
  p <- baseParameters
  p$r0 <- i
  p
})

r0StartInfScenariosList <- apply(
  X = expand.grid(r0 = seq(1,4,1), startInf = seq(0,60,20)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseParameters
    p$r0 <- i["r0"]
    p$startingInfections <- i["startInf"]
    p
  })

r0ExtInfScenariosList <- apply(
  X = expand.grid(r0 = seq(1,4,1), extInf = seq(0,20,5)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseParameters
    p$r0 <- i["r0"]
    p$externalInfections <- i["extInf"]
    p
  })


source("setup.R")

resultsR0 <- lapply(
  r0ScenariosList,
  function(s){
    h <- modelRunner(s, startingStates)
    h$scenarioR0 <- paste0("R0: ",s$r0)
    h
  })

resultsR0StartInf <- lapply(
  r0StartInfScenariosList,
  function(s){
    h <- modelRunner(s, startingStates)
    h$scenarioR0 <- paste0("R0: ",s$r0)
    h$scenarioStartInf <- paste0("SI: ", s$startingInfections)
    h
  })


resultsR0ExtInf <- lapply(
  r0ExtInfScenariosList,
  function(s){
    h <- modelRunner(s, startingStates)
    h$scenarioR0 <- paste0("R0: ",s$r0)
    h$scenarioExtInf <- paste0(
      "EI: ", sprintf(fmt = "%02.0f", s$externalInfections))
    h
  })

resultsR0DF <- bind_rows(resultsR0)
resultsR0StartInfDF <- bind_rows(resultsR0StartInf)
resultsR0ExtInfDF <- bind_rows(resultsR0ExtInf)

pRS <- ggplot(resultsR0StartInfDF) +
  # geom_line(aes(x = cycle, y = susceptible, color = "Susceptible"))+ 
  geom_line(aes(x = cycle, y = infected, color = "Infected")) +
  geom_line(aes(x = cycle, y = isolated, color = "Isolated")) +
  geom_line(aes(x = cycle, y = recovered, color = "Recovered")) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(c(~scenarioR0,~scenarioStartInf)) +
  ggtitle("Effects of increasing starting infections")+
  theme_classic()

print(pRS)



pRE <- ggplot(resultsR0ExtInfDF) +
  # geom_line(aes(x = cycle, y = susceptible, color = "Susceptible"))+ 
  geom_line(aes(x = cycle, y = infected, color = "Infected")) +
  geom_line(aes(x = cycle, y = isolated, color = "Isolated")) +
  geom_line(aes(x = cycle, y = recovered, color = "Recovered")) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(c(~scenarioR0,~scenarioExtInf)) +
  theme_classic()


print(pRE)



