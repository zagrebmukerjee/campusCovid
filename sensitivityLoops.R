source("preamble.R")
source("model.R")



# Set up loops
########################################

r0StartInfScenariosList <- apply(
  X = expand.grid(r0 = seq(1,4,.1), startInf = seq(0,60,5)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$parameters$testingCadence <- 5
    p$parameters$r0 <- i["r0"]
    p$parameters$startingInfections <- i["startInf"]
    p
  })


resultsR0StartInf <- lapply(
  r0StartInfScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioR0 <-s$parameters$r0
    h$scenarioStartInf <- s$parameters$startingInfections
    h
  })

resultsR0StartInfDF <- bind_rows(resultsR0StartInf) %>% 
  pivot_longer(cols = -c(cycle, scenarioR0, scenarioStartInf ))


r0ExtInfScenariosList <- apply(
  X = expand.grid(r0 = seq(1,4,.1), extInf = seq(0,20,.5)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$parameters$r0 <- i["r0"]
    p$parameters$externalInfections <- i["extInf"]
    p
  })

resultsR0ExtInf <- lapply(
  r0ExtInfScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioR0 <- s$parameters$r0
    h$scenarioExtInf <- s$parameters$externalInfections
    h
  })

resultsR0ExtInfDF <- bind_rows(resultsR0ExtInf) %>% 
  pivot_longer(cols = -c(cycle, scenarioR0, scenarioExtInf ))



extInfTestingScenariosList <- apply(
  X = expand.grid(
    extInf = seq(0,20,.5),
    cadence = seq(1,14,1)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$parameters$externalInfections <- i["extInf"]
    p$parameters$testingCadence <- i["cadence"]
    p
  })

resultsExtInfTesting <- lapply(
  extInfTestingScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioCadence <- s$parameters$testingCadence
    h$scenarioExtInf <- s$parameters$externalInfections
    h
  })


resultsExtInfTestingDF <- bind_rows(resultsExtInfTesting) %>% 
  pivot_longer(cols = -c(cycle, scenarioExtInf, scenarioCadence ))

# ggplots
#######################

pRS <- ggplot(
  resultsR0StartInfDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed","externalInfections")),
      scenarioR0 %in% seq(1,4,1),
      scenarioStartInf %in% seq(0,60,20)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line() +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("R0: ",sprintf("%02.0f",scenarioR0)),
      ~paste0("SI: ",sprintf("%02.0f",scenarioStartInf))
    )) +
  ggtitle("Effects of increasing starting infections")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pRS)

ggsave(filename = "StartInfLoops.png",plot = pRS, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)


pRE <- ggplot(
  resultsR0ExtInfDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed", "externalInfections")),
      scenarioR0 %in% seq(1,4,1),
      scenarioExtInf %in% seq(1,10,3)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line() +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("R0: ",sprintf(fmt = "%02.0f", scenarioR0)),
      ~paste0("EI: ",sprintf(fmt = "%02.0f", scenarioExtInf))
    )) +
  ggtitle("R0 and External Infection Daily Rate") +
  theme_classic()+
  theme(legend.position = "bottom")

print(pRE)

ggsave(filename = "ExtInfLoops.png",plot = pRE,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )


pET <- ggplot(
  resultsExtInfTestingDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed", "externalInfections")),
      scenarioExtInf %in% seq(1,10,3),
      scenarioCadence %in% (c(3,7,14))
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line() +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("TC: ",sprintf(fmt = "%02.0f", scenarioCadence)),
      ~paste0("EI: ",sprintf(fmt = "%02.0f", scenarioExtInf))
    )) +
  ggtitle("Testing Cadence and External Infections") +
  theme_classic()+
  theme(legend.position = "bottom")

print(pET)

ggsave(filename = "ExtInfTestLoops.png",plot = pET, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)

# isoquants
#######################

isoqR0ExtInfDF <- resultsR0ExtInfDF %>%  
  group_by(name, scenarioR0, scenarioExtInf) %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  )


pContourRE <- ggplot(
  isoqR0ExtInfDF %>%  filter(name == "infected") %>% 
    filter(scenarioExtInf < 10), 
  aes(x = scenarioR0, y = scenarioExtInf, z = avgInf)) +
  geom_contour() +
  theme_classic() +
  xlab("Scenario R0") +
  ylab("External Infection Rate") +
  ggtitle("Isoquants of Average Infected")

print(pContourRE)


ggsave(filename = "ExtInfContour.png",plot = pContourRE,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )



isoqExtInfTestingDF <- resultsExtInfTestingDF %>%  
  group_by(name, scenarioCadence, scenarioExtInf) %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  )


pContourET <- ggplot(
  isoqExtInfTestingDF %>%  filter(name == "infected") %>% 
    filter(scenarioExtInf < 10), 
  aes(x = scenarioCadence, y = scenarioExtInf, z = avgInf)) +
  geom_contour() +
  theme_classic() +
  xlab("Testing Cadence (days)") +
  ylab("External Infection Rate") +
  ggtitle("Isoquants of Average Infected")

print(pContourET)

ggsave(filename = "ExtInfTestContour.png",plot = pContourET,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )

