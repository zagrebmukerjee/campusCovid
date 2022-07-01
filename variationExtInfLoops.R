source("preamble.R")
source("model.R")



# Set up loops
########################################

extInfVarScenariosList <- apply(
  X = expand.grid(period = seq(5,60,5),
                  amplitude = seq(0,20,.5)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$externalInfectionVariation$period <- i["period"]
    p$externalInfectionVariation$amplitude <- i["amplitude"]
    p
  })


resultsExtInfVar <- lapply(
  extInfVarScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioPeriod <- s$externalInfectionVariation$period
    h$scenarioAmplitude <- s$externalInfectionVariation$amplitude
    h
  })


resultsExtInfVarDF <- bind_rows(resultsExtInfVar) %>% 
  pivot_longer(cols = -c(cycle, scenarioPeriod, scenarioAmplitude ))


extInfVarTestingScenariosList <- apply(
  X = expand.grid(
    amplitude = seq(0,20,.5),
    cadence = seq(1,14,1)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$externalInfectionVariation$period <- 20
    p$externalInfectionVariation$amplitude <- i["amplitude"]
    p$parameters$testingCadence <- i["cadence"]
    p
  })


resultsExtInfVarTesting <- lapply(
  extInfVarTestingScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioCadence <- s$parameters$testingCadence
    h$scenarioAmplitude <- s$externalInfectionVariation$amplitude
    h
  })


resultsExtInfVarTestingDF <- bind_rows(resultsExtInfVarTesting) %>% 
  pivot_longer(cols = -c(cycle, scenarioAmplitude, scenarioCadence ))



extInfVarTestingACScenariosList <- apply(
  X = expand.grid(
    amplitude = seq(0,20,.5),
    cadence = seq(1,14,1)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$externalInfectionVariation$period <- 20
    p$externalInfectionVariation$amplitude <- i["amplitude"]
    p$externalInfectionVariation$varFun <- function(cyc, pd, amp){
      amp*sin(2*pi*1/pd*cyc)+(cyc/15)
    }
    p$parameters$testingCadence <- i["cadence"]
    p
  })


resultsExtInfVarTestingAC <- lapply(
  extInfVarTestingACScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioCadence <- s$parameters$testingCadence
    h$scenarioAmplitude <- s$externalInfectionVariation$amplitude
    h
  })


resultsExtInfVarTestingACDF <- bind_rows(resultsExtInfVarTestingAC) %>% 
  pivot_longer(cols = -c(cycle, scenarioAmplitude, scenarioCadence ))


# ggplots
#######################

pExtInfVar <- ggplot(
  resultsExtInfVarDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed","isolated")),
      scenarioPeriod %in% (seq(15,60,15)),
      scenarioAmplitude %in% seq(2,20,6)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line(alpha = .5) +
  geom_smooth(formula = y~x, method = "loess", se = F, size = .5) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("Pd: ",sprintf("%02.0f",scenarioPeriod)),
      ~paste0("Amp: ",sprintf("%02.0f",scenarioAmplitude))
    )) +
  ggtitle("Varying period/amplitude of external infections")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pExtInfVar)

ggsave(filename = "ExtInfVar.png",plot = pExtInfVar, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)


pExtInfVarTesting <- ggplot(
  resultsExtInfVarTestingDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed","isolated")),
      scenarioCadence %in% (c(3,7,14)),
      scenarioAmplitude %in% seq(2,20,6)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line(alpha = .5) +
  geom_smooth(formula = y~x, method = "loess", se = F, size = .5) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(
      ~paste0("TC: ",sprintf("%02.0f",scenarioCadence)),
      ~paste0("A: ",sprintf("%02.0f",scenarioAmplitude))
    )) +
  ggtitle("Testing and Increasing Variation in External Infection")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pExtInfVarTesting)

ggsave(filename = "extInfVarTesting.png",plot = pExtInfVarTesting, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)



pExtInfVarTestingAC <- ggplot(
  resultsExtInfVarTestingACDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed","isolated")),
      scenarioCadence %in% (c(3,7,14)),
      scenarioAmplitude %in% seq(2,20,6)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line(alpha = .5) +
  geom_smooth(formula = y~x, method = "loess", se = F, size = .5) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(
      ~paste0("TC: ",sprintf("%02.0f",scenarioCadence)),
      ~paste0("A: ",sprintf("%02.0f",scenarioAmplitude))
    )) +
  ggtitle("Testing and Autocorrelated Variation in External Infection")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pExtInfVarTestingAC)

ggsave(filename = "extInfVarTestingAC.png",plot = pExtInfVarTestingAC, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)




