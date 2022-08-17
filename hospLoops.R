source("preamble.R")
source("modelHosp.R")
source("dimensionalComparison.R")
source("utilities.R")


# Set up loops
########################################

# scenarios with different 
vaccTestingScenariosList <- apply(
  X = expand.grid(vaccinationRate = seq(.25,.9,.05),
                  testingCadence = seq(0,20,.5)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$parameters$vaccinationRate <- i["vaccinationRate"]
    p$parameters$testingCadence <- i["testingCadence"]
    p
})


resultsVaccTesting <- lapply(
  vaccTestingScenariosList,
  function(s){
    h <- modelRunnerHosp(s)
    h$scenarioCadence <- s$parameters$testingCadence
    h$scenarioVaccination <- s$parameters$vaccinationRate
    h
  })


resultsVaccTestingDF <- bind_rows(resultsVaccTesting) %>% 
  pivot_longer(cols = -c(cycle, scenarioVaccination, scenarioCadence ))


# hospitalization
pVaccTesting <- ggplot(
  resultsVaccTestingDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed","isolated", "recovered")),
      scenarioCadence %in% c(3,7,14),
      scenarioVaccination %in% seq(.3, .5, .9)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line(alpha = .5) +
  geom_smooth(formula = y~x, method = "loess", se = F, size = .5) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("Vax: ",sprintf("%02.0f",scenarioVaccination)),
      ~paste0("TC: ",sprintf("%02.0f",scenarioCadence))
    )) +
  ggtitle("Varying period/amplitude of external infections")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pVaccTesting)

ggsave(filename = "vaccTesting.png",plot = pVaccTesting, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)





testingVaccsComparison <- dimCompare(
  dim1 = "parameters$testingCadence",
  dim1Range = seq(1,14,1), dim1ChartingRange = c(3,7,14),
  dim2 = "parameters$vaccinationRate", 
  dim2Range = seq(0,1,.25), dim2ChartingRange = seq(0,1,.25),
  chartingCols = c("infected","isolated", "recovered"),
  smoothing = F, facetName1 = "R0", facetName2 = "EI",
  isoquant = T, isoVar = "hospitalized", hospitalized = T
)

plotSaveFun(extInfR0$isoquant +
              ggtitle("Isoquants of Average Recovered"), "extInfR0Iso")
