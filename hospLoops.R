source("preamble.R")
source("modelHosp.R")
source("dimensionalComparison.R")
source("utilities.R")


# Set up loops
########################################

# scenarios with different 
vaccTestingScenariosList <- apply(
  X = expand.grid(vaccinationRate = seq(.2,.9,.05),
                  testingCadence = seq(1,20,.5)),
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
      !(name %in% c("susceptible", "exposed","isolated", "recovered", "infected", "externalInfections")),
      scenarioCadence %in% c(3,7,14),
      scenarioVaccination %in% c(.3, .5, .9)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line() +
  # geom_smooth(formula = y~x, method = "loess", se = F, size = .5) +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("Vax: ",sprintf("%.2f",scenarioVaccination)),
      ~paste0("TC: ",sprintf("%02.0f",scenarioCadence))
    )) +
  ggtitle("Vaccination Rates and Testing")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pVaccTesting)




isoqOfHosp <- resultsVaccTestingDF %>%  
  group_by(name, scenarioVaccination, scenarioCadence)  %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  )


contoursOfHospitalized <- ggplot(
  isoqOfHosp %>%  filter(name == "hospitalized") %>% 
    filter(scenarioCadence < 20), 
  aes(x = scenarioCadence, y = scenarioVaccination, z = avgInf)) +
  geom_contour_filled(binwidth = 1) +
  # scale_fill_manual(breaks = c("(2.0,2.2]", "(2.2,2.4]", "(2.4,2.6]"), 
  #                   values = viridisLite::cividis(27)) +
  labs(fill = "Avg Hospitalizations") + 
  theme_classic() +
  xlab("Testing Cadence") +
  ylab("Vaccination Rate") +
  ggtitle("Isoquants of Average Hospitalized") +
  guides(fill = guide_legend(nrow = 12)) + 
  theme(
    legend.title = element_text(size = 8),
    legend.key.size = unit(.075, "npc"))


print(contoursOfHospitalized)


ggsave(filename = "hospitalizationContours.png",plot = contoursOfHospitalized,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )



# a function to draw tiles: one factor level lower.
tmpFun <- function(fctVal){
  s <- levels(fctVal)
  idx <- which(s==fctVal)-1
  s[ifelse(idx > 0, idx, idx+1)]
}

isoqVaccTesting <- resultsVaccTestingDF %>%  
  mutate(testingRate = 1/scenarioCadence) %>%
  group_by(name, scenarioVaccination, scenarioCadence, testingRate)  %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  ) %>% 
  ungroup() %>% 
  filter(avgInf < 150) %>% 
  mutate(infBin = cut(avgInf,breaks = seq(0,3,.1), include.lowest = T, right = F))  %>%
  filter(name == "hospitalized") %>% 
  filter(!is.na(infBin)) %>% 
  mutate(
    xMin = (scenarioCadence-1),
    xMax = scenarioCadence,
    yMax = infBin
  ) %>%
  rowwise() %>%
  mutate(yMin = tmpFun(infBin))

gridResult <- isoqVaccTesting %>%  
  select(scenarioVaccination, scenarioCadence, infBin) %>% 
  pivot_wider(
    names_from = scenarioCadence,
    values_from = scenarioVaccination,
    values_fn =mean)   

interpolatedPlotData <- gridResult %>%  
  pivot_longer(-infBin,
               names_to = "scenarioCadence",
               values_to = "scenarioVaccination") %>% 
  mutate(scenarioCadence = as.numeric(scenarioCadence)) %>% 
  group_by(scenarioCadence) %>% 
  arrange(scenarioCadence, infBin) %>% 
  fill(scenarioVaccination, .direction = "up") %>%
  filter(!is.na(scenarioVaccination)) %>% 
  mutate(
    xMin = (scenarioCadence-1),
    xMax = scenarioCadence,
    yMax = infBin
  ) %>%
  rowwise() %>%
  mutate(yMin = tmpFun(infBin)) 


testingVaccinationContours <- ggplot(
  interpolatedPlotData , 
  aes(x = scenarioCadence, y = infBin, fill = scenarioVaccination)) +
  # geom_tile() +
  geom_rect(aes(xmax = xMax, xmin = xMin, ymax = yMax,ymin = yMin)) +
  scale_fill_distiller(palette="Greens", direction = 1, values = c(0,.1,1)^.75) +
  theme_classic() +
  labs(fill = "Vaccinations")+
  xlab("Testing Cadence") +
  ylab("Hospitalizations") +
  ggtitle("Isoquants of Vaccination Rate") +
  theme(axis.text.y = element_blank())

print(testingVaccinationContours)


ggsave(filename = "VaccinationContours.png",plot = testingVaccinationContours,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )


