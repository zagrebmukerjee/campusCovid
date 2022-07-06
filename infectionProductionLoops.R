source("preamble.R")
source("model.R")



# Set up loops
########################################


#######################

R0TestingScenariosList <- apply(
  X = expand.grid(
    r0 = seq(0,20,.25),
    # cadence = c(seq(1,4,.1),seq(4,28,1))),
    cadence = seq(1,28,1)),
  MARGIN = 1,
  FUN = function(i){
    p <- baseScenario
    p$parameters$r0 <- i["r0"]
    p$parameters$testingCadence <- i["cadence"]
    p$parameters$externalInfections <- 2*i["r0"] + 2
    p
  })

R0TestingResults <- lapply(
  R0TestingScenariosList,
  function(s){
    h <- modelRunner(s)
    h$scenarioR0 <-s$parameters$r0
    h$scenarioCadence <- s$parameters$testingCadence
    h
  })


R0TestingResultsDF <- bind_rows(R0TestingResults) %>% 
  pivot_longer(cols = -c(cycle, scenarioR0, scenarioCadence ))


# ggplots
#######################


pRT <- ggplot(
  R0TestingResultsDF  %>% 
    filter(
      !(name %in% c("susceptible", "exposed","externalInfections")),
      scenarioR0 %in% seq(1,10,2),
      scenarioCadence %in% c(1,3,5,7)
    ),
  aes(x = cycle, y = value, color = name)) +
  geom_line() +
  scale_color_discrete(name = "Status") +
  ylab("Compartment population") +
  xlab("8-hour cycles") + 
  facet_grid(
    c(~paste0("R0: ",sprintf("%02.0f",scenarioR0)),
      ~paste0("TC: ",sprintf(fmt = "%02.0f", scenarioCadence))
    )) +
  ggtitle("Impact of Testing Cadence")+
  theme_classic()+
  theme(legend.position = "bottom")

print(pRT)

ggsave(filename = "R0TestLoops.png",plot = pRT, 
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250)

#######################
# isoquants
#######################

isoqR0TestingDF <- R0TestingResultsDF %>%  
  group_by(name, scenarioR0, scenarioCadence)  %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  )


pContourRT <- ggplot(
  isoqR0TestingDF %>%  filter(name == "infected") %>% 
    filter(scenarioCadence < 15, scenarioR0 < 10), 
  aes(x = scenarioCadence, y = scenarioR0, z = avgInf)) +
  geom_contour_filled(binwidth = 10) +
  labs(fill = "Avg Infections") + 
  theme_classic() +
  xlab("Testing Cadence") +
  ylab("Variant Infectivity (R0)") +
  ggtitle("Isoquants of Average Infected") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(nrow = 1))


print(pContourRT)


ggsave(filename = "R0TestingContour.png",plot = pContourRT,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )
#########################


isoqR0Testing2DF <- R0TestingResultsDF %>%  
  mutate(testingRate = 1/scenarioCadence) %>% 
  group_by(name, scenarioR0, testingRate)  %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  )


pContourRT2 <- ggplot(
  isoqR0Testing2DF %>%  filter(name == "infected") %>%
    filter(testingRate > 1/15, scenarioR0 < 10), 
  aes(x = testingRate, y = scenarioR0, z = avgInf)) +
  geom_contour_filled(binwidth = 10) +
  theme_classic() +
  xlab("Testing Rate") +
  ylab("Variant Infectivity (R0)") +
  ggtitle("Isoquants of Average Infected") +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(nrow = 1))

print(pContourRT2)


ggsave(filename = "R0TestingContour2.png",plot = pContourRT,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )

#######################

isoqR0Inf <- R0TestingResultsDF %>%  
  mutate(testingRate = 1/scenarioCadence) %>% 
  group_by(name, scenarioR0, scenarioCadence)  %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  ) %>% 
  ungroup() %>% 
  filter(avgInf < 150) %>% 
  mutate(infBin = cut(avgInf,breaks = seq(0,150,10)))#unique(round(seq(0,10,.5)^(2))) ))


pContourRT3 <- ggplot(
  isoqR0Inf %>%  filter(name == "infected") %>%  filter(scenarioCadence < 25), 
  aes(x = scenarioCadence, y = infBin)) +
  geom_tile(aes(fill = scenarioR0)) +
  scale_fill_distiller(palette="Greens", direction = 1, values = c(0,.1,1)^.75) +
  theme_classic() +
  labs(fill = "R0")+
  xlab("Testing Cadence") +
  ylab("Infections") +
  ggtitle("Isoquants of R0")

print(pContourRT3)


ggsave(filename = "R0TestingContour3.png",plot = pContourRT,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )


#######################

tmpFun <- function(fctVal){
  s <- levels(fctVal)
  idx <- which(s==fctVal)-1
  s[ifelse(idx > 0, idx, idx+1)]
}

isoqR0Inf2 <- R0TestingResultsDF %>%  
  mutate(testingRate = 1/scenarioCadence) %>%
  group_by(name, scenarioR0, scenarioCadence, testingRate)  %>% 
  summarize(
    avgInf = mean(value),
    maxInf = max(value)
  ) %>% 
  ungroup() %>% 
  filter(avgInf < 150) %>% 
  mutate(infBin = cut(avgInf,breaks = seq(0,150,10), include.lowest = T, right = F))  %>%
  filter(name == "infected")  %>% 
  mutate(
    xMin = (1/(scenarioCadence+1)),
    xMax = testingRate, 
    yMax = infBin
  ) %>% 
  rowwise() %>% 
  mutate(yMin = tmpFun(infBin))


pContourRT4 <- ggplot(
  isoqR0Inf2 , 
  aes(x = testingRate, y = infBin, fill = scenarioR0)) +
  geom_rect(aes(xmax = xMax, xmin = xMin, ymax = yMax,ymin = yMin)) +
  scale_fill_distiller(palette="Greens", direction = 1, values = c(0,.1,1)^.75) +
  theme_classic() +
  labs(fill = "R0")+
  xlab("Testing Rate (tests/day)") +
  ylab("Infections") +
  ggtitle("Isoquants of R0")

print(pContourRT4)


ggsave(filename = "TestingRateInfContour",plot = pContourRT4,
       path = "Img/", width = 7, height = 7, units = "in",
       dpi = 250 )

