
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

