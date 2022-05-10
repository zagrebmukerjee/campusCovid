
library(dplyr)
library(tidyr)
library(ggplot2)

# For now, define parameters here

nCycles <- 300


nullVariant <- list(
  emergenceStart = 999, #round(1*nCycles/2), #which cycle it starts
  emergenceTime = 21, # how many cycles until it "takes over"
  r0 = 4.0,
  externalInfections = 4
)

# infection event. Example: spring break. 
nullInfectionEvent <- list(
  time = 999,#round(nCycles*1/3), #which cycle it happens
  externalInfections = 0# baseParameters$population*.2 #how many infections
)

