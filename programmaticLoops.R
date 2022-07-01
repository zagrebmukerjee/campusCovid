
testingCadenceExt <- dimCompare(
  dim1 = "parameters$testingCadence",
  dim1Range = seq(1,14,1), dim1ChartingRange = c(3,7,14),
  dim2 = "parameters$externalInfections", 
  dim2Range = seq(0,10,.5), dim2ChartingRange = seq(1,10,3),
  otherChanges = c("externalInfectionVariation$period"), 
  otherChangesVals = c(20),
  chartingCols = c("infected", "isolated", "recovered"),
  smoothing = F, facetName1 = "TC", facetName2 = "Ex",
  isoquant = T, isoVar = "recovered"
)
plotSaveFun(testingCadenceExt$gridChart +
              ggtitle("Testing Controls External Infections"), "testingExt")
plotSaveFun(testingCadenceExt$isoquant + 
              ggtitle("Isoquants of Avg Recovered: A Little Testing Can Go a Long Way"), "testingExtIso")


testingAmp <- dimCompare(
  dim1 = "parameters$testingCadence",
  dim1Range = seq(1,14,1), dim1ChartingRange = c(3,7,14),
  dim2 = "externalInfectionVariation$amplitude", 
  dim2Range = seq(0,5,.25), dim2ChartingRange = seq(0,5,2.5),
  otherChanges = c("externalInfectionVariation$period",
                   "parameters$externalInfections"), 
  otherChangesVals = c(20,5),
  chartingCols = c("externalInfections", "infected", "recovered"),
  smoothing = T, facetName1 = "TC", facetName2 = "A"
)

plotSaveFun(testingAmp$gridChart+
              ggtitle("Testing Mitigates Effects of Variability"),
            "testingAmp")




extInfR0 <- dimCompare(
  dim1 = "parameters$r0",
  dim1Range = seq(1,4,.1), dim1ChartingRange = seq(1,4,1),
  dim2 = "parameters$externalInfections", 
  dim2Range = seq(0,10,.5), dim2ChartingRange = seq(1,10,3),
  otherChanges = c("parameters$testingCadence"), 
  otherChangesVals = c(5),
  chartingCols = c("infected","isolated", "recovered"),
  smoothing = F, facetName1 = "R0", facetName2 = "EI",
  isoquant = T, isoVar = "recovered"
)
plotSaveFun(extInfR0$isoquant +
              ggtitle("Isoquants of Average Recovered"), "extInfR0Iso")





testingVariant <- dimCompare(
  dim1 = "parameters$testingCadence",
  dim1Range = seq(1,7,1), dim1ChartingRange = c(1, 3,7),
  dim2 = "variant$externalInfections", 
  dim2Range = seq(1,10,1), dim2ChartingRange = c(1,5,10),
  otherChanges = c("variant$emergenceStart"), 
  otherChangesVals = c(50),
  chartingCols = c("infected", "isolated", "recovered"),
  smoothing = F, facetName1 = "TC", facetName2 = "V",
  isoquant = T, isoVar = "recovered"
)

plotSaveFun(testingVariant$gridChart+
              ggtitle("Variants Move the Steady State"),
            "testingVariant")

plotSaveFun(testingVariant$isoquant+
              ggtitle("Testing Mitigates Effects of Variants") +
              ylab("Variant Additional External Infections"),
            "testingVariantIso")




testingSS <- dimCompare(
  dim1 = "parameters$testingCadence",
  dim1Range = seq(1,14,1), dim1ChartingRange = c(1,3,7,14),
  dim2 = "infectionEvent$externalInfections", 
  dim2Range = seq(0,200,25), dim2ChartingRange = c(0,100,200),
  otherChanges = c("infectionEvent$time"), 
  otherChangesVals = c(50),
  chartingCols = c("externalInfections", "infected","isolated", "recovered"),
  smoothing = F, facetName1 = "TC", facetName2 = "SS",
  isoquant = T, isoVar = "recovered"
)

plotSaveFun(testingSS$gridChart+
              ggtitle("Shocks Move Away From the Steady State"),
            "testingSS")

plotSaveFun(testingSS$isoquant+
              ggtitle("Testing Mitigates Effects of Shocks") +
              ylab("Shock Size"),
            "testingSSIso")





testingSSVar <- dimCompare(
  dim1 = "parameters$testingCadence",
  dim1Range = seq(1,14,1), dim1ChartingRange = c(1,3,7,14),
  dim2 = "infectionEvent$externalInfections", 
  dim2Range = seq(0,200,25), dim2ChartingRange = c(0,100,200),
  otherChanges = c("infectionEvent$time","externalInfectionVariation$amplitude", "externalInfectionVariation$period"), 
  otherChangesVals = c(50,10,20),
  chartingCols = c("externalInfections", "infected","isolated", "recovered"),
  smoothing = F, facetName1 = "TC", facetName2 = "SS",
  isoquant = T, isoVar = "recovered"
)

plotSaveFun(testingSSVar$gridChart+
              ggtitle("Shocks Move Away From the Steady State"),
            "testingSSVar")

