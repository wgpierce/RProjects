# This program simulates the number of times that a given confidence interval will 
# capture the true meanof a distribution, to help show that a confidence interval 
# with a given confidence level will in fact capture the mean with the given level
# of confidence
# NOTE: We do not have to output the confidence level to a file

mean <- 10 
sd <- 10
confidenceLevel <- .95
sampleSize <- 40
numIntervals <- 500
fileName <- 'output.txt'
outputFile <- file(description = fileName, open = "w")
sink(outputFile)

#Loop to create 30 confidence intervals
for (n in 1:numIntervals) {
  RandomData <- rnorm(sampleSize,mean=mean,sd=sd) #Generate Random Data
  print(t.test(RandomData,conf.level=confidenceLevel))#Output Results to Environment
}
sink(type = "output")#reroute output back to STDout
close(outputFile)

#set up regexp to find confidence interval
library("stringr")
inputFile <- paste(readLines(fileName), collapse="")
regexp <- "[0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+"
intervalMatrix <- str_match_all(inputFile,regexp)

#Search through and evaluate confidence intervals
numMeanCaptured <- 0
for (n in 1:numIntervals) {
  splitIntervalMatrix <- strsplit(intervalMatrix[[1]][n,1], " ")
  lowerValue <- as.double(splitIntervalMatrix[[1]][1])
  upperValue <- as.double(splitIntervalMatrix[[1]][2])
  if (lowerValue < mean && mean < upperValue) {
    numMeanCaptured <- numMeanCaptured  + 1
  }
}

sprintf("The number of times the mean was captured this time is %i of %i", numMeanCaptured, numIntervals)
#unlink(outputFile)   #Uncomment if output file should be deleted