library(dplyr)
library(tidyverse)


#Import animal outcome dataset from csv
animalDFraw <- read.csv("C:\\Users\\purdy\\desktop\\AnimalOutcome-2018-2019.csv", header = FALSE)

#View structure and first 20 rows
#head(animalDFraw,20)
#str(animalDFraw)

#Delete out the top rows of garbage and empty columns
animalDFraw2 <- tail(animalDFraw,-8)
animalDF <- Filter(function(x)!all(is.na(x)), animalDFraw2)
animalDF <- animalDF[!grepl("Total", animalDF$V12),]

#Function to shift up column
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

#Convert data frame columns to characters instead of factors
animalDF$V2 <- animalDF$V1
animalDF[] <- lapply(animalDF, as.character)

#Shift columns up 1 row 
animalDF$V2 <- shift(animalDF$V2, 1)

write.csv(animalDF,"ShelterAnimals.csv",row.names=F)
