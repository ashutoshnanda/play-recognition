setwd("~/OneDrive/Documents/College/Junior Year/Research - Prof. Iyengar/basketball/produceActionSet/")

library(dplyr)

segmentPossessions <- function(tracking.data) {
  start.possession.event.code <- 23
  dribble.event.code <- 21
  current.possession <- 1
  event.codes <- tracking.data$event.id
  tracking.data["possession.number"] <- NA
  in.possession <- F
  for(index in 1:nrow(tracking.data)) {
    current.event.code <- ifelse(is.na(event.codes[index]),
                                 ifelse(in.possession, dribble.event.code, 0), 
                                 event.codes[index])
    if(current.event.code == 21 | current.event.code == 23) {
      tracking.data$possession.number[index] <- current.possession
      if(!in.possession) {
        in.possession <- T
      }  
    } else {
      if(in.possession) {
        in.possession <- F
      }
    }
  }
  tracking.data
}

sample.segmentation <- function() {
  tracking.file.name <- "trackingMat_2016021016.csv"
  data <- read.csv(tracking.file.name, row.names = 1, stringsAsFactors = F)
  write.csv(segmentPossessions(data), gsub("\\.", "withPossessionSegmentation.", tracking.file.name), row.names = F)
}