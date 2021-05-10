
computeSurveyRate <- function(data=data,samling_frame) {
  
  target = samling_frame

  return(round(nrow(data)/target,digits = 2)*100)
  
}

computeDurationAverage <- function(data) {
 #round( (data$end - data$start )/60)
 return(round(median(round( (as_datetime(data$end) - as_datetime(data$start) )/60))))
}

