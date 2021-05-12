
computeSurveyRate <- function(data=data,samling_frame) {
  
  target = samling_frame
  return(round(nrow(data)/target,digits = 2)*100)
  
}

computeDurationAverage <- function(data) {
 #round( (data$end - data$start )/60)
 return(round(median(round( (as_datetime(data$end) - as_datetime(data$start) )/60))))
}

kobohr_getdata_csv<-function(url,u,pw){
  #supply url for the data
  rawdata<-GET(url,authenticate(u,pw),progress())
  d_content <- read_csv(content(rawdata,"raw",encoding = "UTF-8"),na = "n/a")
}

kobohr_getforms_csv <-function(url,u,pw){
  #supply url
  rawdata<-GET(url,authenticate(u,pw),progress())
  d_content_csv <-read_csv(content(rawdata,"raw",encoding = "UTF-8"))
}


download_data <- function(dataurl,kobo_user,kobo_pw) {
  d_raw <-  kobohr_getdata_csv(dataurl,kobo_user,kobo_pw) 
  return(d_raw <- as.data.frame(d_raw))
}