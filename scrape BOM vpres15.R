library(raster)
library(stringr)
library(XML)
library(lubridate)
library(sf)
library(httr)
library(xml2)
library(RCurl)
library(gdalUtilities)
library(terra)
library(here)
library(doParallel)

# Set the base directory where all the subfolders are located
base_folder <- "E:\\AGCD\\bomR"

# precip calib 1 day

dir.create(here("vapourpres15"))
dir.create(here("vapourpres15", "01day"))

server <- "http://opendap.bom.gov.au:8080/thredds/fileServer/agcd/vapourpres_h15/mean/r005/01day/"

yrs <- 1971:year(Sys.Date())
i<- 1

UseCores <- 10
#Register CoreCluster
cl <- makeCluster(UseCores, outfile="")
registerDoParallel(cl)

foreach(i = 1:length(yrs)) %dopar% {
  library(httr)
  library(xml2)
  library(RCurl)
  library(here)
#for (i in 1:length(yrs)) {
  dir.create(here("vapourpres15", "01day", yrs[i]), showWarnings = FALSE)
  m <- 1
  for (m in 1:12){
    if(m < 10){
      mnth <- paste0(0, m)
    }else{
      mnth <- m
    }
    d <- 1
    for (d in 1:31){
      if(d < 10){
        dy <- paste0(0, d)
      }else{
        dy <- d
      }
      filename <- paste0(yrs[i], "/vapourpres_h15_mean_r005_", yrs[i], mnth, dy, "_",  yrs[i], mnth, dy,".nc" ) 
      destfile <- here("vapourpres15", "01day", filename)
    
      if (file.exists(destfile) == FALSE){
        #url.name <- "http://opendap.bom.gov.au:8080/thredds/fileServer/agcd/vapourpres_h15/mean/r005/01day/1971/vapourpres_h15_mean_r005_19710101_19710101.nc"
        url.name <- paste0(server, filename)
        if(url.exists(url.name)) {
          download.file(url.name, destfile, mode = "wb", method = "curl", extra = "--max-time 600")
        } 
      }
    }
  } 
  cat(yrs[i], "is done\n")
}
stopCluster(cl)