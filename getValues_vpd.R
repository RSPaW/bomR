library(raster)
library(sf)
library(here)
library(tidyverse)
library(doParallel)
options(scipen = 999)

p.tif <- list.files("E:\\AGCD\\bomR\\vapourpres15\\01day", pattern = ".nc$", 
                    recursive = TRUE)
tmp <- raster(paste0("E:\\AGCD\\bomR\\vapourpres15\\01day\\",  p.tif[1]))

shps <- list.files(here("shp"), pattern = "shp$")

i <- 1
for (i in 1:length(shps)){
  shp <- st_read(here("shp", shps[i])) %>%
    st_transform(crs = crs(tmp))
  plot(shp)

    #Define how many cores (memory is limiting factor here)
  UseCores <- 15
  #Register CoreCluster
  cl <- makeCluster(UseCores)
  registerDoParallel(cl)
 e <- 1
  df <- foreach(e= 1:length(p.tif),.combine=rbind) %dopar% {
    library(raster)
    library(sf)
    library(here)
    library(tidyverse)
    library(fasterize)
    r1 <- raster(paste0("E:\\AGCD\\bomR\\vapourpres15\\01day\\",  p.tif[e]))
  #plot(r1)
    rst <- crop(r1, shp)
    plot(rst)
    df <- as.data.frame(mean(getValues(rst), na.rm = TRUE))
    colnames(df)[1] <- "mean"
    df$date <- str_split_fixed(p.tif[e], "_", 6)[,5]
    df$loc <- str_sub(shps[i], end = -5)
    df
    
  } 
  stopCluster(cl)  
  
  cat(i ,"of", length(shps), "\n")
  saveRDS(df, here("values", paste0("vapourpres15_", str_sub(shps[i], end = -5))))

}

