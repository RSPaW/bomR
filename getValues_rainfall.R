library(raster)
library(sf)
library(here)
library(tidyverse)
options(scipen = 999)

p.tif <- list.files("annual.precip", pattern = "tif$")

aoi <- st_read(here("Vectors", "tenure.shp")) %>%
  st_transform("+proj=longlat +a=6378137 +rf=298.257232666016 +no_defs")
locs <- unique(aoi$LEG_NAME)

df.all <- data.frame()

i <- 1
for (i in 1:length(p.tif)){
  r1 <- raster(here("annual.precip", p.tif[i]))
  #plot(r1)
  e <- 1
  
  #Define how many cores (memory is limiting factor here)
  #UseCores <- 11
  #Register CoreCluster
  #cl <- makeCluster(UseCores)
  #registerDoParallel(cl)
  #i <- 1
  #df <- foreach(e= 1:length(locs),.combine=rbind) %dopar% {
    library(raster)
    library(sf)
    library(here)
    library(tidyverse)
    library(fasterize)
    e1 <- aoi
    #e1 <- filter(aoi, LEG_NAME == locs[e]) %>%
    #  st_transform(crs(r1))
    rst <- crop(r1, e1)
    #plot(rst)
    df <- as.data.frame(mean(getValues(rst), na.rm = TRUE))
    colnames(df)[1] <- "mean"
    df$year <- str_sub(p.tif[i], -8, -5)
    df$loc <- locs[e]
    #df
    
  #} 
  #stopCluster(cl)  
  
  df.all <- bind_rows(df, df.all)
  cat(i ,"of", length(p.tif), "\n")
}
df.all$year <- as.numeric(df.all$year )
df.all <- arrange(df.all, year) %>%
  filter(year >= 1907)
########################################################
#
library(strucchange)
title <- "Annual rainfall for Wandoo National Park"
i <- 1
#for (i in 1:length(locs)){
dm <- df.all
#dm <- filter(df.all, loc == locs[i]) %>%
#  arrange(year)
#if (nrow(dm)!=0){
bp.t <- breakpoints(dm$mean ~ 1, h = 0.15)
dm$seg <- breakfactor(bp.t, breaks = 2)


b1 <- sum(breakfactor(bp.t, breaks = 2)== "segment1")
b2 <- sum(breakfactor(bp.t, breaks = 2)== "segment2")
b3 <- sum(breakfactor(bp.t, breaks = 2)== "segment3")

s1.lab <- paste0(min(dm$year), ".to.", dm$year[b1])
s2.lab <- paste0(dm$year[b1+1], ".to.", dm$year[b1 + b2])
s3.lab <- paste0(dm$year[b1+b2+1], ".to.", max(dm$year))

dm$lab[dm$seg == "segment1"] <- s1.lab
dm$lab[dm$seg == "segment2"] <- s2.lab
dm$lab[dm$seg == "segment3"] <- s3.lab

df.mean <- dm %>% group_by(lab) %>%
  summarise(m = mean(mean))

dm$lab <- factor(dm$lab, levels = c(s1.lab, s2.lab, s3.lab))

library(ggpubr)
comp <- list(c(s1.lab, s2.lab), c(s2.lab, s3.lab), c(s1.lab, s3.lab))
p1 <- ggplot(dm, aes(lab, mean))+
  geom_boxplot()+
  #stat_compare_means(method = "t.test")+
  stat_compare_means(comparisons = comp)+
  #stat_compare_means(method = "anova")+
  ylim(c(500, 1500))+
  labs(x = "Period", caption = paste0("total change in mean precip = ", 
                                      round(df.mean[3, 2] - df.mean[1, 2], 2 ), " mm"))+
  theme_bw()

p2 <- ggplot(dm, aes(year, mean))+
  geom_point()+
  geom_vline(xintercept = dm$year[b1])+
  geom_vline(xintercept = dm$year[b1 + b2])+
  # first mean
  annotate("segment", x = min(dm$year), xend = dm$year[b1], 
           y = df.mean$m[1], yend = df.mean$m[1],
           colour = "blue", size = 1.1)+
  annotate("text", x = (min(dm$year)+dm$year[b1])/2, 
           y = df.mean$m[1]+40, label = paste0(round(df.mean$m[1], 0), " mm"),
           colour = "blue", size = 5)+
  # second mean
  annotate("segment", x = dm$year[b1], xend = dm$year[b1+b2], 
           y = df.mean$m[2], yend = df.mean$m[2],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1]+dm$year[b1+b2])/2, 
           y = df.mean$m[2]+40, label = paste0(round(df.mean$m[2], 0), " mm"),
           colour = "blue", size = 5)+
  # second mean
  annotate("segment", x = dm$year[b1+b2], xend = max(dm$year), 
           y = df.mean$m[3], yend = df.mean$m[3],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1+b2]+ max(dm$year))/2, 
           y = df.mean$m[3]+25, label = paste0(round(df.mean$m[3], 0), " mm"),
           colour = "blue", size = 5)+
  labs(title = paste0(title))+
  theme_bw()

ggarrange(p2, p1, ncol = 1)

ggsave(here("graphs", paste0("precip.dif3.jpg")), height = 6, width = 6)
#}
#}
s <- 4
ggplot(dm, aes(year, mean))+
  geom_point(alpha = 0.5)+
  geom_vline(xintercept = dm$year[b1])+
  geom_vline(xintercept = dm$year[b1 + b2])+
  # first mean
  annotate("segment", x = min(dm$year), xend = dm$year[b1], 
           y = df.mean$m[1], yend = df.mean$m[1],
           colour = "blue", size = 1.1)+
  annotate("text", x = (min(dm$year)+dm$year[b1])/2, 
           y = df.mean$m[1]+40, label = paste0(round(df.mean$m[1], 0), " mm"),
           colour = "blue", size = s)+
  # second mean
  annotate("segment", x = dm$year[b1], xend = dm$year[b1+b2], 
           y = df.mean$m[2], yend = df.mean$m[2],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1]+dm$year[b1+b2])/2, 
           y = df.mean$m[2]+40, label = paste0(round(df.mean$m[2], 0), " mm"),
           colour = "blue", size = s)+
  # second mean
  annotate("segment", x = dm$year[b1+b2], xend = max(dm$year), 
           y = df.mean$m[3], yend = df.mean$m[3],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1+b2]+ max(dm$year))/2, 
           y = df.mean$m[3]+25, label = paste0(round(df.mean$m[3], 0), " mm"),
           colour = "blue", size = s)+
  labs(title = paste0(title),
       y = "Annual Rainfall (mm)",
       caption = paste0("total change in mean precip = ", 
                        round(df.mean[2, 2] - df.mean[1, 2], 0 ), " mm"))+
  theme_bw()

ggsave(here("graphs", paste0("precipPts.dif3.pts.jpg")), height = 3.5, width = 6)

#########################################################################################

i <- 1
#for (i in 1:length(locs)){
dm <- df.all
#dm <- filter(df.all, loc == locs[i]) %>%
#  arrange(year)
#if (nrow(dm)!=0){
bp.t <- breakpoints(dm$mean ~ 1, h = 0.15)
#dm$seg <- breakfactor(bp.t, breaks = 2)
dm$seg <- breakfactor(bp.t, breaks = 1)

b1 <- sum(breakfactor(bp.t, breaks = 1)== "segment1")
b2 <- sum(breakfactor(bp.t, breaks = 1)== "segment2")
#b3 <- sum(breakfactor(bp.t, breaks = 2)== "segment3")

s1.lab <- paste0(min(dm$year), ".to.", dm$year[b1])
s2.lab <- paste0(dm$year[b1+1], ".to.", dm$year[b1 + b2])
#s3.lab <- paste0(dm$year[b1+b2+1], ".to.", max(dm$year))

dm$lab[dm$seg == "segment1"] <- s1.lab
dm$lab[dm$seg == "segment2"] <- s2.lab
#dm$lab[dm$seg == "segment3"] <- s3.lab

df.mean <- dm %>% group_by(lab) %>%
  summarise(m = mean(mean))

dm$lab <- factor(dm$lab, levels = c(s1.lab, s2.lab))#, s3.lab))

library(ggpubr)
comp <- list(c(s1.lab, s2.lab))#, c(s2.lab, s3.lab), c(s1.lab, s3.lab))
p1 <- ggplot(dm, aes(lab, mean))+
  geom_boxplot()+
  stat_compare_means(method = "t.test")+
  #stat_compare_means(comparisons = comp)+
  #stat_compare_means(method = "anova")+
  ylim(c(500, 1500))+
  labs(x = "Period", caption = paste0("total change in mean precip = ", 
                                      round(df.mean[2, 2] - df.mean[1, 2], 2 ), " mm"))+
  theme_bw()

p2 <- ggplot(dm, aes(year, mean))+
  geom_point()+
  geom_vline(xintercept = dm$year[b1])+
  #geom_vline(xintercept = dm$year[b1 + b2])+
  # first mean
  annotate("segment", x = min(dm$year), xend = dm$year[b1], 
           y = df.mean$m[1], yend = df.mean$m[1],
           colour = "blue", size = 1.1)+
  annotate("text", x = (min(dm$year)+dm$year[b1])/2, 
           y = df.mean$m[1]+40, label = paste0(round(df.mean$m[1], 0), " mm"),
           colour = "blue", size = 5)+
  # second mean
  annotate("segment", x = dm$year[b1], xend = dm$year[b1+b2], 
           y = df.mean$m[2], yend = df.mean$m[2],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1]+dm$year[b1+b2])/2, 
           y = df.mean$m[2]+40, label = paste0(round(df.mean$m[2], 0), " mm"),
           colour = "blue", size = 5)+
  # # second mean
  # annotate("segment", x = dm$year[b1+b2], xend = max(dm$year), 
  #          y = df.mean$m[3], yend = df.mean$m[3],
  #          colour = "blue", size = 1.1)+
  # annotate("text", x = (dm$year[b1+b2]+ max(dm$year))/2, 
  #          y = df.mean$m[3]+25, label = paste0(round(df.mean$m[3], 0), " mm"),
  #          colour = "blue", size = 5)+
  labs(title = paste0(title))+
  theme_bw()

ggarrange(p2, p1, ncol = 1)

ggsave(here("graphs", paste0("precip.dif2.jpg")), height = 6, width = 6)
#}
#}
s <- 4
ggplot(dm, aes(year, mean))+
  geom_point(alpha = 0.5)+
  geom_vline(xintercept = dm$year[b1])+
  #geom_vline(xintercept = dm$year[b1 + b2])+
  # first mean
  annotate("segment", x = min(dm$year), xend = dm$year[b1], 
           y = df.mean$m[1], yend = df.mean$m[1],
           colour = "blue", size = 1.1)+
  annotate("text", x = (min(dm$year)+dm$year[b1])/2, 
           y = df.mean$m[1]+40, label = paste0(round(df.mean$m[1], 0), " mm"),
           colour = "blue", size = s)+
  # second mean
  annotate("segment", x = dm$year[b1], xend = dm$year[b1+b2], 
           y = df.mean$m[2], yend = df.mean$m[2],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1]+dm$year[b1+b2])/2, 
           y = df.mean$m[2]+40, label = paste0(round(df.mean$m[2], 0), " mm"),
           colour = "blue", size = s)+
  # second mean
  annotate("segment", x = dm$year[b1+b2], xend = max(dm$year), 
           y = df.mean$m[3], yend = df.mean$m[3],
           colour = "blue", size = 1.1)+
  annotate("text", x = (dm$year[b1+b2]+ max(dm$year))/2, 
           y = df.mean$m[3]+25, label = paste0(round(df.mean$m[3], 0), " mm"),
           colour = "blue", size = s)+
  labs(title = paste0(title),
       y = "Annual Rainfall (mm)",
       caption = paste0("total change in mean precip = ", 
                        round(df.mean[2, 2] - df.mean[1, 2], 0 ), " mm"))+
  theme_bw()

ggsave(here("graphs", paste0("precipPts.dif2.pts.jpg")), height = 3.5, width = 6)
