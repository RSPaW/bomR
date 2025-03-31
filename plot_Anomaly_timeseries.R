library(ggplot2)
library(ggnewscale)
library(cowplot)

#load data
dir <- "C:\\Users\\GavanMcGrath\\OneDrive - Department of Biodiversity, Conservation and Attractions\\SPP 2000-003 Hydrological Responses to Harvesting - Ecosystem Science Program\\FPC\\dailyVals\\"
precip <- readRDS(paste0(dir,"Precip_FPC"))
p_dates <- as.Date(paste0(substr(precip$date,1,4),"-",substr(precip$date,5,6),"-",substr(precip$date,7,8)))
tmax_mean <- readRDS(paste0(dir,"tmax-mean_FPC"))
t_dates <- as.Date(paste0(substr(tmax_mean$date,1,4),"-",substr(tmax_mean$date,5,6),"-",substr(tmax_mean$date,7,8)))
vapour  <- readRDS(paste0(dir,"vapourpres15_FPC"))
v_dates <- as.Date(paste0(substr(vapour$date,1,4),"-",substr(vapour$date,5,6),"-",substr(vapour$date,7,8)))


#Cumulative 18 month rainfalls (historical and Jul-2023 to Jan-2025)
#Compute cumulative 18 month rainfall and plot to show the anomaly
anom_year <- seq.Date(from = as.Date("2023-07-01"),
                      to = as.Date("2024-12-31"),
                      by = "1 day")

#Loop through all years
#calculate a running mean 30 day for that subset 

start.years <- 1923:2023
all.years <- matrix(0,nrow=length(start.years), ncol=length(anom_year))
for (i in start.years){
  year_dates <- seq.Date(from = as.Date(paste0(i,"-07-01")),
                        to = as.Date(paste0(i+1,"-12-31")),
                        by = "1 day")
  pos <- which(p_dates %in% year_dates)
  cp <- cumsum(precip$mean[pos])
  all.years[i-min(start.years)+1,1:length(cp)] <- cp
  
}

stacked.years <- data.frame(date = year_dates[1:(length(year_dates)-1)], 
                            year = rep(1901,length(year_dates)-1),
                            precip = all.years[1,1:(length(year_dates)-1)])
for (j in 2:(nrow(all.years)-1)){
  stacked.years <- rbind(
    stacked.years,
    data.frame(date = year_dates[1:(length(year_dates)-1)], 
               year = rep(1923+j-1,length(year_dates)-1),
               precip = c(all.years[j,1:(length(year_dates)-1)]))                   
  )
}

prior_stats <- t(apply(all.years[1:(nrow(all.years)-1),],MARGIN=2,
                     FUN = function(x){
                       c(min(x),mean(x),max(x))
                     }))
prior_stats <- prior_stats[-dim(prior_stats)[1],]
prior_stats <- data.frame(prior_stats)
names(prior_stats) <- c("p.025","p.500","p.975")
prior_stats$date <- year_dates[1:(length(year_dates)-1)]
prior_stats$current_year <- all.years[nrow(all.years),1:(length(year_dates)-1)] 


cols = rep("grey70",length(unique(stacked.years$year)))
names(cols) <- unique(stacked.years$year)
stacked.years$year <- as.factor(stacked.years$year)
legend.pos <- data.frame(x=rep(seq.Date(from = as.Date("1900-09-01"),
                                       to = as.Date("1900-09-01"),length.out = 2),3),
                         y=rep(c(1500,1400,1300),each=2),
                         ln = factor(rep(c("2023/24","2009/10","Historical"),each=2),levels = c("2023/24","2009/10","Historical"))
)
pplt2 <- ggplot() +
  geom_line(aes(x=date,y=precip,colour = year),data = stacked.years ) +
  scale_color_manual(name = "hist",values = cols,guide = "none") +
  geom_line(aes(x=date,y=precip,colour = "black"),data = stacked.years[stacked.years$year == "2009",], colour = "black",lwd=1.1 ) +
  geom_line(aes(x=date,y=current_year),color="darkred",lwd=1.1,data = prior_stats)+
  theme_bw() + 
  theme(legend.position = c(0.15,0.8),
        plot.margin = unit(c(0.2, 0.9, 0.2, 0.5), 'cm'),
        legend.background = element_rect(fill=NA)) +
  xlab("") +
  ylab("Cumulative\nprecipitation [mm]") +
  scale_x_date(breaks = seq.Date(from = as.Date("2023-07-01"),
                                 to = as.Date("2025-01-01"),by = "3 month"), 
               date_labels = "%b %Y",
               limits = seq.Date(from = as.Date("2023-07-01"),
                                 to = as.Date("2025-01-01"),length.out = 2), expand = c(0,0)) +
  new_scale_color() +
  geom_line(aes(x=x,y=y,colour = ln,lwd=ln),data = legend.pos) +
  scale_color_manual(name="",values=c("darkred","black","grey70")) +
  scale_linewidth_manual(name="",values=c(1.1,1.1,1))
  
pplt2



 sorted_precip <- sort(all.years[,dim(all.years)[2]-1])
 start.years[which(all.years[,dim(all.years)[2]-1] == sorted_precip[1])]
 start.years[which(all.years[,dim(all.years)[2]-1] == sorted_precip[2])]
 

 
 ##########################################################################
 #Temperature
 
 tanom_year <- seq.Date(from = as.Date("2023-06-01"),
                       to = as.Date("2025-01-30"),
                       by = "1 day")
 
 tstart.years <- 1923:2023
 tall.years <- matrix(0,nrow=length(tstart.years), ncol=length(tanom_year))
 for (i in tstart.years){
   year_dates <- seq.Date(from = as.Date(paste0(i,"-06-01")),
                          to = as.Date(paste0(i+2,"-01-30")),
                          by = "1 day")
   pos <- which(t_dates %in% year_dates)
   cp <- stats::filter(tmax_mean$mean[pos], filter = rep(1,30)/30)
   tall.years[i-min(start.years)+1,1:length(cp)] <- cp[1:length(cp)]
   
 }
 
 tstacked.years <- data.frame(date = year_dates[1:(length(year_dates)-1)], 
                             year = rep(1901,length(year_dates)-1),
                             temp = tall.years[1,1:(length(year_dates)-1)])
 for (j in 2:(nrow(tall.years)-1)){
   tstacked.years <- rbind(
     tstacked.years,
     data.frame(date = year_dates[1:(length(year_dates)-1)], 
                year = rep(1923+j-1,length(year_dates)-1),
                temp = c(tall.years[j,1:(length(year_dates)-1)]))                   
   )
 }
 
 tprior_stats <- t(apply(tall.years[1:(nrow(all.years)-1),],MARGIN=2,
                        FUN = function(x){
                          c(min(x),mean(x),max(x))
                        }))
 tprior_stats <- tprior_stats[-dim(tprior_stats)[1],]
 tprior_stats <- data.frame(tprior_stats)
 names(tprior_stats) <- c("p.025","p.500","p.975")
 tprior_stats$date <- year_dates[1:(length(year_dates)-1)]
 tprior_stats$current_year <- tall.years[nrow(tall.years),1:(length(year_dates)-1)] 
 
 cols = rep("grey70",length(unique(tstacked.years$year)))
 names(cols) <- unique(tstacked.years$year)
 tstacked.years$year <- as.factor(tstacked.years$year)
 
 tplt2 <- ggplot() +
   geom_line(aes(x=date,y=temp,colour = year),data = tstacked.years ) +
   scale_color_manual(values = cols) +
   geom_line(aes(x=date,y=temp,colour = "black"),data = tstacked.years[tstacked.years$year == "2009",], colour = "black",lwd=1.1 ) +
   geom_line(aes(x=date,y=current_year),color="darkred",lwd=1.1,data = tprior_stats)+
   theme_bw() + 
   theme(legend.position = "none",
         plot.margin = unit(c(0.2, 0.9, 0.2, 0.5), 'cm')) +
   xlab("") +
   ylab("Mean 30 day\ntemperature [°C]") +
   scale_x_date(breaks = seq.Date(from = as.Date("2023-07-01"),
                                  to = as.Date("2025-01-01"),by = "3 month"), 
                date_labels = "%b %Y",
                limits = seq.Date(from = as.Date("2023-07-01"),
                                  to = as.Date("2025-01-01"),length.out = 2), expand = c(0,0))
 tplt2

 ##########################################################################
 #VPD
 
 vanom_year <- seq.Date(from = as.Date("2023-06-01"),
                        to = as.Date("2025-01-30"),
                        by = "1 day")
 
 vstart.years <- 1973:2023
 vall.years <- matrix(0,nrow=length(vstart.years), ncol=length(vanom_year))
 for (i in vstart.years){
   year_dates <- seq.Date(from = as.Date(paste0(i,"-06-01")),
                          to = as.Date(paste0(i+2,"-01-30")),
                          by = "1 day")
   pos <- which(v_dates %in% year_dates)
   cp <- stats::filter(vapour$mean[pos], filter = rep(1,30)/30)
   vall.years[i-min(vstart.years)+1,1:length(cp)] <- cp[1:length(cp)]
   
 }
 
 vstacked.years <- data.frame(date = year_dates[1:(length(year_dates)-1)], 
                              year = rep(1901,length(year_dates)-1),
                              vpd = vall.years[1,1:(length(year_dates)-1)])
 for (j in 2:(nrow(vall.years)-1)){
   vstacked.years <- rbind(
     vstacked.years,
     data.frame(date = year_dates[1:(length(year_dates)-1)], 
                year = rep(1973+j-1,length(year_dates)-1),
                vpd = c(vall.years[j,1:(length(year_dates)-1)]))                   
   )
 }
 
 vprior_stats <- t(apply(vall.years[1:(nrow(vall.years)-1),],MARGIN=2,
                         FUN = function(x){
                           c(min(x),mean(x),max(x))
                         }))
 vprior_stats <- vprior_stats[-dim(vprior_stats)[1],]
 vprior_stats <- data.frame(vprior_stats)
 names(vprior_stats) <- c("p.025","p.500","p.975")
 vprior_stats$date <- year_dates[1:(length(year_dates)-1)]
 vprior_stats$current_year <- vall.years[nrow(vall.years),1:(length(year_dates)-1)] 
 
 
 cols = rep("grey70",length(unique(vstacked.years$year)))
 names(cols) <- unique(vstacked.years$year)
 vstacked.years$year <- as.factor(vstacked.years$year)
 
 vplt2 <- ggplot() +
   geom_line(aes(x=date,y=vpd,colour = year),data = vstacked.years ) +
   scale_color_manual(values = cols) +
   geom_line(aes(x=date,y=vpd,colour = "black"),data = vstacked.years[vstacked.years$year == "2009",], colour = "black",lwd=1.1 ) +
   geom_line(aes(x=date,y=current_year),color="darkred",lwd=1.1,data = vprior_stats)+
   theme_bw() + 
   theme(legend.position = "none",
         plot.margin = unit(c(0.2, 0.9, 0.2, 0.5), 'cm')) +
   xlab("") +
   ylab("Mean 30 day\nVPD [kPa]") +
   scale_x_date(breaks = seq.Date(from = as.Date("2023-07-01"),
                                  to = as.Date("2025-01-01"),by = "3 month"), 
                date_labels = "%b %Y",
                limits = seq.Date(from = as.Date("2023-07-01"),
                                  to = as.Date("2025-01-01"),length.out = 2), expand = c(0,0))
 vplt2
 

 cowplot::plot_grid(pplt2,tplt2,vplt2 ,labels = c('A', 'B','C'), label_size = 12, ncol = 1,
                    align="hv")
 
 
