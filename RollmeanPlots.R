library(tidyverse)
library(here)
library(lubridate)
library(zoo)
# data from
# http://www.bom.gov.au/climate/data/index.shtml

lst <- list.files(here("values"), pattern = "precip")
i <- 1
for (i in 1:length(lst)){
  loc <- str_split_fixed(lst[i], "_", 2)[,2]
df <- readRDS(here("values", lst[i])) %>%
  mutate(date = ymd(paste0(str_sub(date, end = 4), "-", 
                           str_sub(date, start = 5, end = 6), "-01"))) %>%
  rename(rain = mean) %>%
  group_by(date) %>%
  summarise(rain = sum(rain)) %>%
  filter(date > ymd("1910-01-01"))

period <- 1
df <- df %>% #bind_rows(df.sp, df.da) %>%
  #filter(date > ymd("1000-01-01") & loc == "Steep Point") %>%
  #dplyr::select(-Year) %>%
  ungroup()%>%
  mutate(rmean = rollmean(rain, k = period * 12, fill = NA, align = 'right'))

df.min <- df[which(df$rmean == min(df$rmean, na.rm = TRUE)), ]

df.min.p <- filter(df, date <= df.min$date[nrow(df.min)] & date > df.min$date[nrow(df.min)] - 365, )

df.g <- df %>% mutate(Year = year(date)) %>% 
  group_by(Year) %>%
  summarise(y.rain = sum(rain))
mean(df.g$y.rain)
sum(df.min.p$rain)
  
ggplot(df, aes(date, rain))+
  geom_col()+
  geom_line(aes(y=rmean), col = "red", size = 1) +
  labs(y = "Monthly Rainfall (mm)", x = "Date", 
       title = paste0(loc, ": ", min(df$date), " to ", max(df$date)), 
       subtitle = paste0("Lowest 12 months of rainfall, ", 
                         df.min.p$date[1], " to ", df.min.p$date[12], " = ", round(sum(df.min.p$rain), 1), " mm\n",
                         "Annual mean = ", round(mean(df.g$y.rain),1), " mm"), 
       caption = "12 month running mean in red\n Source: Bureau of Meteorology")+
  #coord_cartesian(ylim = c(0, 200))+ 
  theme_bw()
ggsave(here("graphs", paste0(loc, "_", max(df$date),".jpg" )), width = 7, height = 5)
#write.csv(df, here("tmp.csv"))
}

################################################################
lst <- list.files(here("values"), pattern = "tmax-mean")
i <- 1
for (i in 1:length(lst)){
  loc <- str_split_fixed(lst[i], "_", 2)[,2]
  df <- readRDS(here("values", lst[i])) %>%
    mutate(date = ymd(paste0(str_sub(date, end = 4), "-", 
                             str_sub(date, start = 5, end = 6), "-01"))) %>%
    rename(tmax = mean) %>%
    group_by(date) %>%
    summarise(tmax = mean(tmax)) %>%
    filter(date > ymd("1910-01-01"))
  
  period <- 1
  df <- df %>% #bind_rows(df.sp, df.da) %>%
    #filter(date > ymd("1000-01-01") & loc == "Steep Point") %>%
    #dplyr::select(-Year) %>%
    ungroup()%>%
    mutate(rmean = rollmean(tmax, k = period * 12, fill = NA, align = 'right'))
  
  df.min <- df[which(df$rmean == max(df$rmean, na.rm = TRUE)), ]
  
  df.min.p <- filter(df, date <= df.min$date[nrow(df.min)] & date > df.min$date[nrow(df.min)] - 365, )
  
  df.g <- df %>% mutate(Year = year(date)) %>% 
    group_by(Year) %>%
    summarise(y.tmax = mean(tmax))

  
  ggplot(df, aes(date, tmax))+
    geom_col()+
    geom_line(aes(y=rmean), col = "red", size = 1) +
    labs(y = "Monthly Rainfall (mm)", x = "Date", 
         title = paste0(loc, ": ", min(df$date), " to ", max(df$date)), 
         subtitle = paste0("Highest 12 months of temperature, ", 
                           df.min.p$date[1], " to ", df.min.p$date[12], " = ", round(mean(df.min.p$tmax), 1), " \n",
                           "Annual mean = ", round(mean(df.g$y.tmax),1), " "), 
         caption = "12 month running mean in red\n Source: Bureau of Meteorology")+
    coord_cartesian(ylim = c(10, 32))+
    theme_bw()
  ggsave(here("graphs", paste0(loc, "_tmax_", max(df$date),".jpg" )), width = 7, height = 5)
  #write.csv(df, here("tmp.csv"))
  
}


################################################################
lst <- list.files(here("values"), pattern = "vapourpres15")
i <- 1
for (i in 1:length(lst)){
  loc <- str_split_fixed(lst[i], "_", 2)[,2]
  df <- readRDS(here("values", lst[i])) %>%
    mutate(date = ymd(paste0(str_sub(date, end = 4), "-", 
                             str_sub(date, start = 5, end = 6), "-01"))) %>%
    rename(vpd = mean) %>%
    group_by(date) %>%
    summarise(vpd = mean(vpd)) %>%
    filter(date > ymd("1910-01-01"))
  
  period <- 1
  df <- df %>% #bind_rows(df.sp, df.da) %>%
    #filter(date > ymd("1000-01-01") & loc == "Steep Point") %>%
    #dplyr::select(-Year) %>%
    ungroup()%>%
    mutate(rmean = rollmean(vpd, k = period * 12, fill = NA, align = 'right'))
  
  df.min <- df[which(df$rmean == max(df$rmean, na.rm = TRUE)), ]
  
  df.min.p <- filter(df, date <= df.min$date[nrow(df.min)] & date > df.min$date[nrow(df.min)] - 365, )
  
  df.g <- df %>% mutate(Year = year(date)) %>% 
    group_by(Year) %>%
    summarise(y.vpd = mean(vpd))
  
  
  ggplot(df, aes(date, vpd))+
    geom_col()+
    geom_line(aes(y=rmean), col = "red", size = 1) +
    labs(y = "Vapor pressure", x = "Date", 
         title = paste0(loc, ": ", min(df$date), " to ", max(df$date)), 
         subtitle = paste0("Highest 12 months of vpd, ", 
                           df.min.p$date[1], " to ", df.min.p$date[12], " = ", round(mean(df.min.p$vpd), 1), " \n",
                           "Annual mean = ", round(mean(df.g$y.vpd),1), " "), 
         caption = "12 month running mean in red\n Source: Bureau of Meteorology")+
    coord_cartesian(ylim = c(8, 18))+ 
    theme_bw()
  ggsave(here("graphs", paste0(loc, "_vpd_", max(df$date),".jpg" )), width = 7, height = 5)
  #write.csv(df, here("tmp.csv"))
}
