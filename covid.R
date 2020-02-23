library(dplyr)
library(ggplot2)
#deaths data
setwd("/Users/andrey/covid")
dd <- read.csv("time_series_19-covid-Deaths.csv")
dat <- as_tibble(dd)

#calculate daily deaths
days = length(dat) - 5 #number of days for which data is availabe
prev_deaths <- 0;
new_deaths <- vector()
for (i in 5:length(dat)) {
  #Hubei only version (Wuhan)
  #today_total_deaths = sum(dat[dat[1]=="Hubei",i])
  
  #All deaths
  today_total_deaths = sum(dat[i])
  new_deaths[i-5] = today_total_deaths - prev_deaths
  prev_deaths = today_total_deaths
}

#Total deaths (cross-check with officail data)
(sum(dat[length(dat)]))

plotDat <- tibble::enframe(new_deaths, "day","deaths")

ggplot(data = plotDat, mapping = aes(x = day, y = deaths)) +
  geom_point() + 
  geom_smooth()


#moving  (5d)
m_avg <- new_deaths
m_avg[2] <- mean(m_avg[1:2])
m_avg[3] <- mean(m_avg[1:3])
m_avg[4] <- mean(m_avg[1:4])
for (i in 5:length(m_avg)) {
  m_avg[i] <- (m_avg[i-4]+m_avg[i-3]+m_avg[i-2]+m_avg[i-1]+m_avg[i])/5
}
m_avg[length(m_avg)] <- m_avg[length(m_avg)-1] 

plotDat <- tibble::enframe(m_avg, "day","deaths")

ggplot(data = plotDat, mapping = aes(x = day, y = deaths)) +
  geom_point() + 
  geom_smooth()


#by country
#View(dd %>% select(Country.Region, X2.21.20) %>% group_by(Country.Region) %>% summarise(n = sum(X2.21.20)))

