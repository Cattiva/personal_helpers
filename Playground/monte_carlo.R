library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)

getSymbols("AMZN",from="2018-08-01",to="2021-11-24")
AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')

AMZN_mean_log <- mean(AMZN_log_returns) 
AMZN_sd_log <- sd(AMZN_log_returns)

mu<-AMZN_mean_log # mean of log returns
sig<-AMZN_sd_log # sd of log returns 

price<-rep(NA,30)

price[1] <- 3580.41 

#start simulating prices
for(i in 2:30){
  price[i]<- price[i-1]*exp(rnorm(1,mu,sig))
}
random_data<-cbind(price,1:(30))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data%>%
  ggplot(aes(Day,Price))+
  geom_line()+
  labs(title="Amazon (AMZN) price simulation for 4 years")+
  theme_bw()

N<-500
mc_matrix<-matrix(nrow=30,ncol=N)
mc_matrix[1,1]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)
final_mat<-cbind(1:(30),mc_matrix)
final_mat<-as_tibble(final_mat)
colnames(final_mat)<-name
dim(final_mat) #1008 501


final_mat %>%
  tidyr::pivot_longer(cols = 2:501, 
                      names_to = "Simulation",
                      values_to = "Price") %>% 
  dplyr::group_by(Day) %>% 
  dplyr::mutate(mean = mean(Price),
                percentile_95 = quantile(Price, 0.65),
                percentile_05 = quantile(Price, 0.35)) %>%
  ggplot()+
  geom_line(aes(Day, mean), size = 1, color = main_color)+
  geom_ribbon(aes(Day, ymax = percentile_95, ymin = percentile_05), size = 0.5, fill = main_color_light, alpha = 0.3)+
  labs(title="Amazon Stock (AMZN)",
       subtitle = "500 Monte Carlo Simulations for 30 days")+
  theme_linkedin_light()


ggplot2::ggplot(data.frame(x = runif(1000))) +
  geom_density(aes(x))


