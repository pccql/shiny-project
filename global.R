library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)

master_df <- read.csv('forestFires.csv')
state_list <- c('Alagoas', 'Bahia', 'Ceara', 'Maranhao',  'Paraiba','Pernambuco','Piaui' , 'Sergipe')

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$date, format='%Y-%m-%d')