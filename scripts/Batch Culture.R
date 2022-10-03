library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)



MBTWKD1 <- read_excel("data/MBTWKD1.xlsx")


library(growthcurver)                    
d <- MBTWKD1                       
gc_fit1 <- SummarizeGrowth(d$Time, d$Fructose)  
plot(gc_fit1) 




gc_fit2 <- SummarizeGrowth(d$Time, d$Biomass)  
plot(gc_fit2) 

gc_fit3 <- SummarizeGrowth(d$Time, d$Acetate)  
plot(gc_fit3) 

growth_rate <- MBTWKD1 %>%
  arrange(Time) %>%
  mutate(Diff_hour = Time - lag(Time),  
         Diff_growth = Biomass - lag(Biomass), 
         Rate_percent = (Diff_growth / Diff_hour)/Biomass * 100) 

Average_growth <- mean(growth_rate$Rate_percent, na.rm = TRUE)




shiny::runGitHub("FitGrowth", "angelovangel", subdir = "v02-drc/")


