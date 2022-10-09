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



partbform <- read_excel("data/partbform.xlsx")


mmdf$v <- predict(MM.model, newdata=mmdf)

ggplot(data = partbform, aes(x = glucose, y = growthrate)) +
  theme_bw() +
  xlab("Glucose Concentration [mM]") +
  ylab("Growth rate (h-1") +
  ggtitle("Techvidvan Michaelis-Menten kinetics") +
  geom_line(data = partbform)+
            aes(x = glucose, y = growthrate, 
            colour = "green")

library(drc)
model <- drm(growthrate ~ glucose, data = partbform, fct=MM.2)

model1 <- nls(growthrate ~ glucose, data = partbform)

modely <- lm(growthrate ~ glucose, data = partbform )
summary(modely)


MM.model <- drm(v~S, data=data, fct=MM.2())


shiny::runGitHub("FitGrowth", "angelovangel", subdir = "v02-drc/")


