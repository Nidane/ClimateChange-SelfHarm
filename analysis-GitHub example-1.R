# Clean up
rm(list=ls())

library(plyr)
library(tidyverse)

wd <- "../analysis.GDPcurrent$+TemChange"

setwd(wd)

df <- read.csv("combined_SelfHarm.GDP.TemChange_.csv")

df$GDP <- as.numeric(df$GDP)
df$Country <- as.factor(df$Country)
df$Years <- df$Year.Code
complete.data <- df

complete.data$SelfHarm.inc.rate <- log10(complete.data$SelfHarm.inc.rate)

library(mgcv)

model0 <- gam(SelfHarm.inc.rate ~ 1 + s(Country, bs = "re"), data = complete.data)
model1 <- gam(SelfHarm.inc.rate ~ s(Country, bs = "re") + s(GDP), data = complete.data)
model2 <- gam(SelfHarm.inc.rate ~ s(Country, bs = "re") + s(Years), data = complete.data)
model3 <- gam(SelfHarm.inc.rate ~ s(Years) + s(GDP) + s(Country, bs = "re"), data = complete.data)
model4 <- gam(SelfHarm.inc.rate ~ te(Years, GDP) + s(Country, bs = "re"), data = complete.data)

model5 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Country, bs = "re"), data = complete.data)
model6 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(Country, bs = "re"), data = complete.data)
model7 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(GDP) + s(Country, bs = "re"), data = complete.data)
model8 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + s(Country, bs = "re"), data = complete.data)
model9 <- gam(SelfHarm.inc.rate ~ s(TemChange) + te(Years, GDP) + s(Country, bs = "re"), data = complete.data)

model10 <- gam(SelfHarm.inc.rate ~ te(TemChange, GDP) + s(Years) + s(Country, bs = "re"), data = complete.data)
model11 <- gam(SelfHarm.inc.rate ~ te(TemChange, Years) + s(GDP) + s(Country, bs = "re"), data = complete.data)
model12 <- gam(SelfHarm.inc.rate ~ te(TemChange, Years, GDP) + s(Country, bs = "re"), data = complete.data)
model13 <- gam(SelfHarm.inc.rate ~ te(TemChange, Years) + s(Country, bs = "re"), data = complete.data)
model14 <- gam(SelfHarm.inc.rate ~ te(TemChange, GDP) + s(Country, bs = "re"), data = complete.data)

model12.2 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + ti(TemChange, Years, GDP) + s(Country, bs = "re"), data = complete.data)
model12.3 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + 
                   ti(TemChange, Years, GDP) + 
                   ti(TemChange, Years) + ti(TemChange, GDP) + ti(GDP, Years) + s(Country, bs = "re"), data = complete.data)

model12.4 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + 
                   ti(TemChange, Years) + ti(TemChange, GDP) + ti(GDP, Years) + s(Country, bs = "re"), data = complete.data)

model12.5 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + ti(TemChange, Years, GDP) + 
                   ti(TemChange, GDP) + ti(GDP, Years) + s(Country, bs = "re"), data = complete.data)

model12.6 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + ti(TemChange, Years, GDP) + 
                   ti(TemChange, Years) + ti(GDP, Years) + s(Country, bs = "re"), data = complete.data)

model12.7 <- gam(SelfHarm.inc.rate ~ s(TemChange) + s(Years) + s(GDP) + ti(TemChange, Years, GDP) + 
                   ti(TemChange, Years) + ti(TemChange, GDP) + s(Country, bs = "re"), data = complete.data)

library(marginaleffects)

plot_predictions(model12.3, condition = list(
  "TemChange", 
  "GDP" = c(quantile(complete.data$GDP)[2], quantile(complete.data$GDP)[3], quantile(complete.data$GDP)[4]), 
  "Years" = c(2020)
)) + theme(
  plot.title = element_text(size = 20),  # Adjust title font size
  axis.title.x = element_text(size = 20), # Adjust x-axis label font size
  axis.title.y = element_text(size = 20), # Adjust y-axis label font size
  axis.text = element_text(size = 20),
  legend.text = element_text(size = 10), 
  strip.text = element_text(size = 20)) + ylim(1.65, 2.15)
