# Preliminary Data Exploration

# Load Libraries 
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggforce)

# Seattle Unemployment Data
SUD <- read_csv("file.csv")

View(SUD)
plot(x=SUD$Year, y=SUD$Value)

# Metropolitan Unemployment Data
sam <- read_excel("ssamatab1.xlsx")
View(ssamatab1)
cleansam <- na.omit(sam)
plot(x=cleansam$...5, y=cleansam$...10, xlab= "Time", ylab= "Unemployment Rates", main = "Unemployment over Time in Metropolitian USA")
