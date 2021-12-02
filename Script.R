# Import Data
library(readxl)
library(tibbletime)
library(dplyr)
library(lubridate)
library(ggplot2)


SCOPES_DATA <- read_excel("SCOPES DATA.xlsx")
above_15_df <- read_excel(path = "SCOPES DATA.xlsx", sheet = "above15")
below_15_df <- read_excel(path = "SCOPES DATA.xlsx", sheet = "below15")
View(SCOPES_DATA)

# col naming
CITIES_COL <- SCOPES_DATA['Cities']
cities_above_15_names <- CITIES_COL[SCOPES_DATA['isAbove15'] == 1]
cities_below_15_names <- CITIES_COL[SCOPES_DATA['isAbove15'] == 0]

# exploratory cpi plots
plot(above_15_df$'Time', above_15_df$'SeattleCPI', ylim = c(200, 325), col="white", xlab = "Time", ylab = "CPI", main = "Major Cities CPI data over Time")

legend(1, 320, legend=c("Above $15 minimum wage", "Below $15 minimum wage"),
       col=c("orange", "blue"), lty=1:1, cex=0.8)

lines(above_15_df$'Time', above_15_df$'SeattleCPI', col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$LACPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$BostonCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$NYCCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$SanFranciscoCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$SanDiegoCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$RiversideCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$MinneapolisCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$WADCCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$ChicagoCPI, col="orange", lwd=1.75)
lines(above_15_df$Time, above_15_df$DenverCPI, col="orange", lwd=1.75)



lines(below_15_df$Time, below_15_df$HoustonCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$DallasCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$PhiladelphiaCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$AtlantaCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$HonoluluCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$TampaCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$StLouisCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$DetroitCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$AnchorageCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$BaltimoreCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$MiamiCPI, col="blue", lwd=1.75)
lines(below_15_df$Time, below_15_df$PhoenixCPI, col="blue", lwd=1.75)

# Seattle cost of goods reg model
SeattleCostOfGoods_lm <- lm(SCOPES_DATA$SeattleCPI ~ SCOPES_DATA$SeattleMIN)
summary(SeattleCostOfGoods_lm)
plot(SeattleCostOfGoods_lm)

# Seattle unemployment reg model 
SeattleUnemployment_lm <- lm(SCOPES_DATA$SeattleUN ~ SCOPES_DATA$SeattleMIN)
summary(SeattleUnemployment_lm)
plot(SeattleUnemployment_lm)

# San Francisco cost of goods reg model
SanFranciscoCostOfGoods_lm <- lm(SCOPES_DATA$SanFranciscoCPI ~ SCOPES_DATA$SanFranciscoMIN)
summary(SanFranciscoCostOfGoods_lm)
plot(SanFranciscoCostOfGoods_lm)

# San Francisco unemployment reg model
SanFranciscoUnemployment_lm <- lm(SCOPES_DATA$SanFranciscoUN ~ SCOPES_DATA$SanFranciscoMIN)
summary(SanFranciscoUnemployment_lm)
plot(SanFranciscoUnemployment_lm)

# Tampa cost of goods linear reg model
TampaCostOfGoods_lm <- lm(SCOPES_DATA$TampaCPI ~ SCOPES_DATA$TampaMIN)
summary(TampaCostOfGoods_lm)
plot(TampaCostOfGoods_lm)

# Phoenix cost of goods linear reg model
PhoenixCostOfGoods_lm <- lm(SCOPES_DATA$PhoenixCPI ~ SCOPES_DATA$PhoenixMIN)
summary(PhoenixCostOfGoods_lm)
plot(PhoenixCostOfGoods_lm)

# Honululu cost of goods linear reg model
HonoluluCostOfGoods_lm <- lm(HonoluluCPI ~ HonululuMIN, data=SCOPES_DATA)
summary(HonoluluCostOfGoods_lm)
plot(HonoluluCostOfGoods_lm)

# Combine columns

CPIcombined <- bind_cols(SCOPES_DATA$DallasCPI, SCOPES_DATA$AtlantaCPI, SCOPES_DATA$BostonCPI, SCOPES_DATA$DetroitCPI, SCOPES_DATA$LACPI, SCOPES_DATA$MinneapolisCPI, SCOPES_DATA$PhiladelphiaCPI, SCOPES_DATA$RiversideCPI, SCOPES_DATA$SanDiegoCPI, SCOPES_DATA$SeattleCPI, SCOPES_DATA$AnchorageCPI, SCOPES_DATA$WADCCPI, SCOPES_DATA$BaltimoreCPI, SCOPES_DATA$ChicagoCPI, SCOPES_DATA$DenverCPI, SCOPES_DATA$HoustonCPI, SCOPES_DATA$MiamiCPI, SCOPES_DATA$NYCCPI, SCOPES_DATA$PhoenixCPI, SCOPES_DATA$StLouisCPI, SCOPES_DATA$SanFranciscoCPI, SCOPES_DATA$TampaCPI, SCOPES_DATA$HonoluluCPI)

CPIcombined <- na.omit(CPIcombined)

UNcombined <- bind_cols(SCOPES_DATA$DallasUN, SCOPES_DATA$AtlantaUN, SCOPES_DATA$BostonUN, SCOPES_DATA$DetroitUN, SCOPES_DATA$LAUN, SCOPES_DATA$MinneapolisUN, SCOPES_DATA$PhiladelphiaUN, SCOPES_DATA$RiversideUN, SCOPES_DATA$SanDiegoUN, SCOPES_DATA$SeattleUN, SCOPES_DATA$AnchorageUN, SCOPES_DATA$WADCUN, SCOPES_DATA$BaltimoreUN, SCOPES_DATA$ChicagoUN, SCOPES_DATA$DenverUN, SCOPES_DATA$HoustonUN, SCOPES_DATA$MiamiUN, SCOPES_DATA$NYCUN, SCOPES_DATA$PhoenixUN, SCOPES_DATA$StLouisUN, SCOPES_DATA$SanFranciscoUN, SCOPES_DATA$TampaUN, SCOPES_DATA$HonoluluUN)

UNcombined <- na.omit(UNcombined)

MINcombined <- bind_cols(SCOPES_DATA$DallasMin, SCOPES_DATA$AtlantaMin, SCOPES_DATA$BostonMin, SCOPES_DATA$DetroitMin, SCOPES_DATA$LAMIN, SCOPES_DATA$MinneapolisMIN, SCOPES_DATA$PhiladelphiaMIN, SCOPES_DATA$RiversideMIN, SCOPES_DATA$SanDiego, SCOPES_DATA$SeattleMIN, SCOPES_DATA$AnchorageMin, SCOPES_DATA$WADCMIN, SCOPES_DATA$BaltimoreMIN, SCOPES_DATA$ChicagoMIN, SCOPES_DATA$DenverMIN, SCOPES_DATA$HoustonMIN, SCOPES_DATA$MiamiMIN, SCOPES_DATA$NYCMIN, SCOPES_DATA$PhoenixMIN, SCOPES_DATA$StLouisMIN, SCOPES_DATA$SanFranciscoMIN, SCOPES_DATA$TampaMIN, SCOPES_DATA$HonululuMIN)

MINcombined <- na.omit(MINcombined)

# Multiple Regression model 

model1 <- lm(CPIcombined ~ MINcombined)

model2 <- lm(CPIcombined ~ UNcombined)

# City based regression modeling


# Averaging all cities that have above $15 min wage
SanFrancisco_CPI <- SCOPES_DATA$SanFranciscoCPI
LA_CPI <- SCOPES_DATA$LACPI
Seattle_CPI <- SCOPES_DATA$SeattleCPI
NY_CPI <- SCOPES_DATA$NYCCPI
WashDC_CPI <- SCOPES_DATA$WADCCPI

Avg_CPI <- c()


for (i in 1:length(SanFrancisco_CPI)) {
  CPI_row_mean <- mean(c(SanFrancisco_CPI[i], LA_CPI[i], Seattle_CPI[i], NY_CPI[i], WashDC_CPI[i]))
  Avg_CPI[i] <- CPI_row_mean
}

SanFrancisco_Min <- SCOPES_DATA$SanFranciscoMIN
LA_Min <- SCOPES_DATA$LAMIN
Seattle_Min <- SCOPES_DATA$SeattleMIN
NY_Min <- SCOPES_DATA$NYCMIN
WashDC_Min <- SCOPES_DATA$WADCMIN

Avg_Min <- c()

for (i in 1:length(SanFrancisco_Min)) {
  Min_row_mean <- mean(c(SanFrancisco_Min[i], LA_Min[i], Seattle_Min[i], NY_Min[i], WashDC_Min[i]))
  Avg_Min[i] <- Min_row_mean
}

Avg_CPI_and_Min_lm <- lm(Avg_CPI ~ Avg_Min)
summary(Avg_CPI_and_Min_lm)
plot(Avg_CPI_and_Min_lm)

SanFrancisco_UN <- SCOPES_DATA$SanFranciscoUN
LA_UN <- SCOPES_DATA$LAUN
Seattle_UN <- SCOPES_DATA$SeattleUN
NY_UN <- SCOPES_DATA$NYCUN
WashDC_UN <- SCOPES_DATA$WADCUN

Avg_UN <- c()

for (i in 1:length(SanFrancisco_UN)) {
  UN_row_mean <- mean(c(SanFrancisco_UN[i], LA_UN[i], Seattle_UN[i], NY_UN[i], WashDC_UN[i]))
  Avg_UN[i] <- UN_row_mean
}

Avg_Min_and_UN_lm <- lm(Avg_UN ~ Avg_Min)
summary(Avg_CPI_and_UN_lm)
plot(Avg_CPI_and_UN_lm)

#find average cost of goods rise per year in cities that have not changed min wage vs cities that have
time <- SCOPES_DATA$Time

yearlyVec <- which(month(as.POSIXlt(time, format("%Y-%m-%d"))) == 9)

SCOPES_DATA_yearly <- SCOPES_DATA[yearlyVec,]

AvgCPI <- c()
city <- c()
start_minWage <- c()
end_minWage <- c()

# cities still at $7.25

#Houston
Houston_CPI <- SCOPES_DATA_yearly$HoustonCPI
Houston_Min <- SCOPES_DATA_yearly$HoustonMIN


Houston_CPI_percent_increase <- c()

for (i in 1:(length(Houston_CPI) - 1)) {
  Houston_CPI_percent_increase[i] <- ((Houston_CPI[i + 1] - Houston_CPI[i]) / abs(Houston_CPI[i])) * 100.0
}

Houston_Avg_CPI_percent_increase <- mean(Houston_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Houston_Avg_CPI_percent_increase)
city <- c(city, "Houston")
start_minWage <- c(start_minWage, Houston_Min[1])
end_minWage <- c(end_minWage, Houston_Min[length(Houston_Min)])


# Dallas
Dallas_CPI <- SCOPES_DATA_yearly$DallasCPI
Dallas_Min <- SCOPES_DATA_yearly$DallasMin


Dallas_CPI_percent_increase <- c()

for (i in 1:(length(Dallas_CPI) - 1)) {
  Dallas_CPI_percent_increase[i] <- ((Dallas_CPI[i + 1] - Dallas_CPI[i]) / abs(Dallas_CPI[i])) * 100.0
}

Dallas_Avg_CPI_percent_increase <- mean(Dallas_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Dallas_Avg_CPI_percent_increase)
city <- c(city, "Dallas")
start_minWage <- c(start_minWage, Dallas_Min[1])
end_minWage <- c(end_minWage, Dallas_Min[length(Dallas_Min)])


# Philadelphia
Philadelphia_CPI <- SCOPES_DATA_yearly$PhiladelphiaCPI
Philadelphia_Min <- SCOPES_DATA_yearly$PhiladelphiaMIN

Philadelphia_CPI_percent_increase <- c()

for (i in 1:(length(Philadelphia_CPI) - 1)) {
  Philadelphia_CPI_percent_increase[i] <- ((Philadelphia_CPI[i + 1] - Philadelphia_CPI[i]) / abs(Philadelphia_CPI[i])) * 100.0
}

Philadelphia_Avg_CPI_percent_increase <- mean(Philadelphia_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Philadelphia_Avg_CPI_percent_increase)
city <- c(city, "Philadelphia")
start_minWage <- c(start_minWage, Philadelphia_Min[1])
end_minWage <- c(end_minWage, Philadelphia_Min[length(Philadelphia_Min)])


# cities that raised from $7.25 but are under $15

# Boston
Boston_CPI <- SCOPES_DATA_yearly$BostonCPI
Boston_Min <- SCOPES_DATA_yearly$BostonMin


Boston_CPI_percent_increase <- c()

for (i in 1:(length(Boston_CPI) - 1)) {
  Boston_CPI_percent_increase[i] <- ((Boston_CPI[i + 1] - Boston_CPI[i]) / abs(Boston_CPI[i])) * 100.0
}

Boston_Avg_CPI_percent_increase <- mean(Boston_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Boston_Avg_CPI_percent_increase)
city <- c(city, "Boston")
start_minWage <- c(start_minWage, Boston_Min[1])
end_minWage <- c(end_minWage, Boston_Min[length(Boston_Min)])


# Minneapolis only has 2018-present day data
Minneapolis_CPI <- na.omit(SCOPES_DATA_yearly$MinneapolisCPI)
Minneapolis_Min <- SCOPES_DATA_yearly$MinneapolisMIN


Minneapolis_CPI_percent_increase <- c()

for (i in 1:(length(Minneapolis_CPI) - 1)) {
  Minneapolis_CPI_percent_increase[i] <- ((Minneapolis_CPI[i + 1] - Minneapolis_CPI[i]) / abs(Minneapolis_CPI[i])) * 100.0
}

Minneapolis_Avg_CPI_percent_increase <- mean(Minneapolis_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Minneapolis_Avg_CPI_percent_increase)
city <- c(city, "Minneapolis")
start_minWage <- c(start_minWage, Minneapolis_Min[1])
end_minWage <- c(end_minWage, Minneapolis_Min[length(Minneapolis_Min)])



# Denver only has 2018-present day data
Denver_CPI <- na.omit(SCOPES_DATA_yearly$DenverCPI)
Denver_Min <- SCOPES_DATA_yearly$DenverMIN

Denver_CPI_percent_increase <- c()

for (i in 1:(length(Denver_CPI) - 1)) {
  Denver_CPI_percent_increase[i] <- ((Denver_CPI[i + 1] - Denver_CPI[i]) / abs(Denver_CPI[i])) * 100.0
}

Denver_Avg_CPI_percent_increase <- mean(Denver_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Denver_Avg_CPI_percent_increase)
city <- c(city, "Denver")
start_minWage <- c(start_minWage, Denver_Min[1])
end_minWage <- c(end_minWage, Denver_Min[length(Denver_Min)])


# Phoenix only has 2018-present day data
Phoenix_CPI <- na.omit(SCOPES_DATA_yearly$DenverCPI)
Phoenix_Min <- SCOPES_DATA_yearly$PhoenixMIN

Phoenix_CPI_percent_increase <- c()

for (i in 1:(length(Phoenix_CPI) - 1)) {
  Phoenix_CPI_percent_increase[i] <- ((Phoenix_CPI[i + 1] - Phoenix_CPI[i]) / abs(Phoenix_CPI[i])) * 100.0
}

Phoenix_Avg_CPI_percent_increase <- mean(Phoenix_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Phoenix_Avg_CPI_percent_increase)
city <- c(city, "Phoenix")
start_minWage <- c(start_minWage, Phoenix_Min[1])
end_minWage <- c(end_minWage, Phoenix_Min[length(Phoenix_Min)])


# Honolulu only has 2018-present day data
Honolulu_CPI <- na.omit(SCOPES_DATA_yearly$HonoluluCPI)
honolulu_Min <- SCOPES_DATA_yearly$HonululuMIN


Honolulu_CPI_percent_increase <- c()

for (i in 1:(length(Honolulu_CPI) - 1)) {
  Honolulu_CPI_percent_increase[i] <- ((Honolulu_CPI[i + 1] - Honolulu_CPI[i]) / abs(Honolulu_CPI[i])) * 100.0
}

Honolulu_Avg_CPI_percent_increase <- mean(Honolulu_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Honolulu_Avg_CPI_percent_increase)
city <- c(city, "Honolulu")
start_minWage <- c(start_minWage, honolulu_Min[1])
end_minWage <- c(end_minWage, honolulu_Min[length(honolulu_Min)])


# San Diego only has 2018-present day data
SanDiego_CPI <- na.omit(SCOPES_DATA_yearly$SanDiegoCPI)
SanDiego_Min <- SCOPES_DATA_yearly$SanDiegoMIN


SanDiego_CPI_percent_increase <- c()

for (i in 1:(length(SanDiego_CPI) - 1)) {
  SanDiego_CPI_percent_increase[i] <- ((SanDiego_CPI[i + 1] - SanDiego_CPI[i]) / abs(SanDiego_CPI[i])) * 100.0
}

SanDiego_Avg_CPI_percent_increase <- mean(SanDiego_CPI_percent_increase)

AvgCPI <- c(AvgCPI, SanDiego_Avg_CPI_percent_increase)
city <- c(city, "San Diego")
start_minWage <- c(start_minWage,  SanDiego_Min[1])
end_minWage <- c(end_minWage, SanDiego_Min[length(SanDiego_Min)])

# cities that eventually went $15 or above


# Seattle
Seattle_CPI <- SCOPES_DATA_yearly$SeattleCPI
Seattle_Min <- SCOPES_DATA_yearly$SeattleMIN

Seattle_CPI_percent_increase <- c()

for (i in 1:(length(Seattle_CPI) - 1)) {
  Seattle_CPI_percent_increase[i] <- ((Seattle_CPI[i + 1] - Seattle_CPI[i]) / abs(Seattle_CPI[i])) * 100.0
}

Seattle_Avg_CPI_percent_increase <- mean(Seattle_CPI_percent_increase)

AvgCPI <- c(AvgCPI, Seattle_Avg_CPI_percent_increase)
city <- c(city, "Seattle")
start_minWage <- c(start_minWage,  Seattle_Min[1])
end_minWage <- c(end_minWage, Seattle_Min[length(Seattle_Min)])


# San Francisco
SanFrancisco_CPI <- SCOPES_DATA_yearly$SanFranciscoCPI
sanFrancisco_Min <- SCOPES_DATA_yearly$SanFranciscoMIN

SanFrancisco_CPI_percent_increase <- c()

for (i in 1:(length(SanFrancisco_CPI) - 1)) {
  SanFrancisco_CPI_percent_increase[i] <- ((SanFrancisco_CPI[i + 1] - SanFrancisco_CPI[i]) / abs(SanFrancisco_CPI[i])) * 100.0
}

SanFrancisco_Avg_CPI_percent_increase <- mean(SanFrancisco_CPI_percent_increase)

AvgCPI <- c(AvgCPI, SanFrancisco_Avg_CPI_percent_increase)
city <- c(city, "San Francisco")
start_minWage <- c(start_minWage,  sanFrancisco_Min[1])
end_minWage <- c(end_minWage, sanFrancisco_Min[length(sanFrancisco_Min)])


# New York City
NYC_CPI <- SCOPES_DATA_yearly$NYCCPI
NYC_Min <- SCOPES_DATA_yearly$NYCMIN

NYC_CPI_percent_increase <- c()

for (i in 1:(length(NYC_CPI) - 1)) {
  NYC_CPI_percent_increase[i] <- ((NYC_CPI[i + 1] - NYC_CPI[i]) / abs(NYC_CPI[i])) * 100.0
}

NYC_Avg_CPI_percent_increase <- mean(NYC_CPI_percent_increase)

AvgCPI <- c(AvgCPI, NYC_Avg_CPI_percent_increase)
city <- c(city, "NYC")
start_minWage <- c(start_minWage,  NYC_Min[1])
end_minWage <- c(end_minWage, NYC_Min[length(NYC_Min)])


# Los Angeles
LA_CPI <- SCOPES_DATA_yearly$LACPI
LA_Min <- SCOPES_DATA_yearly$LAMIN

LA_CPI_percent_increase <- c()

for (i in 1:(length(LA_CPI) - 1)) {
  LA_CPI_percent_increase[i] <- ((LA_CPI[i + 1] - LA_CPI[i]) / abs(LA_CPI[i])) * 100.0
}

LA_Avg_CPI_percent_increase <- mean(LA_CPI_percent_increase)

AvgCPI <- c(AvgCPI, LA_Avg_CPI_percent_increase)
city <- c(city, "LA")
start_minWage <- c(start_minWage,  LA_Min[1])
end_minWage <- c(end_minWage, LA_Min[length(LA_Min)])


# Washington DC 
WADC_CPI <- SCOPES_DATA_yearly$WADCCPI
WADC_Min <- SCOPES_DATA_yearly$WADCMIN

WADC_CPI_percent_increase <- c()

for (i in 1:(length(WADC_CPI) - 1)) {
  WADC_CPI_percent_increase[i] <- ((WADC_CPI[i + 1] - WADC_CPI[i]) / abs(WADC_CPI[i])) * 100.0
}

WADC_Avg_CPI_percent_increase <- mean(WADC_CPI_percent_increase)

AvgCPI <- c(AvgCPI, WADC_Avg_CPI_percent_increase)
city <- c(city, "Washington DC")
start_minWage <- c(start_minWage,  WADC_Min[1])
end_minWage <- c(end_minWage, WADC_Min[length(WADC_Min)])

AvgCPI_increase_df <- data.frame(cityName=city, AverageCPI=AvgCPI, StartMin=start_minWage, EndMin=end_minWage)


AvgCPI_increase_df <- AvgCPI_increase_df[order(AvgCPI_increase_df$AverageCPI),]

row.names(AvgCPI_increase_df) <- NULL

AvgCPI_increase_df <- AvgCPI_increase_df %>%
  mutate (EndMin = as.double(EndMin),
          minWageScale = cut(x=EndMin, 
                         breaks = c(-Inf, 7.50, 14.99, Inf),
                         labels = c("Stayed", "Increased", "Increased 15 or above"),
                         right = TRUE)
  )

ggplot(data=AvgCPI_increase_df, mapping=aes(x=reorder(cityName, AverageCPI), y=AverageCPI, fill=minWageScale)) + 
    geom_col()



       