# Import Data
library(readxl)
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
