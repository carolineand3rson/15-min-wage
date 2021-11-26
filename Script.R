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
plot(above_15_df$'Time', above_15_df$'SeattleCPI', ylim = c(200, 350), col="white")
lines(above_15_df$'Time', above_15_df$'SeattleCPI', col="blue")
lines(above_15_df$Time, above_15_df$LACPI, col="blue")
lines(above_15_df$Time, above_15_df$BostonCPI, col="blue")
lines(above_15_df$Time, above_15_df$NYCCPI, col="blue")
lines(above_15_df$Time, above_15_df$SanFranciscoCPI, col="blue")

lines(below_15_df$Time, below_15_df$HoustonCPI, col="red")
lines(below_15_df$Time, below_15_df$DallasCPI, col="red")
lines(below_15_df$Time, below_15_df$PhiladelphiaCPI, col="red")
lines(below_15_df$Time, below_15_df$AtlantaCPI, col="red")
lines(below_15_df$Time, below_15_df$HonoluluCPI, col="red")
lines(below_15_df$Time, below_15_df$TampaCPI, col="red")
lines(below_15_df$Time, below_15_df$StLouisCPI, col="red")
