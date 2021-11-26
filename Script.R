# Import Data
library(readxl)
SCOPES_DATA <- read_excel("SCOPES DATA.xlsx")
above_15_df <- read_excel(path = "SCOPES DATA.xlsx", sheet = "above15")
below_15_df <- read_excel(path = "SCOPES DATA.xlsx", sheet = "below15")
View(SCOPES_DATA)

CITIES_COL <- SCOPES_DATA['Cities']
cities_above_15_names <- CITIES_COL[SCOPES_DATA['isAbove15'] == 1]
cities_below_15_names <- CITIES_COL[SCOPES_DATA['isAbove15'] == 0]

