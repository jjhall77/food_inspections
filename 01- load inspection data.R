# 01- load food inspection data

url <- "https://data.cityofnewyork.us/resource/xx67-kt59.json"

food_data <- read.socrata(url)
