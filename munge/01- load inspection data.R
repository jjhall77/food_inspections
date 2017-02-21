# 01- load food inspection data

url <- "https://data.cityofnewyork.us/resource/xx67-kt59.json"


#. Need to submit issue re: RSocrata's reading of JSON file
#token <- 
#df <- read.socrata(url, app_token = token)
#write_rds(df, 'data/food_data.Rda')
df <- read_rds('data/food_data.Rda')


#fix column names:
names(df) <- tolower(names(df))
names(df) <- make.names(names(df))


##date formatting
df$inspection.date <- mdy(df$inspection.date)
df$record.date <- mdy(df$record.date)
df$grade.date <- mdy(df$grade.date)


##Add closed indicator
df$closed <- ifelse(str_detect(df$action, "Closed"),1,0)
df$closed <- ifelse(str_detect(df$action, "closed"),1,df$closed)
df$critical <- ifelse(df$critical.flag=="Critical",1,0)
