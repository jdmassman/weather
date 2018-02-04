# parse_forecast

rm(list = ls(all = T))
library(dplyr)

df1 <- read.table(
  file = "../Source_Data/forecast.dat",
  header = F,
  sep = "",
  row.names = NULL,
  col.names = c('city.number', 'target.date', 'target.value', 'forecast.type', 'current.date'),
  stringsAsFactors = F
)

df2 <- df1 %>% group_by(city.number, target.date, current.date, forecast.type) %>% mutate(forecast.rank = row_number())

write.csv(df2, '../Intermediate/forecast.csv', row.names = F)
