# data_clean

rm(list = ls(all = T))
library(dplyr)
library(lubridate)
library(stringr)

db.f <- read.csv('../Intermediate/forecast.csv',   stringsAsFactors = F)
db.h <- read.csv('../Source_Data/histWeather.csv', stringsAsFactors = F)
db.l <- read.csv('../Source_Data/locations.csv',   stringsAsFactors = F)

# Restrict to first prediction of the day...
db.f <- db.f[db.f$forecast.rank == 1, ]
db.f$forecast.length <- as.character(as.period(
  interval(ymd(db.f$current.date), ymd(db.f$target.date)),
  units = 'day'
))
db.f$forecast.length <- as.numeric(str_extract(db.f$forecast.length, '\\d+'))
summary(db.f$forecast.length)

db.l$rownum <- 1:nrow(db.l)
db.h$key <- paste(db.h$AirPtCd, db.h$Date, sep = '|')

db.min.t <- db.f[db.f$forecast.type == 'MinTemp', ]
db.max.t <- db.f[db.f$forecast.type == 'MaxTemp', ]
db.rain  <- db.f[db.f$forecast.type == 'ProbPrecip', ]

db.min.t <- merge(
  db.min.t, db.l,
  by.x = 'city.number', by.y = 'rownum',
  all = F
)
db.max.t <- merge(
  db.max.t, db.l,
  by.x = 'city.number', by.y = 'rownum',
  all = F
)
db.rain <- merge(
  db.rain, db.l,
  by.x = 'city.number', by.y = 'rownum',
  all = F
)
  
db.min.t$key <- paste(db.min.t$AirPtCd, db.min.t$target.date, sep = '|')
db.max.t$key <- paste(db.max.t$AirPtCd, db.max.t$target.date, sep = '|')
db.rain$key  <- paste(db.rain$AirPtCd,  db.rain$target.date,  sep = '|')
  
db.min.t <- merge(
  db.min.t, db.h[, c("Date", "Min_TemperatureF", "Events", "key")],
  by = 'key',
  all = F
)
db.max.t <- merge(
  db.max.t, db.h[, c("Date", "Max_TemperatureF", "Events", "key")],
  by = 'key',
  all = F
)
db.rain <- merge(
  db.rain, db.h[, c("Date", "PrecipitationIn", "Events", "key")],
  by = 'key',
  all = F
)

fltr.rain <- rep(T, nrow(db.rain))
for (i in unique(db.rain$city.number)) {
  tmp <- db.rain[db.rain$city.number == i, ]
  maxf <- max(tmp$forecast.length)
  for (j in 0:maxf) {
    maxp <- max(tmp$target.value[tmp$forecast.length == j], na.rm = T)
    if (is.na(maxp) | length(maxp) == 0) { maxp <- 0 }
    if (maxp == 0) {
      cat(paste0(
        "city ", i,
        ", forecast length ", j,
        " all forecasts are for zero precip. deleting...\r\n"
      ))
      fltr.rain[db.rain$city.number == i & db.rain$forecast.length == j] <- F
    }
  }
}
db.rain <- db.rain[fltr.rain, ]

db.max.t$target.value <- as.numeric(db.max.t$target.value)
db.min.t$target.value <- as.numeric(db.min.t$target.value)
db.max.t <- db.max.t[!is.na(db.max.t$target.value) & !is.na(db.max.t$Max_TemperatureF), ]
db.min.t <- db.min.t[!is.na(db.min.t$target.value) & !is.na(db.min.t$Min_TemperatureF), ]

write.csv(
  db.min.t[, !(names(db.min.t) %in% c('city.number', 'key', "current.time", "Date"))], 
  '../Intermediate/MinTemp.csv',
  row.names = F
)
write.csv(
  db.max.t[, !(names(db.max.t) %in% c('city.number', 'key', "current.time", "Date"))], 
  '../Intermediate/MaxTemp.csv',
  row.names = F
)
write.csv(
  db.rain[, !(names(db.rain) %in% c('city.number', 'key', "current.time", "Date"))], 
  '../Intermediate/Precip.csv',
  row.names = F
)
