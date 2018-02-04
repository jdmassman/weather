# rain

rm(list = ls(all = T))
library(dplyr)
library(lubridate)
library(stringr)
library(binom)

db <- read.csv('../Intermediate/Precip.csv', stringsAsFactors = F)

db0 <- db[db$forecast.length == 0, ]
head(sort(unique(db0$PrecipitationIn)))
tail(sort(unique(db0$PrecipitationIn)))
db0 <- db0[db0$PrecipitationIn != '', ]
db0$precip.tf <- !(db0$PrecipitationIn %in% c('0', '0.00', 'T'))
sort(unique(db0$target.value))
db0 <- db0[, !(names(db0) %in% c('current.date'))]
str(db0$target.date)
db0$month <- as.numeric(substr(db0$target.date, 6, 7))

# per-month per-city accuracy
db.mo.pos <- db0[db0$precip.tf, ] %>% count(city, state, AirPtCd, month, target.value)
db.mo.tot <- db0                  %>% count(city, state, AirPtCd, month, target.value)
db.mo.pos$key <- paste(db.mo.pos$city, db.mo.pos$state, db.mo.pos$month, db.mo.pos$target.value, sep = '|')
db.mo.tot$key <- paste(db.mo.tot$city, db.mo.tot$state, db.mo.tot$month, db.mo.tot$target.value, sep = '|')
db.mo <- merge(
  db.mo.tot, db.mo.pos[, c('key', 'n')],
  by = 'key',
  all = T
)
names(db.mo)[which(names(db.mo) == 'n.y')] <- 'precip.days'
names(db.mo)[which(names(db.mo) == 'n.x')] <- 'days.count'
db.mo <- db.mo[, names(db.mo) != 'key']
db.mo$precip.days[is.na(db.mo$precip.days)] <- 0
# got enough data??
binom.confint(db.mo$precip.days[1:10], db.mo$days.count[1:10], methods = 'exact')
# well, no

# idea #2
# start by being year-agnostic
# for each day, take a time window of, say, 45 days to either side
# in each window and each prediction level, measure accuracy

db.day <- db0
db.day$year <- year(db.day$target.date)
db.day$day.2014 <- NA
db.day$day.2015 <- NA
db.day$day.2016 <- NA
db.day$day.2017 <- NA
for (yr in 2014:2017) {
  yrdiff <- yr - db.day$year
  vname <- paste0('day.', yr)
  db.day[, vname] <- as.character(ymd(db.day$target.date) %m+% years(yrdiff))
}
db.day$win.start.2014 <- NA
db.day$win.start.2015 <- NA
db.day$win.start.2016 <- NA
db.day$win.start.2017 <- NA
db.day$win.end.2014 <- NA
db.day$win.end.2015 <- NA
db.day$win.end.2016 <- NA
db.day$win.end.2017 <- NA
for (yr in 2014:2017) {
  vm <- paste0('day.', yr)
  d1 <- as.character(ymd(db.day[, vm]) %m-% days(45))
  d2 <- as.character(ymd(db.day[, vm]) %m+% days(45))
  v1 <- paste0('win.start.', yr)
  v2 <- paste0('win.end.',   yr)
  db.day[, v1] <- d1
  db.day[, v2] <- d2
}


t1 <- db.day[db.day$city == 'Seattle', ]

fltr <- t1$target.value == 0
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(11, 250, method = 'exact')

fltr <- t1$target.value == 10
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(5, 5, method = 'exact')

fltr <- t1$target.value == 20
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(2, 16, method = 'exact')

fltr <- t1$target.value == 30
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(7, 18, method = 'exact')

fltr <- t1$target.value == 40
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(4, 9, method = 'exact')

fltr <- t1$target.value == 50
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(4, 7, method = 'exact')

fltr <- t1$target.value == 60
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(2, 4, method = 'exact')

fltr <- t1$target.value == 70
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(2, 6, method = 'exact')

fltr <- t1$target.value == 80
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(14, 16, method = 'exact')

fltr <- t1$target.value == 90
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(12, 12, method = 'exact')

fltr <- t1$target.value == 100
rn <- min(row.names(t1[fltr,]))
v1 <- t1$target.value[row.names(t1) == rn]
v2 <- t1$win.start.2014[row.names(t1) == rn]
v3 <- t1$win.end.2014[row.names(t1) == rn]
v4 <- t1$win.start.2015[row.names(t1) == rn]
v5 <- t1$win.end.2015[row.names(t1) == rn]
v6 <- t1$win.start.2016[row.names(t1) == rn]
v7 <- t1$win.end.2016[row.names(t1) == rn]
v8 <- t1$win.start.2017[row.names(t1) == rn]
v9 <- t1$win.end.2017[row.names(t1) == rn]
t2a <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v2 & t1$target.date <= v3]
t2b <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v4 & t1$target.date <= v5]
t2c <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v6 & t1$target.date <= v7]
t2d <- t1$precip.tf[t1$target.value == v1 & t1$target.date >= v8 & t1$target.date <= v9]
t2 <- c(t2a, t2b, t2c, t2d)
table(t2)
binom.confint(10, 10, method = 'exact')
