library(vars)
library(tsDyn)
library(tseries)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(fixest)
library(lubridate)
library(xts)
library(cowplot)  
library(ggthemes) 
library(scales)
library(mFilter)
library(aTSA)
library(urca)
library(seasonal)

geometric.mean <- function(x,na.rm=TRUE)
{ 
  (prod(x))^(1/3) }

# EU ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data$eu <- data$eu / 100

data$eu <- data$eu + 1

df <- apply.quarterly(data[,1:2], geometric.mean, na.rm = T)

df$eu <- df$eu - 1

df$eu <- ((df$eu + 1) ^ 4) - 1

hicp_ts <- ts(df, frequency=4,start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df <- as.data.frame(final(sa_series))
df$date <- seq(from = as.Date("1996-03-01"), by = "3 months", length.out = length(df[[1]]))

tseries::adf.test(df$x) # Stationary
tseries::kpss.test(df$x) # Stationary

#filter_hicp <- hpfilter(df$x, freq = 1600)
#df$x <- filter_hicp$cycle

plot.ts(df$x)

#tseries::adf.test(df$x) # Stationary
#tseries::kpss.test(df$x) # Stationary

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_q.csv")

## New Shadow (Leo Krippner) ------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/krippner_shadow.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data$shadow <- data$shadow / 100
data$shadow <- data$shadow + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$shadow <- df$shadow - 1

df2 <- ts(df$shadow, frequency = 4, start = c(1995,1))

# Seasonal adjustment

sa_series <- seas(df2)
plot.ts(df2)
plot.ts(final(sa_series)) 
autoplot(sa_series) +
  ggtitle("X11 decomposition of total assets of ECB") # No seasonality

tseries::adf.test(df$shadow) # Not stationary
tseries::kpss.test(df$shadow) # Not stationary

#filter_shadow <- hpfilter(df$shadow, type = "lambda", freq = 1600)
#df$shadow <- filter_shadow$cycle

#tseries::adf.test(df$shadow) # Not stationary
#tseries::kpss.test(df$shadow) # Stationary

df$shadow_d <- c(NA, diff(df$shadow))

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/new_shadow_q.csv")

# Germany ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[,c(1, 4)] 

data$Germany <- data$Germany / 100
data$Germany <- data$Germany + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Germany <- df$Germany - 1

df$Germany <- ((df$Germany + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Germany <- final(sa_series)
df$t <- 1:nrow(df)

tseries::adf.test(df$Germany) # Stationary
tseries::kpss.test(df$Germany) # Stationary

plot.ts(df$Germany)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_germany.csv")

# Austria ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[,c(1, 17)]
data$Austria <- data$Austria / 100
data$Austria <- data$Austria + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Austria <- df$Austria - 1

df$Austria <- ((df$Austria + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Austria <- final(sa_series)

tseries::adf.test(df$Austria) # Stationary
tseries::kpss.test(df$Austria) # Not stationary

plot.ts(df$Austria)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_austria.csv")


# France ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 9)]
data$France <- data$France / 100
data$France <- data$France + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$France <- df$France - 1

df$France <- ((df$France + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$France <- final(sa_series)

df$t <- 1:nrow(df)
tseries::adf.test(df$France) # Not stationary
tseries::kpss.test(df$France) # Stationary

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_france.csv")

# Greece ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 7)]

data$Greece <- data$Greece / 100

data$Greece <- data$Greece + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Greece <- df$Greece - 1

df$Greece <- ((df$Greece + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Greece <- final(sa_series)

plot.ts(df$Greece)

tseries::adf.test(df$Greece) # Not stationary
tseries::kpss.test(df$Greece) # Not stationary

#filter_hicp <- hpfilter(df$Greece, freq = 1600)
#df$Greece <- filter_hicp$cycle

#tseries::adf.test(df$Greece) # Stationary
#tseries::kpss.test(df$Greece) # Stationary

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_greece.csv")

# Ireland ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 6)]

data$Ireland <- data$Ireland / 100
data$Ireland <- data$Ireland + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Ireland <- df$Ireland - 1

df$Ireland <- ((df$Ireland + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Ireland <- final(sa_series)

tseries::adf.test(df$Ireland) # Stationary
tseries::kpss.test(df$Ireland) # Stationary

plot.ts(df$Ireland)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_ireland.csv")

# Spain ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 8)]
data$Spain <- data$Spain / 100
data$Spain <- data$Spain + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Spain <- df$Spain - 1

df$Spain <- ((df$Spain + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Spain <- final(sa_series)

tseries::adf.test(df$Spain) # Stationary
tseries::kpss.test(df$Spain) # Stationary

plot.ts(df$Spain)

#filter_hicp <- hpfilter(df$Spain, freq = 1600)
#df$Spain <- filter_hicp$cycle

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_spain.csv")

# Belgium ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 3)]
data$Belgium <- data$Belgium / 100
data$Belgium <- data$Belgium + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Belgium <- df$Belgium - 1

df$Belgium <- ((df$Belgium + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Belgium <- final(sa_series)

tseries::adf.test(df$Belgium) # Stationary
tseries::kpss.test(df$Belgium) # Stationary

plot.ts(df$Belgium)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_belgium.csv")

# Cyprus ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 11)]

data$Cyprus <- data$Cyprus / 100
data$Cyprus <- data$Cyprus + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Cyprus <- df$Cyprus - 1

df$Cyprus <- ((df$Cyprus + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Cyprus <- final(sa_series)

tseries::adf.test(df$Cyprus) # Stationary
tseries::kpss.test(df$Cyprus) # Stationary

plot.ts(df$Cyprus)

#filter_hicp <- hpfilter(df$Cyprus, type = "lambda", freq = 1600)
#df$Cyprus <- filter_hicp$cycle

#tseries::adf.test(df$Cyprus) # Stationary
#tseries::kpss.test(df$Cyprus) # Stationary

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_cyprus.csv")

# Italy ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 10)]
data$Italy <- data$Italy / 100
data$Italy <- data$Italy + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Italy <- df$Italy - 1

df$Italy <- ((df$Italy + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Italy <- final(sa_series)

tseries::adf.test(df$Italy) # Stationary
tseries::kpss.test(df$Italy) # Stationary

#filter_i <- hpfilter(df$Italy, type = "lambda", freq = 1600)
#df$Italy <- filter_i$cycle

plot.ts(df$Italy)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_italy.csv")

# Portugal ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 18)]

data$Portugal <- data$Portugal / 100
data$Portugal <- data$Portugal + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Portugal <- df$Portugal -1

df$Portugal <- ((df$Portugal + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Portugal <- final(sa_series)

tseries::adf.test(df$Portugal) # Stationary
tseries::kpss.test(df$Portugal) # Stationary

#filter_i <- hpfilter(df$Portugal, type = "lambda", freq = 1600)
#df$Portugal <- filter_i$cycle

#tseries::adf.test(df$Portugal) # Stationary
#tseries::kpss.test(df$Portugal) # Stationary

plot.ts(df$Portugal)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_portugal.csv")

# Netherlands ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 16)]

data$Netherlands <- data$Netherlands / 100
data$Netherlands <- data$Netherlands + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Netherlands <- df$Netherlands - 1

df$Netherlands <- ((df$Netherlands + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Netherlands <- final(sa_series)

tseries::adf.test(df$Netherlands) # Stationary
tseries::kpss.test(df$Netherlands) # Stationary

#filter_i <- hpfilter(df$Netherlands, type = "lambda", freq = 1600)
#df$Netherlands <- filter_i$cycle

plot.ts(df$Netherlands)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_netherlands.csv")

# Luxembourg ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 14)]

data$Luxembourg <- data$Luxembourg / 100
data$Luxembourg <- data$Luxembourg + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Luxembourg <- df$Luxembourg - 1

df$Luxembourg <- ((df$Luxembourg + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Luxembourg <- final(sa_series)

tseries::adf.test(df$Luxembourg) # Stationary
tseries::kpss.test(df$Luxembourg) # Stationary

#filter_i <- hpfilter(df$Luxembourg, type = "lambda", freq = 1600)
#df$Luxembourg <- filter_i$cycle

plot.ts(df$Luxembourg)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_luxembourg.csv")

# Finland ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 21)]

data$Finland <- data$Finland / 100
data$Finland <- data$Finland + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Finland <- df$Finland - 1

df$Finland <- ((df$Finland + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Finland <- final(sa_series)

tseries::adf.test(df$Finland) # Stationary
tseries::kpss.test(df$Finland) # Stationary

#filter_i <- hpfilter(df$Finland, type = "lambda", freq = 1600)
#df$Finland <- filter_i$cycle

#tseries::adf.test(df$Finland) # Stationary
#tseries::kpss.test(df$Finland) # Stationary

plot.ts(df$Finland)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_finland.csv")

# Malta ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 15)]

data$Malta <- data$Malta / 100
data$Malta <- data$Malta + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Malta <- df$Malta - 1

df$Malta <- ((df$Malta + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Malta <- final(sa_series)

tseries::adf.test(df$Malta) # Stationary
tseries::kpss.test(df$Malta) # Stationary

plot.ts(df$Malta)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_malta.csv")

# Slovenia ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 19)]

data$Slovenia <- data$Slovenia / 100
data$Slovenia <- data$Slovenia + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Slovenia <- df$Slovenia - 1

df$Slovenia <- ((df$Slovenia + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Slovenia <- final(sa_series)

tseries::adf.test(df$Slovenia) # Not stationary
tseries::kpss.test(df$Slovenia) # Stationary

#filter_hicp <- hpfilter(df$Slovenia, type = "lambda", freq = 1600)
#df$Slovenia <- filter_hicp$cycle

plot.ts(df$Slovenia)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_slovenia.csv")

# Estonia ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 5)]

data$Estonia <- data$Estonia / 100
data$Estonia <- data$Estonia + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Estonia <- df$Estonia - 1

df$Estonia <- ((df$Estonia + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Estonia <- final(sa_series)

tseries::adf.test(df$Estonia) # Stationary
tseries::kpss.test(df$Estonia) # Stationary

#filter_hicp <- hpfilter(df$Estonia, type = "lambda", freq = 1600)
#df$Estonia <- filter_hicp$cycle

#tseries::adf.test(df$Estonia) # Not stationary
#tseries::kpss.test(df$Estonia) # Stationary

plot.ts(df$Estonia)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_estonia.csv")

# Slovakia ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1, 20)]

data$Slovakia <- data$Slovakia / 100
data$Slovakia <- data$Slovakia + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Slovakia <- df$Slovakia - 1

df$Slovakia <- ((df$Slovakia + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Slovakia <- final(sa_series)

tseries::adf.test(df$Slovakia) # Not stationary
tseries::kpss.test(df$Slovakia) # Stationary

#filter_hicp <- hpfilter(df$Slovakia, type = "lambda", freq = 1600)
#df$Slovakia <- filter_hicp$cycle

plot.ts(df$Slovakia)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_slovakia.csv")

# Latvia ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1,12)]

data$Latvia <- data$Latvia / 100
data$Latvia <- data$Latvia + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Latvia <- df$Latvia - 1

df$Latvia <- ((df$Latvia + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Latvia <- final(sa_series)

tseries::adf.test(df$Latvia) # Stationary
tseries::kpss.test(df$Latvia) # Stationary

#filter_hicp <- hpfilter(df$Latvia, type = "lambda", freq = 1600)
#df$Latvia <- filter_hicp$cycle

#tseries::adf.test(df$Latvia) # Not stationary
#tseries::kpss.test(df$Latvia) # Stationary

plot.ts(df$Latvia)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_latvia.csv")

# Lithuania ----------------------------------------------------------------------
## HICP --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_m.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data <- data[, c(1,13)]

data$Lithuania <- data$Lithuania / 100
data$Lithuania <- data$Lithuania + 1

df <- apply.quarterly(data, geometric.mean, na.rm = T)
df$Lithuania <- df$Lithuania - 1

df$Lithuania <- ((df$Lithuania + 1)^4) - 1

hicp_ts <- ts(df, frequency=4, start=c(1996,1))

# Seasonal adjustment

sa_series <- seas(hicp_ts)
plot.ts(hicp_ts)
plot.ts(final(sa_series))
autoplot(sa_series) +
  ggtitle("X11 decomposition of HICP") # Seasonality

df$Lithuania <- final(sa_series)

tseries::adf.test(df$Lithuania) # Stationary
tseries::kpss.test(df$Lithuania) # Stationary

#filter_hicp <- hpfilter(df$Lithuania, type = "lambda", freq = 1600)
#df$Lithuania <- filter_hicp$cycle

plot.ts(df$Lithuania)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/hicp_lithuania.csv")