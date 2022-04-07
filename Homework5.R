install.packages(c("lubridate", "ggplot2", "dplyr", "olsrr"))
install.packages("forecast")
install.packages("PerformanceAnalytics")

library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(olsrr)
library(PerformanceAnalytics)

ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

ETall <- ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean(Ensemble.ET))

Pistachios <- ETall %>%
  filter(crop == "Pistachios")

Pistachio_ts <- na.omit(ts(Pistachios$ET.in, 
                   start=c(2016,1),
                   frequency = 12))

Pistachio_decompose <- decompose(Pistachio_ts)

plot(Pistachio_decompose)

acf(na.omit(Pistachio_ts), 
    lag.max=24)

pacf(na.omit(Pistachio_ts), 
     lag.max = 24)

model1 <- arima(Pistachio_ts,
                order=c(1,0,0))

#####Homework 5
#Question 1

#run log
# log transform methane fluxes
ghg$log.ch4 <- log(ghg$ch4+1)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

unique(ghg$Region)

# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

# transform CO2
ghg$NEW.CO2 <- ((1)/(ghg$co2+1000))

# multiple regression
# creates a model object
mod.fully <- lm(NEW.CO2 ~ mean.depth+
                  airTemp+
                  log.age+
                  log.DIP+
                  log.precip+
                  log.ch4, data=ghg)

summary(mod.fully)

regl <- lm(ghg$precipitation ~ ghg$runoff)
summary(regl)
str(regl)

#get reg table
regTable <- summary(mod.fully)$coefficients
#write to file then click more>export
write.csv(regTable, "/cloud/project/reg_out.csv")

#Question 2
#almonds, pistachios, fallow/idle fields, corn, and table grapes
#summarise data in inches
ETaller <- ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean(Ensemble.ET))

#almonds
Almonds <- ETaller %>%
  filter(crop == "Almonds")

Almonds_ts <- na.omit(ts(Almonds$ET.in, 
                           start=c(2016,1),
                           frequency = 12))

Almonds_decompose <- decompose(Almonds_ts)

plot(Almonds_decompose)


#pistachios
Pistachios <- ETaller %>%
  filter(crop == "Pistachios")

Pistachio_ts <- na.omit(ts(Pistachios$ET.in, 
                           start=c(2016,1),
                           frequency = 12))

Pistachio_decompose <- decompose(Pistachio_ts)

plot(Pistachio_decompose)

#Fallow/Idle Cropland
Fallow <- ETaller %>%
  filter(crop == "Fallow/Idle Cropland")

Fallow_ts <- na.omit(ts(Fallow$ET.in, 
                           start=c(2016,1),
                           frequency = 12))

Fallow_decompose <- decompose(Fallow_ts)

plot(Fallow_decompose)               
                  
#corn
Corn <- ETaller %>%
  filter(crop == "Corn")

Corn_ts <- na.omit(ts(Corn$ET.in, 
                        start=c(2016,1),
                        frequency = 12))

Corn_decompose <- decompose(Corn_ts)

plot(Corn_decompose)

#table grapes
TableGrapes <- ETaller %>%
  filter(crop == "Grapes (Table/Raisin)")

TableGrapes_ts <- na.omit(ts(TableGrapes$ET.in, 
                      start=c(2016,1),
                      frequency = 12))

TableGrapes_decompose <- decompose(TableGrapes_ts)

plot(TableGrapes_decompose)

#Question3
#autoregressive model for pistachios, and fallow/idle fields

#pistachios
Pista_y <- na.omit(Pistachio_ts)
modelPista <- arima(Pista_y , # data 
                order = c(1,0,0))
modelPista

modelP <- arima(Pista_y , # data 
                order = c(4,0,0)) 
modelP

#new pista
newPista <- forecast(modelP)
newPista

#make dataframe for plotting
newPistaF <- data.frame(newPista)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistaF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Pistachios$date[1]),newPistaF$dateF[24])+  # Plotting original data
  geom_line(data = newPistaF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistaF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#fallow/idle fields
Fallow_y <- na.omit(Fallow_ts)
modelFallow <- arima(Fallow_y , # data 
                    order = c(1,0,0))
modelFallow

modelF <- arima(Fallow_y , # data 
                order = c(4,0,0)) 
modelF

#new Fallow
newFallow <- forecast(modelF)
newFallow

#make dataframe for plotting
newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Fallow$date[1]),newFallowF$dateF[24])+  # Plotting original data
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

