#STA 141A Final Project R Code
#Donggyun Ha
#Chan Woong Joo
#Sungwon Lee
#Jong Chan Park

# Packages
library('ggplot2')
library('corrgram')
install.packages("ggExtra")
library('ggExtra')
library('lubridate')
library('GGally')


# Importing dataset to the environment
wh <- read.csv('/Users/kjcpark/Desktop/weatherHistory.csv')
attach(wh)

# Corrogram of weather history
corrgram(wh[c(4,5,6,7,8,9,11)],lower.panel = panel.shade, 
         upper.panel = panel.pie,text.panel = panel.txt, main = "Weather History Corrogram")

#Scatter plot of temperature, humidity, Visibility, Wind Speed of each pair
pairs( ~ Temperature..C.+Humidity+Wind.Speed..km.h.+Visibility..km.+ Wind.Speed..km.h.,
       data = wh, main = "Weather History Scatter Plot")



#Transform date into months
wh$Formatted.Date = as.Date(wh$Formatted.Date)
wh$Formatted.Date = months(wh$Formatted.Date)
wh$Formatted.Date

# We have receievd information about faceting grid according to monthly order from the following website: 
# https://www.neonscience.org/dc-time-series-plot-facets-ndvi-r

#Make them into a factor and add it
wh$month_name = factor(wh$Formatted.Date, levels = c('January','February','March',
                                                   'April','May','June','July',
                                                   'August','September','October',
                                                   'November','December'))

#GGplots of Temperature and Humidity for each month
plot1 = ggplot(wh, aes(Temperature..C. , Humidity))  + facet_wrap(~month_name, nc = 3) + 
  geom_smooth() + ggtitle("Temperature and Humidity across months") + xlab("Temperature (C)") +
  ylab("Humidity") +theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) +
  theme(text = element_text(size=12))

#GGplots of Visibility and Humidity for each month
plot2 = ggplot(wh, aes(Visibility..km., Humidity))  + facet_wrap(~month_name, nc = 3) + 
  geom_smooth() +ggtitle("Visibility and Humidity across months") + xlab("Visibility (km)") + 
  ylab("Humidity") + theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) +
  theme(text = element_text(size=12))

#GGplots of Temperature and Visibility for each month
plot3 = ggplot(wh, aes(Temperature..C., Visibility..km.))  + facet_wrap(~month_name, nc = 3) + 
  geom_smooth() + ggtitle("Temperature and Visibility across months") + xlab("Temperature (C)") +
  ylab("Visibility (km)") + theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) +
  theme(text = element_text(size=12))

#GGplots of Temperature and Wind Speed for each month
plot4 = ggplot(wh, aes(Temperature..C., Wind.Speed..km.h.))  + facet_wrap(~month_name, nc = 3) +
  geom_smooth() + ggtitle("Temperature and Wind Speed across months") + xlab("Temperature (C)") +
  ylab("Wind Spped (km/h)") + theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) +
  theme(text = element_text(size=12))

#Show ggplots
plot1
plot2 
plot3
plot4

#GGpairs of Temperature vs. Humidity
pair1 = ggpairs(data= wh, columns = c(4,6), title = "Temperature vs. Humidity") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) + 
  theme(text = element_text(size=12))

#GGpairs of Visibility vs. Humidity
pair2 = ggpairs(data= wh, columns = c(9,6), title = "Visibility vs. Humidity") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) + 
  theme(text = element_text(size=12))

#GGpairs of Temperature and Visibility
pair3 = ggpairs(data= wh, columns = c(4,9), title = "Temperature vs. Visibility") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) + 
  theme(text = element_text(size=12))

#GGpairs of Temperature vs. Wind speed
pair4 = ggpairs(data= wh, columns = c(4,7), title = "Temperature vs. Wind speed") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 13)) + 
  theme(text = element_text(size=12))


#Show ggpairs
pair1
pair2
pair3
pair4



#Predictive model
install.packages("moments")

library(moments)
qplot(Temperature..C., data  = na.omit(wh), binwidth = 0.3)
skewness(wh$Temperature..C.)

qplot(log(Temperature..C.+ 25), data = na.omit(wh), binwidth = 1/8)
log.temp = log(wh$Temperature..C. + 25)

month = as.factor(wh$Formatted.Date)

# Temperature predictive model with month factor
# First.
temp.lm1 = lm(log.temp ~  Apparent.Temperature..C. + Humidity + Wind.Speed..km.h. +
                Wind.Bearing..degrees. + Visibility..km. + Loud.Cover + Pressure..millibars.+ month +
                Apparent.Temperature..C.*month + Humidity * month + Wind.Speed..km.h. * month+
                Wind.Bearing..degrees. * month + 
                Visibility..km. * month + Pressure..millibars. * month)   




summary(temp.lm1)

# Second.
# drop Loud cover, wind.Bearing..degrees * month, pressure * month
temp.lm2 = lm(log.temp ~  Apparent.Temperature..C. + Humidity + Wind.Speed..km.h. +
                Wind.Bearing..degrees. + Visibility..km. + Pressure..millibars.+ month +
                Apparent.Temperature..C.*month + Humidity * month + Wind.Speed..km.h. * month+
                Visibility..km. * month)   


summary(temp.lm2)


# Third
# drop Apparent.Temperature..C., Apparent.Temperature..C. * month
temp.lm3 = lm(log.temp ~ Humidity + Wind.Speed..km.h. +
                Wind.Bearing..degrees. + Visibility..km. + Pressure..millibars.+ month +
                Humidity * month + Wind.Speed..km.h. * month+
                Visibility..km. * month)   


summary(temp.lm3)


# but it is still too complicated
# so we drop Wind.Bearing..degrees

temp.lm4 = lm(log.temp ~ Humidity + Wind.Speed..km.h. +
                Visibility..km. + month +
                Humidity * month + Wind.Speed..km.h. * month+
                Visibility..km. * month)   

summary(temp.lm4)


# Final
# drop wind Wind.Speed..km.h.
temp.lm5 = lm(log.temp ~ Humidity + 
                Visibility..km. + month +
                Humidity * month +
                Visibility..km. * month)   

summary(temp.lm5)

