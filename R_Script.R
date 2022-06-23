#Time Series Project

my_data <- read.csv('/Users/namithamariacherian/Projects_R/Energy_Production/opsd_germany_daily.csv', header= TRUE, row.names = 'Date')
head(my_data)
tail(my_data)
View(my_data)
dim(my_data)
str(my_data)
row.names(my_data)
my_data['2008-09-12',]
my_data['2006-07-11',]
my_data[c('2009-02-11','2008-02-02'),]
summary(my_data)

mydata2 <- read.csv('/Users/namithamariacherian/Projects_R/Energy_Production/opsd_germany_daily.csv', header= TRUE)
head(mydata2)
str(mydata2)
str(mydata2$Date)

x <- as.Date(mydata2$Date)
class(x)
str(x)

year <- as.numeric(format(x,'%Y'))
head(year)
class(year)

month <- as.numeric(format(x,'%m'))
head(month)

day <- as.numeric(format(x,'%d'))
head(day)

head(mydata2)
mydata2 <- cbind(mydata2, year, month, day)
head(mydata2)


library(dplyr)
sample_n(mydata2,size=8)

plot(mydata2$year, mydata2$Consumption, type='l', 
     xlab='year', ylab='Power Consumption',
     xlim=c(2006,2018), ylim = c(900,1400))

par(mfrow=c(1,1))
plot(mydata2[,2], ylab='consumption', type='l', lwd=2,col='blue',
     ylim=c(900,2000), main='Consumtion Graph')

library(ggplot2)
ggplot(data=mydata2, mapping = aes(x=year, y=Consumption, col='red')) +geom_line(linetype='dashed')


head(mydata2)

min(mydata2[,3],na.rm=T)
max(mydata2[,3],na.rm=T)

min(mydata2[,4],na.rm=T)
max(mydata2[,4],na.rm=T)

min(mydata2[,5],na.rm=T)
max(mydata2[,5],na.rm=T)

x <- as.Date(mydata2$Date)
moddate <- as.Date(x, format='%m/%d%Y')
mydata3 <- cbind(moddate,mydata2)
head(mydata3)


par(mfrow= c(3,1))
plot1 <- plot(mydata3[,1],mydata3[,3], xlab='Date', ylab='Consumption',col='red', type='l', lwd='1', main='Consumtion in Time', ylim=c(850,1750))
plot2 <- plot(mydata3[,1],mydata3[,4], xlab='Date', ylab='Wind Prod',col='orange', type='l', lwd='1', main='Wind Energy Production in Time', ylim=c(0,1900))
plot3 <- plot(mydata3[,1],mydata3[,5], xlab='Date', ylab='Solar Prod',col='blue', type='l', lwd='1', main='Solar Energy Production in Time', ylim=c(0,300))


#Plotting for 1 year for further investigation

mydata4 <- subset(mydata3, subset= mydata3$moddate >= '2017-01-01' & mydata3$moddate <= '2017-12-31')
head(mydata4)

par(mfrow=c(1,1))
plot4 <- plot(mydata4[,1], mydata4[,3], xlab='Date', ylab='Consumption', type='l', lwd=1, col='green')

#Zooming in more

mydata4 <- subset(mydata3, subset= mydata3$moddate >= '2017-01-01' & mydata3$moddate <= '2017-02-28')
head(mydata4)

xmin <- min(mydata4[,1])
xmax <- max(mydata4[,1])
ymin <- min(mydata4[,3])
ymax <- max(mydata4[,3])

xmin
xmax
ymin
ymax


plot4 <- plot(mydata4[,1], mydata4[,3], xlab='Date', ylab='Consumption', type='l', lwd=1, col='green', xlim=c(xmin,xmax),ylim=c(ymin,ymax))
grid()
abline(h=c(1300,1400,1500,1600))
abline(v=seq(xmin,xmax,7),lty=2,col='blue')


#Exploring Seasonality
boxplot(mydata3$Consumption)
boxplot(mydata3$Solar)
boxplot(mydata3$Wind)

quantile(mydata3$Consumption, probs = c(0,0.25,0.5,0.75,1))

boxplot(mydata3$Consumption, ylim=c(700,1800), ylab='Consumption', main='Consumption Boxplot')

#grouping by year
boxplot(mydata3$Consumption ~ mydata3$year, ylim=c(700,1800), xlab ='years' , ylab='Consumption', main='Consumption Boxplot', las=1)

#grouping by months
boxplot(mydata3$Consumption ~ mydata3$month, ylim=c(700,1800), xlab ='months' , ylab='Consumption', main='Consumption Boxplot', las=1)


par(mfrow=c(3,1))
boxplot(mydata3$Consumption ~ mydata3$month, ylim=c(700,1800), xlab ='Months' , ylab='Power Consumption', main='Power Consumption by month', las=1, col='blue')
boxplot(mydata3$Wind ~ mydata3$month, ylim=c(0,1000), xlab ='Months' , ylab='Wind Production', main='Wind Production by month', las=1, col='green')
boxplot(mydata3$Solar ~ mydata3$month, ylim=c(0,500), xlab ='Months' , ylab='Solar Production', main='Solar Production by month', las=1, col='yellow')



weekday <- weekdays(mydata3$moddate) 
weekday

mydata5 <- cbind(mydata3,weekday)
head(mydata5)

par(mfrow=c(1,1))
boxplot(mydata5$Consumption ~ mydata5$weekday, ylim=c(700,1800), xlab ='Day' , ylab='Power Consumption', main='Power Consumption by day of week', las=1, col='blue')


#Exploring
mydata3
colSums(!is.na(mydata3))
sum(is.na(mydata3$Consumption))
sum(is.na(mydata3$Wind))
sum(is.na(mydata3$Solar))
sum(is.na(mydata3$Wind.Solar))

#Checking frequency of data

xmin <- min(mydata3[,1], na.rm=T)
xmin        
freq1 <- seq(from=xmin, by='day', length.out=10)
freq1
freq2 <- seq(from=xmin, by='month', length.out=10)
freq2
freq3 <- seq(from=xmin, by='year', length.out=10)
freq3

#Selecting row values with NA for Wind

head(mydata3)

selwind1 <- mydata3[which(is.na(mydata3$Wind)), c("moddate","Consumption","Wind","Solar")]
head(selwind1)
View(selwind1)


selwind2 <- mydata3[which(!is.na(mydata3$Wind)), c("moddate","Consumption","Wind","Solar")]
head(selwind2)
View(selwind2)

#finding NA values for wind
sum(is.na(mydata3$Wind[mydata3$year=='2011']))
sum(!is.na(mydata3$Wind[mydata3$year=='2011']))

selwind3 <- mydata3[which(mydata3$year=='2011'), c("moddate","Consumption","Wind","Solar")]
head(selwind3)
str(selwind3)

selwind4 <- selwind3[which(is.na(selwind3$Wind)),]
selwind4

#more investigation
test1 <- subset(selwind3, subset=(selwind3$moddate > '2011-12-12' & selwind3$moddate < '2011-12-16'))
test1

library(tidyr)
fill(test1,Wind)

install.packages('zoo')
library(zoo)
test_3day <- zoo :: rollmean(mydata3$Consumption, k=3, fill=NA)
head(test_3day)

library(dplyr)

test_three <- mydata3 %>% 
  dplyr::arrange(desc(year)) %>% 
  dplyr::group_by(year) 

test_three_new <- cbind(test_three, test_3day)
head(test_three_new)

test_three_new$...11
  
ungroup(mydata3)
head(mydata3)

mydataTest <- mydata3 %>%
  dplyr::arrange(desc(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(test_7day = zoo::rollmean(Consumption, k=7, fill=NA),
                test_365day = zoo::rollmean(Consumption, k=365, fill=NA)) %>%
  dplyr::ungroup()

head(mydataTest)

mydataTest2 <- mydataTest %>%arrange(desc(year)) %>%select(Consumption, moddate, test_7day, test_365day)
head(mydataTest2)

par(mfrow=c(1,1))
plot(mydataTest2$moddate,mydataTest2$Consumption, xlab='Date',ylab='Consumption', main='Consumption over time', col='blue', type='l', lwd=1)
points(mydataTest2$moddate, mydataTest2$test_7day, col='orange', type='l', lwd=1)
points(mydataTest2$moddate, mydataTest2$test_365day, col='green', type='l',lwd=5)
legend(2016, 1600, legend=c('Everyday Consumption', '7 day rolling mean', '365 day rolling mean'), col=c('blue','orange','green'),pch = c('o','*','+') ,lty=c(1,2,3), ncol=1)
