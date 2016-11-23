# timeseriesinr#time series examples#

jj = scan ("C:/Users/steve/Documents/R/tsdata/mydata/jj.dat")
jj= ts(jj,start=1960,frequency=4)
plot(jj,    ylab="Quarterly Earnings Per share")

#mri example#
par(mfrow=c(2,1))
ts.plot(fmri[,2:5], lty=c(1,4), ylab="BOLD")
ts.plot(fmri[,6:9],  lty=c(1,4), ylab="Bold")

x= matrix ( scan ("C:/Users/steve/Documents/R/tsdata/mydata/eq5exp6.dat"), ncol=2)
par(mfrow=c(2,1))
plot.ts(x[,1], main="Earthquake", ylab="EQ5")
plot.ts(x[,2], main="Explosion", ylab="EXP6")

w=rnorm(500,0,1)
v=filter(w,sides=2, rep(1,3)/3) #moving averages
par(mfrow=c(2,1))
plot.ts(w)
plot.ts(v)


w=rnorm(550,0,1)
x=filter(w, filter=c(1,-.9), method="recursive")
plot.ts(x[51:550])

set.seed(154)
w = rnorm(200,0,1); x=cumsum(w)
wd=w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55))
lines(x)
lines(.2*(1:200), lty="dashed")

install.packages("blscrapeR")
library(blscrapeR)
library(ggplot2)
df <- bls_api(c("LNS13327709", "LNS14000000"),
              startyear = 2000, endyear = 2015)
tail(df)
ggplot(data=df, aes(x = date, y = value, color=seriesID)) + 
  geom_line() +
  labs(title = "US Unemployment") +
  theme(legend.position="top") +
  scale_color_discrete(breaks=c("LNS13327709", "LNS14000000"),
                       labels=c("U-6", "U-3"))

library(blscrapeR)
library(ggplot2)
df <- bls_api(c("ENU0608510010", "ENU1209510010"),
              startyear = 2005, endyear = 2015)
tail(df)


ggplot(df, aes(x=date, y=value, color=seriesID)) +
  geom_line() +
  labs(title = "Total Employed by County") +
  theme(legend.position="top") +
  scale_color_discrete(name="Activity",
                       breaks=c("ENU0608510010", "ENU1209510010"),
                       labels=c("Santa Clara, CA", "Orange, FL"))

install.packages("astsa")  # install it ... I'm using version 1.5 below
library(astsa)             # then load it (has to be done at the start of each session)
data()  # use this command to view all the loaded data 
jj
plot(jj, ylab="Earnings per Share", main="J & J")
plot(jj, type="o", col="blue", lty="dashed")
plot(diff(log(jj)), main="logged and diffed") 


x = 5:5                  # sequence of integers from 5 to 5
y = 5*cos(x)              # guess
par(mfrow=c(3,2))         # multifigure setup: 3 rows, 2 cols
#  plot:
plot(x, main="plot(x)")
plot(x, y, main="plot(x,y)")
#  plot.ts:
plot.ts(x, main="plot.ts(x)")
plot.ts(x, y, main="plot.ts(x,y)")
#  ts.plot:
ts.plot(x, main="ts.plot(x)")
ts.plot(ts(x), ts(y), col=1:2, main="ts.plot(x,y)")  # note x and y are ts objects 
#  the help files [? and help() are the same]:
?plot.ts
help(ts.plot)
?par        # might as well skim the graphical parameters help file while you're here

k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))          
fjj = filter(jj, sides=2, k)  # ?filter for help [but you knew that already]
plot(jj) 
lines(fjj, col="red")         # adds a line to the existing plot 
lines(lowess(jj), col="blue", lty="dashed")


dljj = diff(log(jj))        # difference the logged data
plot(dljj)                  # plot it (not shown)
shapiro.test(dljj)          # test for normality  

par(mfrow=c(2,1))        # set up the graphics 
hist(dljj, prob=TRUE, 12)   # histogram     
lines(density(dljj))     # smooth it  ?density for details 
qqnorm(dljj)             # normal QQ plot  
qqline(dljj)             # add a line  

lag1.plot(dljj, 4)  # this is the astsa version of lag.plot in the stats package
acf2(dljj)   # astsa gives both in one swell foop ... or use acf() and pacf() individually
plot(dog < stl(log(jj), "per"))

#time series examples#

jj = scan ("C:/Users/steve/Documents/R/tsdata/mydata/jj.dat")
jj= ts(jj,start=1960,frequency=4)
plot(jj,    ylab="Quarterly Earnings Per share")

#mri example#
par(mfrow=c(2,1))
ts.plot(fmri[,2:5], lty=c(1,4), ylab="BOLD")
ts.plot(fmri[,6:9],  lty=c(1,4), ylab="Bold")

x= matrix ( scan ("C:/Users/steve/Documents/R/tsdata/mydata/eq5exp6.dat"), ncol=2)
par(mfrow=c(2,1))
plot.ts(x[,1], main="Earthquake", ylab="EQ5")
plot.ts(x[,2], main="Explosion", ylab="EXP6")

w=rnorm(500,0,1)
v=filter(w,sides=2, rep(1,3)/3) #moving averages
par(mfrow=c(2,1))
plot.ts(w)
plot.ts(v)


w=rnorm(550,0,1)
x=filter(w, filter=c(1,-.9), method="recursive")
plot.ts(x[51:550])

set.seed(154)
w = rnorm(200,0,1); x=cumsum(w)
wd=w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55))
lines(x)
lines(.2*(1:200), lty="dashed")

install.packages("blscrapeR")
library(blscrapeR)
library(ggplot2)
df <- bls_api(c("LNS13327709", "LNS14000000"),
              startyear = 2000, endyear = 2015)
tail(df)
ggplot(data=df, aes(x = date, y = value, color=seriesID)) + 
  geom_line() +
  labs(title = "US Unemployment") +
  theme(legend.position="top") +
  scale_color_discrete(breaks=c("LNS13327709", "LNS14000000"),
                       labels=c("U-6", "U-3"))

library(blscrapeR)
library(ggplot2)
df <- bls_api(c("ENU0608510010", "ENU1209510010"),
              startyear = 2005, endyear = 2015)
tail(df)


ggplot(df, aes(x=date, y=value, color=seriesID)) +
  geom_line() +
  labs(title = "Total Employed by County") +
  theme(legend.position="top") +
  scale_color_discrete(name="Activity",
                       breaks=c("ENU0608510010", "ENU1209510010"),
                       labels=c("Santa Clara, CA", "Orange, FL"))

install.packages("astsa")  # install it ... I'm using version 1.5 below
library(astsa)             # then load it (has to be done at the start of each session)
data()  # use this command to view all the loaded data 
jj
plot(jj, ylab="Earnings per Share", main="J & J")
plot(jj, type="o", col="blue", lty="dashed")
plot(diff(log(jj)), main="logged and diffed") 


x = 5:5                  # sequence of integers from 5 to 5
y = 5*cos(x)              # guess
par(mfrow=c(3,2))         # multifigure setup: 3 rows, 2 cols
#  plot:
plot(x, main="plot(x)")
plot(x, y, main="plot(x,y)")
#  plot.ts:
plot.ts(x, main="plot.ts(x)")
plot.ts(x, y, main="plot.ts(x,y)")
#  ts.plot:
ts.plot(x, main="ts.plot(x)")
ts.plot(ts(x), ts(y), col=1:2, main="ts.plot(x,y)")  # note x and y are ts objects 
#  the help files [? and help() are the same]:
?plot.ts
help(ts.plot)
?par        # might as well skim the graphical parameters help file while you're here

k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))          
fjj = filter(jj, sides=2, k)  # ?filter for help [but you knew that already]
plot(jj) 
lines(fjj, col="red")         # adds a line to the existing plot 
lines(lowess(jj), col="blue", lty="dashed")


dljj = diff(log(jj))        # difference the logged data
plot(dljj)                  # plot it (not shown)
shapiro.test(dljj)          # test for normality  

par(mfrow=c(2,1))        # set up the graphics 
hist(dljj, prob=TRUE, 12)   # histogram     
lines(density(dljj))     # smooth it  ?density for details 
qqnorm(dljj)             # normal QQ plot  
qqline(dljj)             # add a line  

lag1.plot(dljj, 4)  # this is the astsa version of lag.plot in the stats package
acf2(dljj)   # astsa gives both in one swell foop ... or use acf() and pacf() individually
plot(dog < stl(log(jj), "per"))
