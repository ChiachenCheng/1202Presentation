library(reshape2)
library(lubridate)
library(tseries)
library(MASS,stats)
library(car)

data <- read.csv("SP500.csv")
head(data)

Ret <- data$sprtrn
dates <- ymd(data$caldt)

plot(dates, Ret, type = "l", main = "data")
# plot(Y$date, Y$JPM, type = "l", main = "Compound returns for JP Morgan", 
#     ylab = "Returns", xlab = "Date", col = "red")

tseries::jarque.bera.test(Ret)

para = 20
q = acf(Ret,para)
plot(q[2:para])
q = acf(Ret^2,para)
plot(q[2:para])
b = Box.test(Ret,lag=para+1,type="Ljung-Box")
print(b)

para = 1000
q = acf(Ret,para)
plot(q[2:para])
q = acf(Ret^2,para)
plot(q[2:para])
b = Box.test(Ret,lag=para+1,type="Ljung-Box")
print(b)

qqPlot(Ret, distribution = "norm", envelope = FALSE)

# 2 degrees of freedom
qqPlot(Ret, distribution = "t", df = 2, envelope = FALSE,
      main = "2 Degrees of Freedom")

# 3 degrees of freedom
qqPlot(Ret, distribution = "t", df = 3, envelope = FALSE,
      main = "3 Degrees of Freedom")

# 4 degrees of freedom
qqPlot(Ret, distribution = "t", df = 4, envelope = FALSE,
      main = "4 Degrees of Freedom")

library(fGarch)

# p=get.hist.quote(instrument = "^gspc", start = "2005-01-01", end="2009-12-31",quote="AdjClose",quiet=T
# y=diff(log(p))*100
y = Ret
y=y-mean(y)
# garchFit(~garch(1,0), data = y, include.mean=FALSE)
# garchFit(~garch(4,0), data = y, include.mean=FALSE)
# garchFit(~garch(4,1), data = y, include.mean=FALSE)
# garchFit(~garch(1,1), data = y, include.mean=FALSE)
# garchFit(~garch(1,1), data = y, include.mean=FALSE, cond.dist="std",trace=F)
res=garchFit(~garch(1,1), data = y, include.mean=FALSE, cond.dist="sstd",trace=F)
res
plot(res)


