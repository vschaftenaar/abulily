\name{plt}
\alias{plt}
\title{abulily plot}
}
\description{
Bar plot and/or 2 lines
}\examples{
#create data
x=c(2010:2021)
bar=5000*cumprod(rnorm(12,.025,.05)+1)
line.1=100*cumprod(rnorm(12,.025,.05)+1)
line.2=100*cumprod(rnorm(12,.025,.05)+1)

#plot both the bar and the lines
plt(
  x         = x,
  bar       = bar,
  line.1    = line.1,
  line.2    = line.2,
  line.type = c('p','b'),
  line.lty  = c(3,3),
  line.lwd  = c(1,1),
  main      = 'Abulily plot',
  line.cex  = c(1,1))

#plot only one lines
plt(
  x         = x,
  line.1    = line.1,
  line.type = 'b',
  line.lty  = 1,
  line.lwd  = 1.5,
  main      = 'Abulily plot')
}
