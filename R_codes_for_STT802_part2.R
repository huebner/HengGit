##############################
# Basic functions
sqr <- function(x) {x*x}
sqr(2)
sqr(c(2,4,6,8,10))

logbb <- function(x,b) log(x)/log(b)
logbb(8,2)

#########################
# Function using recursing
fact <- function(n)
{
  if (n<=1) 1
  else n*fact(n-1)
}
fact(3)

fact_1 <- function(n)
{
  prod(seq(1:n))
}
fact_1(3)

###################################
quad <- function(a, b, c) 
{
  x <- NA
  d <- b^2 - 4 * a * c
  if (d < 0) cat("*** Real roots do not exist! *** nn")
  else x <- (-b + c(-1, 1) * sqrt(d)) / (2 * a)
  list(Root1 = x[1], Root2 = x[2])
}
result <- quad(6, -5, 1)
names(result)

result$Root1
result$Root2

################################
num = 122
bits = num%%2
num = num%/%2
num # the quotient
bits # the remainder

bits = c(num%%2, bits)
num = num%/%2
num
bits

bits = c(num%%2, bits)
num = num%/%2
num
bits

bits = c(num%%2, bits)
num = num%/%2
num
bits

bits = c(num%%2, bits)
num = num%/%2
num # The quotient is zero now
bits

#################################
# For loop example
x = c(3, 4, 2, -1, 8, 9, 7)
cs = x[1]
for (i in 2:length(x)) {
  cs[i] = cs[i - 1] + x[i]
}
cs

################################
# While loop example
bits = 122%%2
num = 122%/%2
while (num > 0) {
  bits = c(num%%2, bits)
  num = num%/%2
}
bits


###############################
# repeat loop example

bits = 122%%2
num = 122%/%2
repeat {
  bits = c(num%%2, bits)
  num = num%/%2    
  if (num == 0) break
}
bits

#############################
# if
x = c(1, 8, -9, 3, 4, -7, 16, -8)
pos.sum = 0
for (i in 1:length(x)) {
  if (x[i] > 0) {
    pos.sum = pos.sum + x[i]
  }
  else {
    next
  }
}
pos.sum

sum(x[x>0])
##############################
# break
x = c(1, 8, -9, 3, 4, -7, 16, -8)
pos.sum = 0
for (i in 1:length(x)) {
  if (x[i] > 0) {
    pos.sum = pos.sum + x[i]
  }
  else {
    break
  }
}
pos.sum
i

first.neg = min(which(x <= 0))
if (first.neg == 1) {
  pos.sum = 0
} else {
  pos.sum = sum(x[1:(first.neg - 1)])
}
pos.sum
first.neg



###############################
# ifelse

a<-c(1,2,-3,4,-2)
b<-rep("b", length(a))
for (i in 1 : length(a))
{  
  if (a[i]>0)
  {b[i] <- "positive"}
  else
  {b[i] <- "negative"}
}


a<-c(1,2,-3,4,-2)
b <- ifelse(a>0, "positive", "negative")

##################################
# any(), all()
h <- c(15.1, 11.3, 7.0, 9.0) 

h>10 

h[1]>10 && h[2]>10 && h[3]>10 && h[4]>10

all(h>10)

h[1]>10||h[2]>10||h[3]>10||h[4]>10

any(h>10)




#################################
dec2bin = function(num) {
  bits = num%%2
  quot = num%/%2
  while (quot > 0) {
    bits = c(quot%%2, bits)
    quot = quot%/%2
  }
  return(bits)
}
dec2bin(122)

dec2bin(54)

dec2bin(100)

dec2bin(-16)

########################################
dec2bin = function(num) {
  if (num < 0) {
    return("You must specify a nonnegative integer")
  }
  bits = num%%2
  quot = num%/%2
  while (quot > 0) {
    bits = c(quot%%2, bits)
    quot = quot%/%2
  }
  return(bits)
}

dec2bin(122)

dec2bin(-16)

###########################################
# Babylonian method to solve square root

root = function(x, maxiter = 10) {
  rold = 0
  rnew = 1
  for (i in 1:maxiter) {
    if (rnew == rold)
      break
    rold = rnew
    rnew = 0.5 * (rnew + x/rnew)
  }
  rnew
}

###########################################
# Babylonian method to solve square root
# vectorized

root = function(x, maxiter = 10) {
  rold = 0
  rnew = 1
  for (i in 1:maxiter) {
    if (all(rnew == rold))
      break
    rold = rnew
    rnew = 0.5 * (rnew + x/rnew)
  }
  rnew
}
root(3)
root(c(2,3,4))

################################################
# Bisection algorithm
f <- function(x) { x^3 - 3 * x + 1}
x <- seq(from = 0, to = 1, length = 100)
plot(x = x, y = f(x), type="l")
abline(h = 0, lty = 3)

bisection <- function(fun, lower, upper, epx = 1e-03, epf =
                        1e-03)
{
  ## Make sure that the lower bound is smaller than 
  ## the upper bound. If not, switch lower and upper
  if (lower > upper) 
  {
    x1 <- lower
    lower <- upper
    upper <- x1
  }
  x0 <- lower
  x1 <- upper
  
  ## Make sure that the interval contains the root. 
  ## If not, report an error
  if (fun(x0) * fun(x1) >= 0)
  {cat("No root in interval. Come back with better brackets")}
  else {
    repeat {
      x2 <- (x0 + x1) / 2
      if (fun(x2) * fun(x0) < 0) {
        x1 <- x2
      }
      else x0 <- x2
      if ((abs(x1 - x0) < epx) | (abs(fun(x2)) < epf))  break
    }
    
    x2
  }
}

bisection(f, 0, 1)

#########################################
# Fixed point iteration
fixed.fun<-function(g, x0, nlim, eps)
{
  iterno <- 0
  repeat {
    iterno <- iterno + 1
    if (iterno > nlim) {
      cat("Iteration Limit Exceeded: Current = ",iterno, fill = T)
      x1 <- NA
      break
    }
    else
    {
      x1 <- g(x0)
      cat("****Iter. No: ", iterno, " Current Iterate = ", x1, fill= T)
      
      if (abs(x1 - x0) < eps || abs(x1 - g(x1)) < 1e-10)
        break
      
      x0 <- x1
    }
  }
  return(x1)
}


g<-function(x)
{return((x^3 + 1)/3)}

fixed.fun(g, 0.4, 100, 1e-8)

#############################################################
# Basic plot

GNP = c(16.8, 21.3, 18.7, 5.9, 11.4, 17.8, 10.9, 16.6, 21, 16.4, 7.8, 14)
GNP

AGRIC = c(2.7, 5.7, 3.5, 22.2, 10.9, 6, 14, 8.5, 3.5, 4.3, 17.4, 2.3)
AGRIC

countries = c("BE", "DK", "DE", "GR", "ES", "FR", "IE", "IT", "LU", "NL", "PT", "UK")

twogroups = c(2, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2)
fac.twogroups = factor(twogroups) 

euro.data = data.frame(countries = countries, percapGNP = GNP, percAGRIC = AGRIC, grouping = fac.twogroups)

plot(euro.data$percAGRIC, euro.data$percapGNP)

plot(euro.data$grouping, euro.data$percapGNP)

boxplot(euro.data$percapGNP ~ euro.data$grouping)

summary(euro.data$percapGNP)


#######################################################33
illit <- state.x77[ ,3]
murder <- state.x77[, 5]
plot(illit, murder)

lmout <- lm(murder ~ illit)
plot(lmout)

############################################
# High level plot

AGRIC = euro.data$percAGRIC
GNP = euro.data$percapGNP
plot(AGRIC, GNP)

plot(AGRIC, GNP, xlab = "percent employed in agriculture", ylab = "per capita GNP", xlim = c(0, 30), ylim = c(0,30), pch = 15, col = "blue", bty = "L", cex = 1.5)

plot(AGRIC, GNP, xlab = "percent employed in agriculture", ylab = "per capita GNP", xlim = c(0, 30), ylim = c(0,30), pch = 15, col = "blue", bty = "n", cex = 1.5)

plot(AGRIC, GNP, xlab = "percent employed in agriculture", ylab = "per capita GNP", xlim = c(0, 30), ylim = c(0,30), pch = 15, col = "blue", bty = "o", cex = 1.5)

#################################################
# brain body weight
brainbody = read.table("http://www.stt.msu.edu/~melfi/cstat/brainbody.txt", header = T)

plot(brainbody$BodyWeight, brainbody$BrainWeight)

plot(log(brainbody$BodyWeight), log(brainbody$BrainWeight),xlab = "Body Weight", ylab = "Brain Weight",  bty = "n", pch = 2, col = "red", main = "Log transformed brain and body weights")

Animal_ind<-seq(1:nrow(brainbody))

text(x = log(brainbody$BodyWeight)+0.4, y = log(brainbody$BrainWeight+0.3), Animal_ind)

#########################################################
#

par(mfrow = c(2, 2))
tmpx = seq(-3, 3, 0.01)
plot(tmpx, dnorm(tmpx, 0, 1), type = "l", xlab = "x", ylab = "density at x", main = "standard normal density")
rm(tmpx)
tmpx = 0:20
plot(tmpx, dbinom(tmpx, 20, 0.4), type = "h", xlab = "x", ylab = "prob of x", main = "binomial probabilities")
plot(tmpx, pbinom(tmpx, 20, 0.4), type = "s", xlab = "x", ylab = "cdf at x", main = "binomial cdf")
rm(tmpx)
tmpx = 1:15
tmpy = rnorm(15, 5, 1)
plot(tmpx, tmpy, type = "b", main = "connect the dots")
rm(tmpx)
rm(tmpy)
par(mfrow = c(1, 1))


#####################################################
#
x<-1
y<-1
par(mfrow = c(2, 3))
for (i in 1:6)
{
  plot(x, y, xlim=c(-1, 1), ylim=c(-1, 1), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
  text(0, 0, i)
}
par(mfrow = c(1, 1))


x<-1
y<-1
par(mfcol = c(2, 3))
for (i in 1:6)
{
  plot(x, y, xlim=c(-1, 1), ylim=c(-1, 1), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
  text(0, 0, i)
}
par(mfcol = c(1, 1))

#########################################################
# layout

layout(matrix(1:6, nrow = 3, ncol = 2))
layout.show(6)
for (i in 1:6)
{
  plot(x, y, xlim=c(-1, 1), ylim=c(-1, 1), type="n", xaxt="n", yaxt="n", xlab="", ylab="")
  text(0, 0, i)
}

layout(matrix(1:6, nrow = 3, ncol = 2, byrow = T))
layout.show(6)

layout(matrix(1:6, nrow = 3, ncol = 2, byrow = T), heights = c(3, 2, 1))
layout.show(6)

mm = matrix(c(0, 0, 3, 3, 3, 1, 1, 3, 3, 3, 0, 0, 3, 3, 3, 0, 2, 2, 0, 5, 4, 2, 2, 0, 5), nrow = 5, ncol = 5)
layout(mm)
layout.show(5)


########################################################
# save plot to a file 

jpeg('rplot.jpg')
plot(seq(1:100), rnorm(100,0,1))
dev.off()

pdf('rplot.pdf')
plot(seq(1:100), rnorm(100,0,1))
dev.off()

###########################################################
# 3d perspective plot
x <- seq(-10, 10, length = 30)
y <- x
f <- function(x, y) {
  r <- sqrt(x^2 + y^2)
  10 * sin(r)/r
}
z <- outer(x, y, f)
z[is.na(z)] <- 1
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col="blue", shade=0.1)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col=rainbow(10), shade=0.1)


###########################################################
# A pie chart
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry", "Apple","Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) 


pie(pie.sales, col = c("purple", "violetred1", "green3",
                       "cornsilk", "cyan", "white"))

pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))


n <- 200
pie(rep(1,n), labels="", col=rainbow(n), border=NA)


##############################################################
# 
1 - pbinom(10, 20, 0.25)

# plot the power function
tmpp = seq(0, 1, 0.01)
tmppower = 1 - pbinom(10, 20, tmpp)
plot(tmpp, tmppower, type = "l", xlab = "p", ylab = "power")

tmpp = seq(0, 1, 0.01)
tmppower = 1 - pbinom(8, 20, tmpp)
points(tmpp, tmppower, type = "l", lty = "dashed")

points(0.25, 1 - pbinom(10, 20, 0.25), pch = 20)
points(0.25, 1 - pbinom(8, 20, 0.25), pch = 20)
legend(0.6, 0.4, legend = c("k=10", "k=8"), lty = c("solid", "dashed"))
title("Power function for two values of k")

# Change the thickness of the line
tmpp = seq(0, 1, 0.01)
tmppower = 1 - pbinom(10, 20, tmpp)
plot(tmpp, tmppower, type = "l", lwd = 15, 
     xlab = "p", ylab = "power")

############################################################
# Adding text to a plot
GNP = c(16.8, 21.3, 18.7, 5.9, 11.4, 17.8, 10.9, 16.6, 21, 16.4, 7.8, 14)
GNP

AGRIC = c(2.7, 5.7, 3.5, 22.2, 10.9, 6, 14, 8.5, 3.5, 4.3, 17.4, 2.3)
AGRIC

countries = c("BE", "DK", "DE", "GR", "ES", "FR", "IE", "IT", "LU", "NL", "PT", "UK")

twogroups = c(2, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2)
fac.twogroups = factor(twogroups) 

euro.data = data.frame(countries = countries, percapGNP = GNP, percAGRIC = AGRIC, grouping = fac.twogroups)

plot(AGRIC, GNP, type = "n")
text(AGRIC, GNP, labels = countries)

plot(AGRIC, GNP)
text(AGRIC, GNP, labels = countries, pos = 1, cex = 0.75)

plot(AGRIC, GNP, xlim = c(0, 25), ylim = c(0, 25), xaxt = "n", yaxt = "n", type = "n", ann = F)
points(AGRIC, GNP, pch = 22)
axis(side = 1, c(5, 12.5, 20), labels = FALSE)
axis(side = 2, c(5, 12.5, 20), labels = FALSE)

plot(AGRIC, GNP, xlim = c(0, 25), ylim = c(0, 25), xaxt = "n", yaxt = "n", 
     type = "n", xlab="", ylab="", bty="n")
points(AGRIC, GNP, pch = 22)
axis(side = 1, c(5, 12.5, 20), labels = FALSE)
axis(side = 2, c(5, 12.5, 20), labels = FALSE)

mtext("percent employed in agriculture", side = 1, line = 2)
mtext("per capita GNP", side = 2, line = 2)

mtext(c(5, 12.5, 20), side = 1, at = c(5, 12.5, 20), line = 0.5)
mtext(c(5, 12.5, 20), side = 2, at = c(5, 12.5, 20), line = 0.5)

#################################################
# Adding formulas in R plot
xlabel <- expression(paste(Delta, italic(s), sep = ""))
ylabel <- expression(alpha[1] * " in (kg)"^2)
plotname <- expression(sin * (beta))
plot(rnorm(50), rnorm(50), xlab = xlabel, ylab = ylabel, main = plotname, xlim = c(-pi, pi), ylim = c(-3, 3), axes = FALSE)
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi), labels = expression(-pi, -pi/2, 0, pi/2, pi))
axis(2)
box()
text(-pi/2, -2, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(pi/2, 2, expression(paste(frac(1, sigma*sqrt(2*pi)), exp*(frac(-(x-mu)^2, 2*sigma^2)), sep = "")), cex = 1.5)

############################################
#abline and arrow
par(mfrow = c(2, 2))
plot(AGRIC, GNP)
euro.lm = lm(GNP ~ AGRIC)
abline(euro.lm)
plot(AGRIC, GNP)
abline(h = mean(GNP))
abline(v = mean(AGRIC), lty = "dashed")

plot(AGRIC, GNP)
arrows(AGRIC[5], GNP[5], AGRIC[1], GNP[1], code = 1)
plot(AGRIC, GNP)
arrows(AGRIC[5], GNP[5], AGRIC[1], GNP[1], code = 2)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(AGRIC, GNP)
arrows(AGRIC[5], GNP[5], AGRIC[1], GNP[1], code = 3)
plot(AGRIC, GNP)
segments(AGRIC[5], GNP[5], AGRIC[1], GNP[1])
par(mfrow = c(1, 1))

#############################################
# Axes and marginal text, long jump data

lj.men <- c(8.21, 8.24, 8.28, 8.31, 8.31, 8.34, 8.35, 8.35, 8.90, 8.95)
lj.women <- c(6.40, 6.42, 6.48, 6.53, 6.70, 6.76, 6.82, 6.84, 6.92, 6.99, 7.07, 7.09, 7.20, 7.21, 7.43, 7.44, 7.45, 7.45, 7.45, 7.52)

par(mar=c(5, 4, 4, 4)) # change the width of the margin
plot(rep(1, length(lj.women)), lj.women, xlim = c(0,2.5), ylim = c(6, 9), ann = FALSE, axes = FALSE)

# This is the same as following
plot(rep(1, length(lj.women)), lj.women, xlim = c(0,3), ylim = c(6, 9), ann = FALSE, xaxt = "n", yaxt="n", bty="n")

points(rep(2, length(lj.men)), lj.men)
axis(1, at = c(1, 2), labels = c("women", "men"))
axis(2, at = c(6, 7, 8, 9), labels = c(6, 7, 8, 9))
axis(4, at = c(6, 7, 8, 9), labels = round(c(6, 7, 8, 9) * 3.28, 1))
mtext("long jump in meters", side = 2, line = 2)
mtext("long jump in feet", side = 4, line = 2)
title("Long jump world records")
box()
par(mar=c(5, 4, 4, 2))

################################################
# Old Faithful geyser
data(faithful) 
faithful[1:10, ]

hist(faithful$waiting, xlab = "waiting time to next eruption", col = "gray80", main = "Histogram of waiting times")
rug(faithful$waiting)

wait.hist = hist(faithful$waiting, plot = F)
wait.hist
xx = c(wait.hist$breaks[1], wait.hist$mids, wait.hist$breaks[13])

yy = c(0, wait.hist$counts, 0)
hist(faithful$waiting, xlab = "waiting time to next eruption", col = "gray80", 
     main = "Histogram with frequency polygon")
lines(xx, yy, lwd = 3)


#############################################
# Black cherry tree data
data(trees)
trees[1:10, ]
symbols(trees$Height, trees$Volume, circles = trees$Girth/16, inches = FALSE, xlab = "Height", ylab = "Volume")
title("Radius represents girth of tree")

###################################################################
rmt <- matrix(rexp(1000 * 32), nrow=32)
mns <- cbind(rmt[1,], apply(rmt[1:4,], 2, mean), apply(rmt[1:32,], 2, mean))
meds <- cbind(rmt[1,], apply(rmt[1:4,], 2, median), apply(rmt[1:32,], 2, median))

hist(mns[,1])
hist(mns[,2], main = "Means of samples of size 4", xlab="size 4 means", las=3) # set the axis label style
hist(mns[,3], main = "Means of samples of size 32", xlab="size 32 means", las=1, col="blue", prob = TRUE)
lines(density(mns[,3]), col="red")


library(lattice)
histogram(~mns | ssz, data = data.frame(mns = c(mns), ssz = gl(3, 1000), labels=c("1", "4", "32")), layout=c(3,1), main="Histograms of means by sample size")

alldat <- data.frame(sim = c(mns, meds), 
                     ssz = gl(3, 1000, len=6000, labels=c("1", "4", "32")),
                     type = gl(2, 3000, labels=c("Mean", "Median")))
str(alldat)

histogram(~ sim | ssz * type, data = alldat, 
          layout = c(3,2), 
          main = "Histograms of means and medians by sample size")

###################################################
# boxplot
bwplot(ssz ~ sim | type, data = alldat, main = "Boxplots of means and medians by sample size")

#######################################################
# scatter plot
data(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1, main = "cars data")
lines(lowess(cars$speed, cars$dist, f = 2/3, iter = 3), col = "red")


xyplot(dist ~ speed, data = cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
       panel = function(x, y){panel.xyplot(x,y); panel.loess(x,y)})

xyplot(dist ~ speed, data = cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)")

xyplot(dist ~ speed, data = cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
       panel = function(x, y){panel.xyplot(x,y);panel.loess(x,y)})

library(nlme)
data(Oxboys)
xyplot(height ~ age | Subject, data = Oxboys, ylab = "Height (cm)")

xyplot(height ~ age | Subject, data = Oxboys, ylab = "Height (cm)",
       aspect = "xy", # calculate an optimal aspect ratio
       panel = function(x,y){panel.grid(); panel.xyplot(x,y)})

xyplot(height ~ age | Subject, data = Oxboys, ylab = "Height (cm)",
       aspect = 3/4, # calculate an optimal aspect ratio
       panel = function(x,y){panel.grid(); panel.xyplot(x,y)})

####################################################################
# scatter plot matrices
data(USArrests)
USArrests[1:10,]
pairs(USArrests)

splom(~ USArrests)
splom(~ USArrests, panel = function(x,y){panel.xyplot(x,y); panel.loess(x,y)})

##########################################################
# Import / export data set into R
file.choose() 

cat("TITLE extra line", "2 3 5 7", "11 13 17", file="ex.txt", sep="\n")
scan("ex.txt", skip = 1)
scan("ex.txt")


A<-matrix(c(1, 2, 3, 4, 5, 6), nrow=2)
write(A, "A.txt", ncolumns=3)
write.table(A, "A.txt")


rownames(A)<-c("the 1st row", "the 2nd row")
colnames(A)<-c("a", "b", "c")
write.table(A, "A.txt", row.names = FALSE, col.names = FALSE)
write.table(A, "A.txt", row.names = TRUE, col.names = TRUE)
AA<-read.table("A.txt", header=T, col.names=c("a", "b", "c"))

is.numeric(AA)
is.matrix(AA)
class(AA)
BB <- as.matrix(AA)

# RData
x <- runif(20)
y <- list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.RData")

# .dat 
smoothing_data<-read.table("smoothing_data.txt.txt", header=T)
write.table(smoothing_data, file="smoothing_data.dat")
smoothing_data_2<-read.table("smoothing_data.dat", header=T)



# web page
PET <- matrix(scan(file = "fbp-img.txt"),ncol = 128, nrow = 128, byrow = T)
read.table(file = "smoothing_data_1.txt")
matrix(scan(file = "smoothing_data_1.txt"), nrow=2, byrow=T)
scan(file = "ex.txt", skip=1)

# Excel
# xls.getshlib()
library(xlsReadWrite)
An_A<-read.xls("An_excel_file.xls")
write.xls(An_A, "An_excel_file_2.xls")

library(xlsx)
An_B<-read.xlsx("Another_excel_file.xlsx",sheetIndex=1)
write.xlsx(An_B, file="Another_excel_file_2.xlsx", col.names=TRUE, row.names=FALSE)

smoothing_data<-read.table("smoothing_data.txt.txt", header=T)
write.table(smoothing_data, file="smoothing_data.dat")
smoothing_data_2<-read.table("smoothing_data.dat", header=T)


