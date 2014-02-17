##########################
0.45 == 3*0.15

0.45 - 3*0.15
###########################
# Test data types
typeof(10)

is.numeric(10)

is.integer(10)

is.na(10)

is.character(10)

is.character("10")

is.character("ten")

is.na("ten")

as.integer(10)
###########################
# Coerce data types
as.character(3)

as.numeric(as.character(3))

3*as.character(3)

3*as.numeric(as.character(3))

as.character(3) > 1

as.character(3) > "A"
##########################
# Data vectors
GNP = c(16.8, 21.3, 18.7, 5.9, 11.4, 17.8, 10.9, 16.6, 21, 16.4, 7.8, 14)
GNP

AGRIC = c(2.7, 5.7, 3.5, 22.2, 10.9, 6, 14, 8.5, 3.5, 4.3, 17.4, 2.3)
AGRIC

countries = c("BE", "DK", "DE", "GR", "ES", "FR", "IE", "IT", "LU", "NL", "PT", "UK")

twogroups = c(2, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2)
fac.twogroups = factor(twogroups) 

plot(AGRIC, GNP, type="n")
text(AGRIC, GNP, countries)
text(AGRIC-0.2, GNP-0.4, twogroups, col=2)

mean(twogroups)
mean(fac.twogroups)

##########################
# seq() and rep() 
seq(from = 1, to = 7, by = 0.5)
seq(1, 7, by = 0.5) 
seq(7, 1, by = -0.5) 
seq(1, 7, len = 10)
1:10
10:1
rep(3, 5)
rep(1:3, each=5)



##############################
# Logical Vectors
x <- rpois(n = 20, lambda = 15); temp <- x > 13
x
temp
###############################
r=c(0,0,1,1)
g=c(1,0,1,0)
r>0 & g>0 
r>0 | g>0
r>0
!r>0
r>0 && g>0
r>0 || g>0
######################################################
#Missing Values
z <- c(1:3,NA)
z
is.na(z) 
z==NA

zz <- c(1:3, NA, 0/0)
is.na(zz)
is.nan(zz)
########################################################
#Character vectors
cat("Hello\n!") 
cat("Hello\t!")
cat("Hello\b!")

x <- c("Name", "Height", "Gender")
labs <- paste(c("X","Y"), 1:4, sep="")
labs
labs <- paste(c("X","Y"), 1:4, sep=",") 
labs
###############################################
#Logical index vectors
x <- c(-1, 1 : 3, NA)
y <- x[!is.na(x)] 
y
z <- x[!is.na(x) & x>0]
z
#######################################
#Index vectors with positive integral quantities 
x <- c(-1, 1 : 3, NA) 
y <- x[c(3, 5, 2)] 
x
y 
c("x","y")[rep(c(1,2,2,1), times=4)] 
#######################################
#Index vectors with negative integral quantities
x <- c(-1, 1 : 3, NA) 
y <- x[-c(3, 5, 2)] 
y 
x <- c(-1, 1 : 3, NA) 
y1 <- x[-c(2, 3, 5)] 
y1 
##################################
#Index vectors with character strings
fruit <- c(5, 10, 1, 20) 
names(fruit) <- c("orange", "banana", "apple", "peach") 
lunch <- fruit[c("apple","orange")] 
lunch 

############################
# Matrices
mat1 = matrix(data = 1:12, nrow = 4, ncol = 3)
mat1
mat2 = matrix(data = 1:12, nrow = 4, ncol = 3, byrow=T)
mat2

dim(mat2)
#############################
euromat = matrix(data = c(GNP, AGRIC), nrow = 12)
euromat

euromat2 = cbind(GNP, AGRIC)
euromat2

euromat3 = rbind(GNP, AGRIC)
euromat3

###############################
# Array
z<- 1:30
dim(z) <- c(3,5,2)
z
z[2,3,1]
z[2,,]
z[,,2]
dim(z)

##############################
# Index matrices
x <- array(1:20, dim=c(4,5)) 
# Generate a 4 by 5 array.
x
i <- array(c(1:3,3:1), dim=c(3,2))
i # i is a 3 by 2 index array.
x[i] # Extract those elements
x[i] <- 0 # Replace those elements by zeros.
x

###############################
# Matrix multiplication
A <- matrix(1:9, ncol = 3, by = T) -> B;
A
B
A * B

###############################
# The diag() function
diag(c(1,3,2,4))
A <- matrix(1:9, ncol = 3, by = T)
diag(A)
diag(3)

###############################
# The apply() function
m1 <- matrix(1:6, nrow = 3, ncol = 2, byrow = T)
apply(m1, 2, sum)
apply(m1, 1, max)

apply(state.x77, 2, mean)
apply(state.x77,2,quantile,.99)

x <- array(rnorm(n = 24), dim=c(3, 4, 2)) 
apply(x, 1, sum)
apply(x, c(1,3), sum) # sum all the 
# elements sharing the same row 
# and the same layer
sum(x[1, ,1])

####################################
# A simple example to illustrate the image example
x <- matrix(rpois(15, lambda = 10), ncol = 5)
x.qt <- quantile(x, probs = seq(0, 1, length = 5))
x.qt.mid <- (x.qt[-1] + x.qt[-length(x.qt)])/2

x.arr <- array(x, dim = c(dim(x), length(x.qt.mid)))
mid.arr <- array(rep(x.qt.mid, each = prod(dim(x))), dim = dim(x.arr))

sq.diff.arr <- (x.arr - mid.arr)^2

min.idx1 <- apply(X = sq.diff.arr, MAR = c(1, 2), FUN = which.min)
x.quant <- array(x.qt.mid[min.idx1], dim = dim(x))


#########################################
# color quantization of images
PET <- matrix(scan(file =
                     "http://www.stt.msu.edu/Academics/ClassPages
                   + /uploads/FS12/802-1/fbp-img.txt"),
              ncol = 128, nrow = 128, byrow = T)

image(1:128, 1:128, PET[, 128:1], col =
        topo.colors(128^2))


PET.qt <- quantile(PET, probs = seq(0, 1, length = 9))
PET.qt.mid <- (PET.qt[-1] + PET.qt[-length(PET.qt)])/2

PET.arr <- array(PET, dim = c(dim(PET), length(PET.qt.mid)))
mid.arr <- array(rep(PET.qt.mid, each = prod(dim(PET))), dim = dim(PET.arr))

sq.diff.arr <- (PET.arr - mid.arr)^2
min.idx1 <- apply(X = sq.diff.arr, MAR = c(1, 2), FUN = which.min)
PET.quant <- array(PET.qt.mid[min.idx2], dim = dim(PET))

image(1:nrow(PET), 1:ncol(PET), PET[, 128:1], col = topo.colors(256))

PET.kmns <- kmeans(as.vector(PET), centers = 8, nstart = 1000)
PET.kmns.img <- array(PET.kmns$centers[PET.kmns$cluster], dim = dim(PET))
image(1:ncol(PET), 1:nrow(PET), PET.kmns.img[, 128:1], col = topo.colors(256))


####################################
# Factor example
data(USArrests)
state <- rownames(USArrests)

statef <- factor(state)
statef

levels(statef)

x.rep <- rep(1:nlevels(statef), times =
               sample(1:10, size = length(statef), rep = T))
state.f.rep <- statef[x.rep]
incomes <- rpois(n = length(state.f.rep), lambda =
                   300)
state.income <- data.frame(state.f.rep, incomes)

incmeans <- tapply(X = incomes, INDEX = state.f.rep, FUN = mean)

##################
# Eigen values of a matrix
mm <- matrix(rpois(9, 4), ncol = 3)
emm <- eigen(mm)
mode(emm)

######################
# lsfit() example
illit <- state.x77[,3]
murder <- state.x77[,5]
regout <- lsfit(illit, murder)
regout$coef

names(regout)

##########################
h <- c(15.1, 11.3, 7.0, 9.0)
names(h) <- c("APE", "BOX", "CAT", "DOG")
m <- c(1,2,3,4)
hm <- list(h,m)
hm <- list(h = h, m = m)

mode(hm)
hm1 <- sapply(hm, log)


chickwts
wtm <- tapply(X = chickwts$weight, INDEX = chickwts$feed, FUN = mean)
wtsd <- tapply(X = chickwts$weight, INDEX = chickwts$feed, FUN = sd)

###############################
help(cabbages,package="MASS")
data(cabbages,package="MASS")
with(cabbages, tapply(X = HeadWt, INDEX = list(Cult,Date), FUN = length))
with(cabbages, tapply(X = HeadWt, INDEX = list(Cult,Date), FUN = mean))
