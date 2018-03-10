#' Calculate Mean, Variane, Standard Deviation
#'
#' Computes the mean, variance and stanard deviation of a vector
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' calculate1(rnorm(10))
calculate1 = function(x){
  a = sum(x)/length(x)  
  b = sum((x - a)^2) / length(x)   
  c = sqrt(b)  
  return(list(mean = a, var = b, sd = c))
}

x <- 1:10
calculate1(x)


#' Calculate Mean, Variane, SD (again)
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' calculate2(rnorm(10))
calculate2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' Maximum Likelihood Estimate with a gamma distribution
#'
#' Computes the maximum of the liklihood with a gamma distribution
#'
#' @param x vector
#'
#' @return scalar
#' @export
#' @examples
#' gmlestimate(rnorm(10))
gmlestimate = function(x){
  alpha = pi   
  func3 = function(alpha){
    sum(dgamma(x, shape = alpha, log = TRUE)) 
  }
  interval <- mean(x) + c(-1, 1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout <- optimize(func3, maximum = TRUE, interval)    
  return(oout$maximum)
}

#' Weighted Mean, Variane, and Standard Deviation
#'
#' Computes the weighted mean, variane, standard deviation
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' data(d)
#' wcalculate1(d)
wcalculate1 <- function(d){
  
  ## x and p are the column vector of d
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  
  return(list(mean=a,var=b,sd=c))
  
}

#' Weighted Mean, Variane, and Standard Deviation with User Checkes
#'
#' Computes the weighted Mean, Variane, and Standard Deviation with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' data(d)
#' wcalculate2(d)
wcalculate2 <- function(d){
  
  stopifnot(is.numeric(d$x))
  stopifnot(is.numeric(d$p))
  
  stopifnot(length(d$x)!=0)
  stopifnot(length(d$p)!=0)
  
  stopifnot(is.finite(d$x))
  stopifnot(is.finite(d$p))

  stopifnot(all.equal(sum(d$p),1))
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  
  return(list(mean=a,var=b,sd=c))
  
}

#' High-levels Check Function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' hlcheck(NA)

hlcheck <- function(x){
  
  tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
  tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
  tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
  tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
  tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
  
}

#' Maximum Likelihood Estimate of A Given Distribution for Data X
#'
#' Estimate the maxium of liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function (experession)
#' @param interval vector(interval for optimize function)
#'
#' @return scalar
#' @export
#' @examples
#' x1 = rgamma(100,3)
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result <- mle(x1,func1,c(0,100))
mle <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
} 

#' Compute Transpose And Inverse
#'
#' Given a numeric matrix A and a numeric vector `x`, 
#' calculates $x^T A^{-1} x$
#' Note that this only makes sense when A is a square matrix 
#' and the dimension of x is the same as the dimensions of the row and column dimensions of A 
#'
#'
#' @param x vector
#' @param a matrix
#'
#' @return scalar
#' @export
#' @examples
#' x <- c(1,2,3,4)
#' mtx <-matrix(rnorm(16),nc=4,nr=4)
#' result8 <- calculate3(mtx, x)
#'

calculate3 = function(a, x){
  stopifnot(is.matrix(a))
  stopifnot(is.vector(x))
  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(is.finite(a))
  stopifnot(is.finite(x))
  
  m = solve(a, x)
  
  return(t(x) %*% m)
}


#' Standardization
#'
#' Take a numeric matrix and standardizes its columns
#'
#' @param m matrix which has more than one row
#'
#' @return matrix
#' @export
#' @examples
#' m <-matrix(rnorm(16),nc=4,nr=4)
#' result9 <- stdization(m)
#' 
stdization <- function(m){
  stopifnot(nrow(m) > 1)
  stopifnot(is.matrix(m))
  stopifnot(is.numeric(m))
  stopifnot(is.finite(m))
  
  f = function(x){
    
    return((x-mean(x))/sd(x))
  }
  
  return (apply(m, 2, f))
}



#' My Apply Function
#'
#' Myapply function just like the function array
#' function(X, MARGIN, FUN, ...)
#'
#' @param  X a matrix 
#' @param MARGIN either (the number) 1 or (the number) 2
#' @param FUN an R function that maps vectors to vectors
#' 
#' @return array or matrix
#' @export
#' @examples
#' m <- matrix(1:6, ncol = 2)
#' s = array(rnorm(72),c(3,3,8))
#' result10 <- myapply(m,1,"mean")
#' 
myapply <- function(X, MARGIN, FUN, ...)
{
  #stopifnot(length(dim(X))==2)
  
  if(length(dim(X))!=2)
  {
    stop("matrix is not 2d")
  } 
  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not in 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }
  }else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}


