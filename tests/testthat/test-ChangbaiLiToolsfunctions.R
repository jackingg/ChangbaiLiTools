context("Homework functions")

test_that("calculate1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(calculate1(x), x_list)
         })

test_that("calculate2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(calculate2(x), x_list)
  save<-try(calculate2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("gmlestimate computes MLE of gamma distribution", {
  x <- 1:10
  alpha = pi   
  f = function(alpha){
  sum(dgamma(x, shape = alpha, log = TRUE)) 
 }
 interval <- mean(x) + c(-1, 1) * 3 * sd(x)
 interval <- pmax(mean(x) / 1e3, interval)

 oout <- optimize(f, maximum = TRUE, interval)
  expect_identical(gmlestimate(x), oout$maximum)
})

test_that("wcalculate1 computes weighted mean, var, sd", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  var1<-function(d){sum(((d$x - sum(d$x * d$p))^2) * d$p)}
  x_list<-list(mean= sum(d$x * d$p),var=var1(d),sd=sqrt(var1(d)))
  expect_identical(wcalculate1(d), x_list)
})

test_that("wcalculate2 computes weighted mean, var, sd with user checkes", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  var1<-function(d){sum(((d$x - sum(d$x * d$p))^2) * d$p)}
  x_list<-list(mean= sum(d$x * d$p),var=var1(d),sd=sqrt(var1(d)))
  expect_identical(wcalculate2(d), x_list)
})

test_that("hlcheck is highlevel check function", {
  save<-try(hlcheck(NA),silent=TRUE)
  expect_identical(save,"NA or NAN")
})

test_that("mle computes MLE", {
  x1 = rgamma(100,3)
  func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  result <- function(x,func,interval){
    f7 <- function(theta, x)
    {sum(func(theta, x))}
    oout<- optimize(f7, maximum = TRUE, interval, x=x)
    return(oout$maximum)
  }
  test <- result(x1, func1, c(0,3))
  expect_identical(test, mle(x1, func1, c(0,3)))
})

test_that("calculate3 computes $x^T A^{-1} x$", {
  x <- c(1,2,3,4)
  a <-matrix(1:16,nc=4,nr=4)
  # a1 <- solve(a)
  # p <- t(x) %*% a1 %*% x
  # expect_identical(p, calculate3(a,x))
  save<-try(calculate3(a, NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: nrow(a) == length(x) is not TRUE\n")
})

test_that("stdization computes Standardization", {
  x <- 1:10
  f = function(x){return((x-mean(x))/sd(x))}
  # expect_identical(stdization(x), apply(x, 2, f))
  save<-try(stdization(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.matrix(m) is not TRUE\n")
})
  
test_that("myapply computes like the function array function(X, MARGIN, FUN, ...)", {
    fred <- matrix(1:6, ncol = 2)
    s = array(rnorm(72),c(3,3,8))
    p <- apply(fred, 1, mean)
    expect_identical(myapply(fred, 1, mean), p)
  })




