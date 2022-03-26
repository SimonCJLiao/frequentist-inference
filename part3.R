set.seed(1112)
prop_stepAIC <- function(n){
  set.seed(1)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  sigma <- seq(0.1,1,.1)
  x3 <- .5*x1/sd(x1)+sqrt(1-.5^2)*rnorm(n)
  x4 <- .7*x2/sd(x2)+sqrt(1-.7^2)*rnorm(n)
  M <- matrix(NA,nrow=1000,ncol=10)
  for(i in 1:1000){
    for (j in 1:10){
      y <- 4+3*x1-.1*x2+rnorm(n,0,(sigma[j])^2)
      M[i,j] <- stepAIC(lm(y~x1+x2+x3+x4))$call == lm(y~x1+x2)$call
      
    }
  }
  return(apply(M,2,mean))
}
data <- cbind(prop_stepAIC(100),prop_stepAIC(500),prop_stepAIC(1000))
save(data, file = "data.RData")

