### ComputeCost
computeCost <- function(X,y,theta) {
  h <- X %*% theta
  J <- sum( ((h - y) ^ 2) / ( 2 * length(y)) )
}

### GradientDescent
gradientDescent <- function(X,y,theta,alpha,iterations){
  m <- length(y)
  J_history <- rep(0,iterations)
  
  for (i in 1:iterations) {
    d <- (X %*% theta) - y
    d <- (t(d) %*% X) * (alpha/m)
    
    theta <- theta - t(d)
    
    J_history[i] <- computeCost(X,y, theta)
    
  }
  
  list(theta,J_history)
}

#####
data1 <- read.csv("ex1data1.txt",header = F)
X <- data1[,1]
y <- data[,2]
m<-length(X)

plot(X,y)

X <- cbind(rep(1,m), X)

theta <- matrix(rep(0,2),2,1)

iterations <- 1500
alpha <- 0.01

J <- computeCost(X,y,theta)
print(J)
print(computeCost(X,y,matrix(c(-1,2),2,1)))

ret<-gradientDescent(X,y,theta, alpha, iterations)
theta <- ret[[1]]
J_history <- ret[[2]]



plot(X[,2], X%*% theta)