library(ggplot2)
library(dplyr)



sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

costFunction <- function(X,y) {
  function (theta) {
    h <- sigmoid(as.matrix(X) %*% theta)
    J <- -log(h)*y - log(1-h)*(1-y)
    J <- mean(J[,])
  }
}

gradient <- function(X,y) {
  function (theta) {
    h <- sigmoid(as.matrix(X) %*% theta)
    apply(X * rep((h-y), length(X)), MARGIN = 2, mean)
  }
}

data <- read.csv("ex2/ex2data1.txt", header = F)
df <- data.frame(intercept=1, exam1=data[,1],exam2=data[,2],admitted=data[,3])
ggplot(data = df) + geom_point(aes(x=exam1,y=exam2,color=admitted))

X <- df %>% select(intercept,exam1,exam2)
y <- df %>% select(admitted)

initial_theta <- rep(0,length(X))

cf <- costFunction(X,y)(initial_theta)
grad <- gradient(X,y)(initial_theta)

test_theta <- c(-24,0.2, 0.2)
cf <- costFunction(X,y)(test_theta)
grad <- gradient(X,y)(test_theta)

#######

optimRes <- optim(par=initial_theta, fn=costFunction(X,y), gr = gradient(X,y), method="BFGS", control = list(maxit = 400))

theta <- optimRes$par
cost <- optimRes$value

# Print theta to screen
cat(sprintf('Cost at theta found by optim: %f\n', cost))
cat(sprintf('theta: \n'))
cat(sprintf(' %f \n', theta))

# Plot Boundary
ggplot(data = df) + geom_point(aes(x=exam1,y=exam2,color=admitted)) + geom_abline(intercept = -theta[1]/theta[3], slope= -theta[2] / theta[3])

# como hacerlo usando glm
glm(data=df, admitted ~ exam1+exam2, family = binomial(link = logit))