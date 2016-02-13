# **************************************************
# Simulate a random portfolio performance and plot it as a density 
# **************************************************

# Create portfolios: Time, Index, y1, y2, ..., yn
n <- 100
nsim <- 200
set.seed(123)
ret_result <- data.frame(as.numeric(1:n))
index <- data.frame(rnorm(n, mean = 0.02, sd = 0.015))
index <- index_val(index)
ret_result <- cbind(ret_result, index)
colnames(ret_result) <- c("Time", "Index")
for(i in 3:nsim){
     ret <- data.frame(rnorm(n, mean = 0.02, sd = 0.015))
     ret_result <- cbind(ret_result,index_val(ret))
     colnames(ret_result)[i] <- paste("y", i, sep = "")
}
# helper function to change return percentages to index vale
index_val <- function(returns){
     result <- data.frame()
     index <- 100
     for(i in 1:length(returns[,1])){
          index <- index * (1 + returns[i,1])
          result <- rbind(result,index)
     }
     return(result)
}

# plots 
library(ggplot2)

# Scatterplot
g <- ggplot(ret_result, aes(x = ret_result$Time))
vars <- names(ret_result)
for(i in vars[-2]){
     g <- g + geom_point(aes_string(y=toString(i)), alpha = 0.2)
}
print(g)

# Line graph
g <- ggplot(ret_result, aes(x = ret_result$Time))
vars <- names(ret_result)
for(i in vars[-1]){
     g <- g + geom_line(aes_string(y=toString(i)), alpha = 0.1)
}
g <- g + geom_line(aes(y=ret_result$Index), colour = "red", alpha = 1, size = 1) +
     ylab("Portfolio Value") +
     xlab("Time")
print(g)
