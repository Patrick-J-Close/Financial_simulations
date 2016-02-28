path <- "C:/Users/Patrick Close/Documents/Dev/Projects/R_files"
setwd(path)

# **** Helper Functions ****
# compute undiscounted EPS CAGR given v(0), v(n) and years fwd
undisc_eps <- function(t0, tn, n){
  CAGR <- (tn/t0)^(1/n) - 1
  # as decimal
  return(CAGR)
}
# compute discounted EPS CAGR given v(0), v(n), years fwd, and disc rate
disc_eps <- function(t0, tn, n, r){
  r <- r/100
  e.n <- (tn)/((1+r)^n)
  CAGR <- (e.n/t0)^(1/n) - 1
  # as decimal 
  return(CAGR)
}

# **** Data Import & Aggregation ****
eps1_file.names <- dir(path = path, pattern = "_EPS1.csv")
eps2_file.names <- dir(path = path, pattern = "_EPS2.csv")
eps3_file.names <- dir(path = path, pattern = "_EPS3.csv")
pe1_file.names <- dir(path = path, pattern = "_PE1.csv")
hist_file.names <- dir(path = path, pattern = "_hist.csv")

# merge data
eps <- data.frame()
for(i in 1:length(hist_file.names)){
  eps1 <- read.csv(eps1_file.names[i], stringsAsFactors = FALSE)
  colnames(eps1)[2] <- "BEST_EPS1"
  eps2 <- read.csv(eps2_file.names[i], stringsAsFactors = FALSE)
  colnames(eps2)[2] <- "BEST_EPS2"
  eps3 <- read.csv(eps3_file.names[i], stringsAsFactors = FALSE)
  colnames(eps3)[2] <- "BEST_EPS3"
  pe1 <- read.csv(pe1_file.names[i], stringsAsFactors = FALSE)
  hist1 <- read.csv(hist_file.names[i], stringsAsFactors = FALSE)
  
  df <- cbind(substr(hist_file.names[i],1,8), eps1[,1:2])
  colnames(df)[1] <- "Date"
  df <- merge(df, eps2[,1:2], by = "Ticker")
  df <- merge(df, eps3[,1:2], by = "Ticker")
  df <- merge(df, pe1[,1:2], by = "Ticker")
  df <- merge(df, hist1, by = "Ticker")
  eps <- rbind(eps, df)
}
# clean #N/A values
eps$BEST_EPS1 <- as.numeric(eps$BEST_EPS1)
eps$BEST_EPS2 <- as.numeric(eps$BEST_EPS2)
eps$BEST_EPS3 <- as.numeric(eps$BEST_EPS3)
eps$BEST_PE_RATIO <- as.numeric(eps$BEST_PE_RATIO)

# add UNdiscounted EPS CAGR values
eps$UN_CAGR1 <- undisc_eps(eps$TRAIL_12M_EPS, eps$BEST_EPS1,1)
eps$UN_CAGR2 <- undisc_eps(eps$TRAIL_12M_EPS, eps$BEST_EPS2,2)
eps$UN_CAGR3 <- undisc_eps(eps$TRAIL_12M_EPS, eps$BEST_EPS3,3)

# add discounted EPS CAGR values
eps$Disc_CAGR1 <- disc_eps(eps$TRAIL_12M_EPS, eps$BEST_EPS1,1,eps$WACC_COST_EQUITY)
eps$Disc_CAGR2 <- disc_eps(eps$TRAIL_12M_EPS, eps$BEST_EPS2,2,eps$WACC_COST_EQUITY)
eps$Disc_CAGR3 <- disc_eps(eps$TRAIL_12M_EPS, eps$BEST_EPS3,3,eps$WACC_COST_EQUITY)

# perform cross sectional regressions and store result
library(plyr)

vars <- names(eps[13:18])
for(i in 1:length(vars)){
     # remove all NA PEs & CAGR variable
     df <- subset(eps, (!is.na(eps$PE_RATIO)) & (!is.na(eps[,(12+i)])))
     reg_formula <- paste(vars[i], " ~ PE_RATIO")
     reg_df <- ddply(df, "Date", function(x){
     model <- lm(formula = as.formula(reg_formula), data = x, na.action = na.exclude)
     return(c(coef(model), summary(model)$r.squared))
  })
  if(i == 1){
       colnames(reg_df) <- c("Date", paste("Beta0_",vars[i], sep = ""),
                             paste("Beta1_",vars[i], sep = ""),
                             paste("r-squared_", vars[i], sep = "")) 
       regressions <- reg_df
  }else{
       colnames(reg_df) <- c("Date", paste("Beta0_",vars[i], sep = ""),
                             paste("Beta1_",vars[i], sep = ""),
                             paste("r-squared_", vars[i], sep = ""))
       regressions <- merge(regressions, reg_df, by = "Date")
  }
}
colMeans(regressions[2:19])

# Conclusions: model fit improves as EPS CAGR timeframe is increased and undiscounted
# vs. discounted methods yield similar model fits on average.  

# **** Forward Return Analysis ****
# STEP 2: pool residuals and test for relationship to standardized forward returns
# 3-year discounted EPS CAGR vs Fwd retruns at 6, 12, 18, 24 months

# Using best fit model, build df of residuals
pool_resids <- function(x){
     model <- lm(Disc_CAGR3 ~ PE_RATIO, data = x, na.action = na.exclude)
     return(cbind(x[,1:2],residuals(model)))
}

# compute cumulative returns and z-scores given subset of eps df for all periods to be computed
fwd_returns <- function(df){
     rets <- ddply(df,"Ticker", function(x){
          ret <- x$CUST_TRR_RETURN_HOLDING_PER_90D_FWD
          ret <- ret/100 + 1
          ret <- prod(ret) - 1 
          return(ret)
     })
     colnames(rets)[2] <- c("TotalReturn")
     mu <- mean(rets[,2], na.rm = TRUE)
     sd <- sd(rets[,2], na.rm = TRUE)
     rets$Zscore <- (rets$TotalReturn - mu)/sd
     rets <- cbind(Date = df$Date[1], rets)
     return(rets)
}

# compute standardized returns cross sectionally
# build df of date, ticker, fwd retrun (nper), z-scores and bind to residuals
nper <- 4 # quarters fwd
df <- subset(eps, (!is.na(eps$BEST_PE_RATIO)) & (!is.na(eps$Disc_CAGR3)))
d <- unique(df$Date)
res_zscores <- data.frame()
for(i in 1:(length(d)-nper)){
     temp <- merge(pool_resids(subset(df, df$Date == d[i])),
          fwd_returns(df[df$Date %in% d[1:(i+nper-1)],]), by = "Ticker")
     res_zscores <- rbind(res_zscores, temp)
}
res_zscores <- cbind(res_zscores[1:3], res_zscores[5:6])
colnames(res_zscores)[3] <- "e"
            
model <- lm(Zscore ~ e, data = res_zscores, na.action = na.exclude)
summary(model)


# **** Plots ****
library(ggplot2)

g <- ggplot(data = res_zscores, aes(x = Zscore, y = e)) + 
     geom_point() + geom_smooth(method = lm)
print(g)

