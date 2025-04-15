library(vars)
library(tsDyn)
library(tseries)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(extrafont)
library(fixest)
library(lubridate)
library(xts)
library(cowplot)  
library(ggthemes) 
library(scales)
library(mFilter)
library(aTSA)
library(urca)
library(MTS)
library(lpirfs)
library(car)
library(VAR.etp)
library(nlme)
library(portes)


# Function ----------------------------------------------------------------

irfs <- function(model, variable = 1, nsteps) {
  # Get parameters from model
  p <- model$p  
  K <- model$K
  
  # Initialize matrices based on p
  if (p == 1) {
    A1 <- Bcoef(model)[, 1:K]
    x0 <- Bcoef(model)[, p * K + 2]
  }
  
  if (p == 2) {
    A1 <- Bcoef(model)[, 1:K]
    A2 <- Bcoef(model)[, (K+1):(p*K)]
    x0 <- Bcoef(model)[, p * K + 2]
  }
  
  if (p == 3) {
    A1 <- Bcoef(model)[, 1:K]
    A2 <- Bcoef(model)[, (K+1):(2*K)]
    A3 <- Bcoef(model)[, (2*K+1):(3*K)]
    x0 <- Bcoef(model)[, p * K + 2]
  }
  
  if (p == 4) {
    A1 <- Bcoef(model)[, 1:K]
    A2 <- Bcoef(model)[, (K+1):(2*K)]
    A3 <- Bcoef(model)[, (2*K+1):(3*K)]
    A4 <- Bcoef(model)[, (3*K+1):(4*K)]
    x0 <- Bcoef(model)[, p * K + 2]
  }
  
  # Initialize list to store x values
  x <- list()  
  x[[1]] <- as.matrix(x0)  # x1 is just x0
  
  # Loop for generating IRFs based on p
  if (p == 1) {
    for (t in 2:nsteps) {
      x[[t]] <- A1 %*% x[[t-1]]
    }
  }
  
  if (p == 2) {
    for (t in 2:nsteps) {
      if (t == 2) {
        x[[t]] <- A1 %*% x[[1]]  # x2 uses x1 and x0
      } else {
        x[[t]] <- A1 %*% x[[t-1]] + A2 %*% x[[t-2]]  # xt uses previous two x values
      }
    }
  }
  
  if (p == 3) {
    for (t in 2:nsteps) {
      if (t == 2) {
        x[[t]] <- A1 %*% x[[1]]  # x2 uses x1 and x0
      } else if (t == 3) {
        x[[t]] <- A1 %*% x[[2]] + A2 %*% x[[1]]  # x3 uses x2, x1, and x0
      } else {
        x[[t]] <- A1 %*% x[[t-1]] + A2 %*% x[[t-2]] + A3 %*% x[[t-3]]  # for t > 3
      }
    }
  }
  
  if (p == 4) {
    for (t in 2:nsteps) {
      if (t == 2) {
        x[[t]] <- A1 %*% x[[1]]  # x2 uses x1 and x0
      } else if (t == 3) {
        x[[t]] <- A1 %*% x[[2]] + A2 %*% x[[1]]  # x3 uses x2, x1, and x0
      } else if (t == 4) {
        x[[t]] <- A1 %*% x[[3]] + A2 %*% x[[2]] + A3 %*% x[[1]]  # for t > 3
      } else {
        x[[t]] <- A1 %*% x[[t-1]] + A2 %*% x[[t-2]] + A3 %*% x[[t-3]] + A4 %*% x[[t-4]]
      }
    }
  }
  
  # Create a list to store the selected variable's values from each x
  irfs <- list()  
  for (t in 1:nsteps) {
    irfs[[t]] <- x[[t]][variable]  # Select the desired element for each x[t]
  }
  irfs <- as.matrix(irfs)
  irfs <- matrix(unlist(irfs), ncol = 1)
  dimnames(irfs) <- list(NULL, "var")
  
  return(irfs)  # Return the list of IRFs
}

bootstr <- function (x, n.ahead, runs, seed, variable) 
{
  VAR <- x
  p <- VAR$p
  K <- VAR$K
  obs <- VAR$obs
  total <- VAR$totobs
  type <- VAR$type
  B <- Bcoef(VAR)
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(VAR$y)
  Zdet <- NULL
  if (ncol(VAR$datamat) > (K * (p + 1))) {
    Zdet <- as.matrix(VAR$datamat[, (K * (p + 1) + 1):ncol(VAR$datamat)])
  }
  resorig <- scale(resid(VAR), scale = FALSE)
  B <- Bcoef(VAR)
  set.seed(seed)
  for (i in 1:runs) {
    booted <- sample(c(1:obs), replace = TRUE)
    resid <- resorig[booted, ]
    lasty <- c(t(VAR$y[p:1, ]))
    ysampled[c(1:p), ] <- VAR$y[c(1:p), ]
    for (j in 1:obs) {
      lasty <- lasty[1:(K * p)]
      Z <- c(lasty, Zdet[j, ])
      ysampled[j + p, ] <- B %*% Z + resid[j, ]
      lasty <- c(ysampled[j + p, ], lasty)
    }
    varboot <- update(VAR, y = ysampled)
    if (is(x, "svarest")) {
      varboot <- update(x, x = varboot)
    }
    BOOT[[i]] <- irfs(model = varboot, variable = variable, nsteps = n.ahead)
  }
  ci <- 0.05
  lower <- ci / 2
  upper <- 1 - ci / 2
  hall <- 0.32
  hlower <- hall / 2
  hupper <- 1 - hall / 2
  fift <- 0.50
  flower <- fift / 2
  fupper <- 1 - fift / 2
  mat.l <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)
  mat.u <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)
  mat.hl <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)
  mat.hu <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)
  mat.fl <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)
  mat.fu <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)
  mat.m <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)  # Matrix for median response
  mat.mean <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)  # Matrix for mean response
  mat.rand <- matrix(NA, nrow = length(BOOT[[1]]), ncol = 1)  # Matrix for mean response
  
  FLower <- list()
  FUpper <- list()
  HLower <- list()
  HUpper <- list()
  Lower <- list()
  Upper <- list()
  
  # Initialize temp vector before starting the loop
  temp <- numeric(runs)
  
  for (l in 1:nrow(mat.l)) {  # Loop over time steps
    for (i in 1:runs) {       # Loop over bootstrap iterations
      temp[i] <- BOOT[[i]][l]  # Extract values for time step `l`
    }
    
    # Compute various quantiles and statistics
    mat.l[l, 1] <- quantile(temp, probs = lower, na.rm = TRUE)  # Compute lower CI
    mat.u[l, 1] <- quantile(temp, probs = upper, na.rm = TRUE)  # Compute upper CI
    mat.hl[l, 1] <- quantile(temp, probs = hlower, na.rm = TRUE)  # Compute lower CI for high CI
    mat.hu[l, 1] <- quantile(temp, probs = hupper, na.rm = TRUE)  # Compute upper CI for high CI
    mat.fl[l, 1] <- quantile(temp, probs = flower, na.rm = TRUE)  # Compute lower CI for full CI
    mat.fu[l, 1] <- quantile(temp, probs = fupper, na.rm = TRUE)  # Compute upper CI for full CI
    mat.m[l, 1] <- median(temp, na.rm = TRUE)  # Compute median response
    mat.mean[l, 1] <- mean(temp, na.rm = TRUE)  # Compute mean response
    mat.rand[l, 1] <- sample(temp, 1)  # Take a random sample from the bootstrap
  }
  
  
  Lower <- mat.l
  Upper <- mat.u
  HLower <- mat.hl
  HUpper <- mat.hu
  FLower <- mat.fl
  FUpper <- mat.fu
  IRF_median <- mat.m
  IRF_mean <- mat.mean
  
  point <- irfs(VAR, variable = variable, nsteps = n.ahead)
  result <- list(Lower = Lower, Upper = Upper, IRF_point = point,
                 HLower = HLower, HUpper = HUpper, IRF_median = IRF_median)
  result <- as.data.frame(result)
  result$t <- 1:nrow(result)-1
  
  return(result)
}

BOOT <- function (x, n.ahead, runs, seed, variable) 
{
  VAR <- x
  p <- VAR$p
  K <- VAR$K
  obs <- VAR$obs
  total <- VAR$totobs
  type <- VAR$type
  B <- Bcoef(VAR)
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(VAR$y)
  Zdet <- NULL
  if (ncol(VAR$datamat) > (K * (p + 1))) {
    Zdet <- as.matrix(VAR$datamat[, (K * (p + 1) + 1):ncol(VAR$datamat)])
  }
  resorig <- scale(resid(VAR), scale = FALSE)
  B <- Bcoef(VAR)
  set.seed(seed)
  for (i in 1:runs) {
    booted <- sample(c(1:obs), replace = TRUE)
    resid <- resorig[booted, ]
    lasty <- c(t(VAR$y[p:1, ]))
    ysampled[c(1:p), ] <- VAR$y[c(1:p), ]
    for (j in 1:obs) {
      lasty <- lasty[1:(K * p)]
      Z <- c(lasty, Zdet[j, ])
      ysampled[j + p, ] <- B %*% Z + resid[j, ]
      lasty <- c(ysampled[j + p, ], lasty)
    }
    varboot <- update(VAR, y = ysampled)
    if (is(x, "svarest")) {
      varboot <- update(x, x = varboot)
    }
    BOOT[[i]] <- irfs(model = varboot, variable = variable, nsteps = n.ahead)
  }
  return(BOOT)
}

# Data Preparation --------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/slovakia.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data$dlgdp <- c(NA, diff(log(data$gdp)))

data$hicp_d <- c(NA, diff(data$hicp))

data <- data[57:99,]

tseries::adf.test(data$dlgdp) # Stationary
tseries::kpss.test(data$dlgdp) # Stationary

tseries::adf.test(data$hicp_d) # Not stationary
tseries::kpss.test(data$hicp_d) # Stationary

#data$gdp_l <- log(data$gdp) 
#hp_gdp <- hpfilter(data$gdp_l, freq = 1600, type = "lambda")
#data$gdp_f <- hp_gdp$cycle

# Shadow --------------------------------------------------------------
acf(data$new_shadow_mps)
pacf(data$new_shadow_mps)

VARselect(y = data[1:nrow(data), c("hicp_d", "dlgdp", "ns_d")], lag.max = 4, type = "const", 
          exogen = data[1:nrow(data), "new_shadow_mps"])

var_shadow <- vars::VAR(y = data[1:nrow(data), c("hicp_d", "dlgdp", "ns_d")], 
                        p = 1, 
                        type = "const", 
                        exogen = data[1:nrow(data), "new_shadow_mps"])

summary(var_shadow)
# Stable 

serial.test(var_shadow)
# No autocorrelation
serial.test(var_shadow, type = "PT.adjusted")
# There is autocorrelation

hicp <- bootstr(var_shadow, n.ahead = 21, runs = 1000, seed = 123, variable = 1)
gdp <- bootstr(var_shadow, n.ahead = 21, runs = 1000, seed = 123, variable = 2)
intr <- bootstr(var_shadow, n.ahead = 21, runs = 1000, seed = 123, variable = 3)

## IRFs --------------------------------------------------------------------

hicp <- hicp %>%
  mutate(facet_label = "Monetary Policy Shock → HICP inflation rate")

p1 <- ggplot(data = hicp) +
  geom_ribbon(aes(x = t, ymin = Lower * 100, ymax = Upper * 100), fill = "#bdbdbd", alpha = 0.5) +
  geom_ribbon(aes(x = t, ymin = HLower * 100, ymax = HUpper * 100), fill = "#a8a8a8", alpha = 0.5) +
  
  # Impulse Response Function (IRF) Line
  geom_line(aes(x = t, y = var * 100), color = "#1b1b1b", size = 1) +
  
  # Horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Labels
  labs(
    x = "Quarters",
    y = "(%-point)"
  ) +
  
  # Scale X Axis with Custom Breaks
  scale_x_continuous(
    breaks = seq(min(hicp$t), max(hicp$t), by = 5)  
  ) +
  
  # Facet Wrap to Create Title Box
  facet_wrap(~ facet_label) +
  
  # Custom Theme: No Legends & Title Box via Facet
  theme_bw(base_size = 16) +  
  theme(
    strip.text = element_text(size = 18, face = "bold", color = "black"),
    strip.background = element_rect(fill = "gray85"),  
    axis.title = element_text(size = 18, face = "bold", color = "#333333"),
    axis.text = element_text(size = 14, color = "#333333"),
    
    # Remove Legends Completely
    legend.position = "none",
    
    panel.grid.major = element_line(size = 0.3, linetype = "dotted", color = "gray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

gdp <- gdp %>%
  mutate(facet_label = "Monetary Policy Shock → RGDP growth rate")

p2 <- ggplot(data = gdp) +
  geom_ribbon(aes(x = t, ymin = Lower * 100, ymax = Upper * 100), fill = "#bdbdbd", alpha = 0.5) +
  geom_ribbon(aes(x = t, ymin = HLower * 100, ymax = HUpper * 100), fill = "#a8a8a8", alpha = 0.5) +
  
  # Impulse Response Function (IRF) Line
  geom_line(aes(x = t, y = var * 100), color = "#1b1b1b", size = 1) +
  
  # Horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Labels
  labs(
    x = "Quarters",
    y = "(100 x log)"
  ) +
  
  # Scale X Axis with Custom Breaks
  scale_x_continuous(
    breaks = seq(min(gdp$t), max(gdp$t), by = 5)  
  ) +
  
  # Facet Wrap to Create Title Box
  facet_wrap(~ facet_label) +
  
  # Custom Theme: No Legends & Title Box via Facet
  theme_bw(base_size = 16) +  
  theme(
    strip.text = element_text(size = 18, face = "bold", color = "black"),
    strip.background = element_rect(fill = "gray85"),  
    axis.title = element_text(size = 18, face = "bold", color = "#333333"),
    axis.text = element_text(size = 14, color = "#333333"),
    
    # Remove Legends Completely
    legend.position = "none",
    
    panel.grid.major = element_line(size = 0.3, linetype = "dotted", color = "gray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

intr <- intr %>%
  mutate(facet_label = "Monetary Policy Shock → Shadow rate")

p3 <- ggplot(data = intr) +
  geom_ribbon(aes(x = t, ymin = Lower * 100, ymax = Upper * 100), fill = "#bdbdbd", alpha = 0.5) +
  geom_ribbon(aes(x = t, ymin = HLower * 100, ymax = HUpper * 100), fill = "#a8a8a8", alpha = 0.5) +
  
  # Impulse Response Function (IRF) Line
  geom_line(aes(x = t, y = var * 100), color = "#1b1b1b", size = 1) +
  
  # Horizontal line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Labels
  labs(
    x = "Quarters",
    y = "(%-point)"
  ) +
  
  # Scale X Axis with Custom Breaks
  scale_x_continuous(
    breaks = seq(min(intr$t), max(intr$t), by = 5)  
  ) +
  
  # Facet Wrap to Create Title Box
  facet_wrap(~ facet_label) +
  
  # Custom Theme: No Legends & Title Box via Facet
  theme_bw(base_size = 16) +  
  theme(
    strip.text = element_text(size = 18, face = "bold", color = "black"),
    strip.background = element_rect(fill = "gray85"),  
    axis.title = element_text(size = 18, face = "bold", color = "#333333"),
    axis.text = element_text(size = 14, color = "#333333"),
    
    # Remove Legends Completely
    legend.position = "none",
    
    panel.grid.major = element_line(size = 0.3, linetype = "dotted", color = "gray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

p1
p2
p3

grid.arrange(p1, p2, p3, ncol = 1)

## Monetary Policy Multiplicator -----------------------------------------

hicp <- bootstr(var_shadow, n.ahead = 20, runs = 1000, seed = 123, variable = 1)
gdp <- bootstr(var_shadow, n.ahead = 20, runs = 1000, seed = 123, variable = 2)
intr <- bootstr(var_shadow, n.ahead = 20, runs = 1000, seed = 123, variable = 3)

(sum(gdp$var) / sum(intr$var)) #0.1093356
(sum(hicp$var) / sum(intr$var)) #-0.05814724

(sum(gdp$var[1:4]) / sum(intr$var[1:4])) #0.1041299
(sum(hicp$var[1:4]) / sum(intr$var[1:4])) #-0.05999967

(sum(gdp$var[1]) / sum(intr$var[1])) #-0.002359198
(sum(hicp$var[1]) / sum(intr$var[1])) #-0.1357726

### H = 20 ---------------------------------------------------------------

ci <- 0.32
lower <- ci / 2
upper <- 1 - ci / 2

multip_hicp <- BOOT(var_shadow, n.ahead = 20, runs = 1000, seed = 123, variable = 1)
multip_gdp <- BOOT(var_shadow, n.ahead = 20, runs = 1000, seed = 123, variable = 2)
multip_intr <- BOOT(var_shadow, n.ahead = 20, runs = 1000, seed = 123, variable = 3)

sum_gdp <- sapply(multip_gdp, sum)
sum_intr <- sapply(multip_intr, sum)
sum_hicp <- sapply(multip_hicp, sum)

gdp_intr <- sum_gdp / sum_intr
quantile(gdp_intr, probs = lower, na.rm = T) #-0.04526522            
quantile(gdp_intr, probs = upper, na.rm = T) #0.2569145             

sd(gdp_intr)/sqrt(length(gdp_intr)) #0.005071058

hicp_intr <- sum_hicp / sum_intr
quantile(hicp_intr, probs = lower, na.rm = T) #-0.1420005              
quantile(hicp_intr, probs = upper, na.rm = T) #0.01072977              

sd(hicp_intr)/sqrt(length(hicp_intr)) #0.002517038

### H = 4 ---------------------------------------------------------------

multip_hicp <- BOOT(var_shadow, n.ahead = 4, runs = 1000, seed = 123, variable = 1)
multip_gdp <- BOOT(var_shadow, n.ahead = 4, runs = 1000, seed = 123, variable = 2)
multip_intr <- BOOT(var_shadow, n.ahead = 4, runs = 1000, seed = 123, variable = 3)

sum_gdp <- sapply(multip_gdp, sum)
sum_intr <- sapply(multip_intr, sum)
sum_hicp <- sapply(multip_hicp, sum)

gdp_intr <- sum_gdp / sum_intr
quantile(gdp_intr, probs = lower, na.rm = T) #-0.04967132             
quantile(gdp_intr, probs = upper, na.rm = T) #0.2481292              

sd(gdp_intr)/sqrt(length(gdp_intr)) #0.004974859

hicp_intr <- sum_hicp / sum_intr
quantile(hicp_intr, probs = lower, na.rm = T) #-0.1422745               
quantile(hicp_intr, probs = upper, na.rm = T) #0.009877359               

sd(hicp_intr)/sqrt(length(hicp_intr)) #0.00248843

### On-Impact ------------------------------------------------------------

impact_gdp <- as.numeric(lapply(multip_gdp, function(x) x[1, ]))
impact_intr <- as.numeric(lapply(multip_intr, function(x) x[1, ]))
impact_hicp <- as.numeric(lapply(multip_hicp, function(x) x[1, ]))

gdp_intr <- impact_gdp / impact_intr
quantile(gdp_intr, probs = lower, na.rm = T) #-0.1474907             
quantile(gdp_intr, probs = upper, na.rm = T) #0.1418184            
sd(gdp_intr)/sqrt(length(gdp_intr)) #0.004680508

hicp_intr <- impact_hicp / impact_intr
quantile(hicp_intr, probs = lower, na.rm = T) #-0.2223053              
quantile(hicp_intr, probs = upper, na.rm = T) #-0.04813892              
sd(hicp_intr)/sqrt(length(hicp_intr)) #0.002813181
