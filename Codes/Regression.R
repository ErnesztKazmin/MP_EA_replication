library(ggplot2)
library(ggrepel)
library(MASS)
library(fixest)
library(xtable)
library(psych)
library(Hmisc)
library(StepReg)
library(dplyr)
library(stargazer)
library(summarytools)
library(xtable)
library(dplyr)
library(gridExtra)

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "*", "")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data")

data$inflation <- data$inflation * 100
data$gdp_growth <- data$gdp_growth * 100
data$hpi <- data$hpi * 100
data$relative_rgdp <- data$relative_rgdp * 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL


df <- data[,c(4:11)]
# Compute custom descriptive stats
custom_stats <- data.frame(
  Mean   = sapply(df, mean),
  Median = sapply(df, median),
  SD     = sapply(df, sd),
  Min    = sapply(df, min),
  Max    = sapply(df, max)
)

# Round if desired
custom_stats <- round(custom_stats, 2)

# Convert to LaTeX
xtable(custom_stats)

df2 <- data[,c(1,12)] 

df2 <- df2 %>%
  filter(country != "Lithuania") %>%
  filter(country != "Latvia") %>%
  filter(country != "Estonia")

mean(df2$market_cap) #49.1366
median(df2$market_cap) #37.82578
sd(df2$market_cap) #33.71838
min(df2$market_cap) #5.394723
max(df2$market_cap) #132.6625


# Output plots ------------------------------------------------------------

g1 <- ggplot(data, aes(x = financial_dev, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "FDI", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g2 <-ggplot(data, aes(x = bank_loans, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Bank loans (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g3 <-ggplot(data, aes(x = trade, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Trade (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )


g4 <-ggplot(data, aes(x = relative_gdp_ppp_pc, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic development", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g5 <-ggplot(data, aes(x = relative_rgdp, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic size (% of EA GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g6 <-ggplot(data, aes(x = gov_debt, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Government debt (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g7 <-ggplot(data, aes(x = inflation, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "HICP inflation rate", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g8 <-ggplot(data, aes(x = gdp_growth, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "RGDP growth rate", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g9 <-ggplot(data, aes(x = market_cap, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Market Capitalization (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

grid.arrange(g1, g2, g3, g4, ncol = 2)
grid.arrange(g5, g6, g7, g8, ncol = 2)

g9

# Inflation plots ---------------------------------------------------------

p1 <- ggplot(data, aes(x = financial_dev, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "FDI", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p2 <-ggplot(data, aes(x = bank_loans, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Bank loans (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p3 <-ggplot(data, aes(x = trade, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Trade (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )


p4 <-ggplot(data, aes(x = relative_gdp_ppp_pc, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic development", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p5 <-ggplot(data, aes(x = relative_rgdp, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic size (% of EA GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p6 <-ggplot(data, aes(x = gov_debt, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Government debt (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p7 <-ggplot(data, aes(x = inflation, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "HICP inflation rate", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p8 <-ggplot(data, aes(x = gdp_growth, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "RGDP growth rate", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p9 <-ggplot(data, aes(x = market_cap, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Market Capitalization (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)

p9

corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper", result = "latex")
corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper")
rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")$P
round(rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")$P, 2)

data$market_cap <- NULL

# Scatter plot ------------------------------------------------------------
ggplot(data, aes(x = gdp, y = hicp, label = country)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 8, family = "serif") + 
  labs(
    x = "Output Multiplier of MP", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 18, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 14),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

# Models ------------------------------------------------------------------

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                data = data,
                type = "linear",
                strategy = "bidirection",
                metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.1415  

gdp.aic <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.3865  

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.1415    

gdp.aicc <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.3504     


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.1415    

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.3504     

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.1415     

gdp.sl <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.3504      

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.1415      

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.3865     

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.2347        

gdp.adjRsq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.3865        


regr1 <- feols(hicp ~ relative_gdp_ppp_pc, data = data, vcov = "HC1")
regr2 <- feols(gdp ~ financial_dev, data = data, vcov = "HC1")
etable(regr1, tex = T)
etable(regr2, tex = T)

summary(regr1)
summary(regr2)

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data_H4")

data$inflation <- data$inflation * 100
data$gdp_growth <- data$gdp_growth * 100
data$hpi <- data$hpi * 100
data$relative_rgdp <- data$relative_rgdp * 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL

# Output plots ------------------------------------------------------------

g1 <- ggplot(data, aes(x = financial_dev, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "FDI", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g2 <-ggplot(data, aes(x = bank_loans, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Bank loans (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g3 <-ggplot(data, aes(x = trade, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Trade (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )


g4 <-ggplot(data, aes(x = relative_gdp_ppp_pc, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic development", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g5 <-ggplot(data, aes(x = relative_rgdp, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic size (% of EA GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g6 <-ggplot(data, aes(x = gov_debt, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Government debt (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g7 <-ggplot(data, aes(x = inflation, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "HICP inflation rate", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g8 <-ggplot(data, aes(x = gdp_growth, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "RGDP growth rate", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g9 <-ggplot(data, aes(x = market_cap, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Market Capitalization (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

grid.arrange(g1, g2, g3, g4, ncol = 2)
grid.arrange(g5, g6, g7, g8, ncol = 2)

g9

# Inflation plots ---------------------------------------------------------

p1 <- ggplot(data, aes(x = financial_dev, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "FDI", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p2 <-ggplot(data, aes(x = bank_loans, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Bank loans (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p3 <-ggplot(data, aes(x = trade, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Trade (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )


p4 <-ggplot(data, aes(x = relative_gdp_ppp_pc, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic development", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p5 <-ggplot(data, aes(x = relative_rgdp, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic size (% of EA GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p6 <-ggplot(data, aes(x = gov_debt, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Government debt (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p7 <-ggplot(data, aes(x = inflation, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "HICP inflation rate", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p8 <-ggplot(data, aes(x = gdp_growth, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "RGDP growth rate", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p9 <-ggplot(data, aes(x = market_cap, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Market Capitalization (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)

p9

corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper", result = "latex")
corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper")
round(rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")$P, 2)

data$market_cap <- NULL

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.2081   

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.5357   

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.1676     

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.5357      


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.1676     

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.5357      

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.1676      

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.5357       

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.1676       

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.5357      

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.2667         

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.5441    

regr3 <- feols(hicp ~ relative_gdp_ppp_pc, data = data, vcov = "HC1")
regr4 <- feols(gdp ~ financial_dev  + relative_gdp_ppp_pc, data = data, vcov = "HC1")
etable(regr3, tex = T)
etable(regr4, tex = T)

summary(regr3)
summary(regr4)

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data_H0")

data$inflation <- data$inflation * 100
data$gdp_growth <- data$gdp_growth * 100
data$hpi <- data$hpi * 100
data$relative_rgdp <- data$relative_rgdp * 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL

# Output plots ------------------------------------------------------------

g1 <- ggplot(data, aes(x = financial_dev, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "FDI", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g2 <-ggplot(data, aes(x = bank_loans, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Bank loans (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g3 <-ggplot(data, aes(x = trade, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Trade (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )


g4 <-ggplot(data, aes(x = relative_gdp_ppp_pc, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic development", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g5 <-ggplot(data, aes(x = relative_rgdp, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic size (% of EA GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g6 <-ggplot(data, aes(x = gov_debt, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Government debt (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g7 <-ggplot(data, aes(x = inflation, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "HICP inflation rate", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g8 <-ggplot(data, aes(x = gdp_growth, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "RGDP growth rate", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

g9 <-ggplot(data, aes(x = market_cap, y = gdp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Market Capitalization (% of GDP)", 
    y = "Output Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

grid.arrange(g1, g2, g3, g4, ncol = 2)
grid.arrange(g5, g6, g7, g8, ncol = 2)

g9

# Inflation plots ---------------------------------------------------------

p1 <- ggplot(data, aes(x = financial_dev, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "FDI", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p2 <-ggplot(data, aes(x = bank_loans, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Bank loans (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p3 <-ggplot(data, aes(x = trade, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Trade (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )


p4 <-ggplot(data, aes(x = relative_gdp_ppp_pc, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic development", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p5 <-ggplot(data, aes(x = relative_rgdp, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Economic size (% of EA GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p6 <-ggplot(data, aes(x = gov_debt, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Government debt (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p7 <-ggplot(data, aes(x = inflation, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "HICP inflation rate", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p8 <-ggplot(data, aes(x = gdp_growth, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "RGDP growth rate", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

p9 <-ggplot(data, aes(x = market_cap, y = hicp, label = cc)) +
  geom_point(color = "black", size = 3, shape = 16) + 
  geom_text_repel(size = 6, family = "serif") + 
  labs(
    x = "Market Capitalization (% of GDP)", 
    y = "Inflation Multiplier of MP"
  ) +
  theme_bw(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 20),  # Increase axis text size
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

grid.arrange(p1, p2, p3, p4, ncol = 2)
grid.arrange(p5, p6, p7, p8, ncol = 2)

p9

corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper", result = "latex")
corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper")
round(rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")$P, 2)

data$market_cap <- NULL

# Correlation -------------------------------------------------------------

cor(data[,c(2:11)])
mcor<-round(cor(data[,c(2:11)]),2)

corstars(data[,c(2:11)], method = "pearson", removeTriangle = "upper")

rcorr(as.matrix(data[,c(2:11)]),  type = "pearson")

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.3105      

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.5311     

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc)  #0.278 

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.5311       


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.278      

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.5311      

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.278       

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.5311        

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.278         

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.5311       

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.3105          

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.5311    

regr5 <- feols(hicp ~ relative_rgdp + financial_dev, data = data, vcov = "HC1")
regr6 <- feols(gdp ~ financial_dev  + relative_gdp_ppp_pc, data = data, vcov = "HC1")
etable(regr5, tex = T)
etable(regr6, tex = T)

summary(regr5)
summary(regr6)

etable(regr1, regr3, regr5)
etable(regr2, regr4, regr6)

etable(regr1, regr3, regr5, tex = T)
etable(regr2, regr4, regr6, tex = T)

# Models without Ireland ------------------------------------------------------------------
# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data")

data$bank_loans <- data$bank_loans / 100
data$trade <- data$trade / 100
data$stock_r <- data$stock_r / 100
data$volatility <- data$volatility / 100
data$gov_debt <- data$gov_debt / 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL
data$market_cap <- NULL

data <- data %>%
  filter(country != "Ireland")

cor(data[,c(2:11)])

mcor<-round(cor(data[,c(2:11)]),2)
corstars(data[,c(2:11)], method = "pearson", removeTriangle = "upper")


## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.1318   

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.3284   

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.1318 

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.3284      


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.1318     

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.3284      

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.1318      

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.3284       

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.1318       

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.3284      

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.2676        

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.3474         

regr1 <- feols(hicp ~ relative_gdp_ppp_pc, data = data, vcov = "HC1")
regr2 <- feols(gdp ~ financial_dev, data = data, vcov = "HC1")
etable(regr1, tex = T)
etable(regr2, tex = T)

summary(regr1)
summary(regr2)

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data_H4")

data$bank_loans <- data$bank_loans / 100
data$trade <- data$trade / 100
data$stock_r <- data$stock_r / 100
data$volatility <- data$volatility / 100
data$gov_debt <- data$gov_debt / 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL
data$market_cap <- NULL

data <- data %>%
  filter(country != "Ireland")

# Correlation -------------------------------------------------------------

cor(data[,c(2:11)])

mcor<-round(cor(data[,c(2:11)]),2)
corstars(data[,c(2:11)], method = "pearson", removeTriangle = "upper")

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.1569   

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.5194   

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.1569     

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.5035      


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.1569     

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.5035      

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.1569      

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.5035       

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.1569       

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.5194      

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.319         

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.536    

regr3 <- feols(hicp ~ relative_gdp_ppp_pc, data = data, vcov = "HC1")
regr4 <- feols(gdp ~ financial_dev+relative_gdp_ppp_pc, data = data, vcov = "HC1")
etable(regr3, tex = T)
etable(regr4, tex = T)

summary(regr3)
summary(regr4)

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data_H0")

data$bank_loans <- data$bank_loans / 100
data$trade <- data$trade / 100
data$stock_r <- data$stock_r / 100
data$volatility <- data$volatility / 100
data$gov_debt <- data$gov_debt / 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL
data$market_cap <- NULL

data <- data %>%
  filter(country != "Ireland")

# Correlation -------------------------------------------------------------

cor(data[,c(2:11)])

mcor<-round(cor(data[,c(2:11)]),2)
corstars(data[,c(2:11)], method = "pearson", removeTriangle = "upper")

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.2873     

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.4989    

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc)  

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.4989       


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.2365      

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.4989       

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.2365       

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.4989        

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.2873         

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.4989       

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.2873          

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.5571     

regr5 <- feols(hicp ~ inflation+bank_loans, data = data, vcov = "HC1")
regr6 <- feols(gdp ~ financial_dev  + relative_gdp_ppp_pc, data = data, vcov = "HC1")
etable(regr5, tex = T)
etable(regr6, tex = T)

summary(regr5)
summary(regr6)

etable(regr1, regr3, regr5)
etable(regr2, regr4, regr6)

etable(regr1, regr3, regr5, tex = T)
etable(regr2, regr4, regr6, tex = T)

# Models with Market Capitalization ------------------------------------------------------------------
# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data")

data$inflation <- data$inflation * 100
data$gdp_growth <- data$gdp_growth * 100
data$hpi <- data$hpi * 100
data$relative_rgdp <- data$relative_rgdp * 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL

data <- data %>%
  filter(country != "Lithuania") %>%
  filter(country != "Latvia") %>%
  filter(country != "Estonia")
  
corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper")
rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")


## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.2599    

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.2845    

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.2599  

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.2453        

## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.2599      

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.2453       

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.2599       

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.2453        

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.2599        

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.2453       

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.3844          

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.3372          

regr1 <- feols(hicp ~ market_cap, data = data, vcov = "HC1")
regr2 <- feols(gdp ~ financial_dev, data = data, vcov = "HC1")
etable(regr1, tex = T)
etable(regr2, tex = T)

summary(regr1)
summary(regr2)

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data_H4")

data$inflation <- data$inflation * 100
data$gdp_growth <- data$gdp_growth * 100
data$hpi <- data$hpi * 100
data$relative_rgdp <- data$relative_rgdp * 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL

data <- data %>%
  filter(country != "Lithuania") %>%
  filter(country != "Latvia") %>%
  filter(country != "Estonia")

corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper")
rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.2909

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.4092    

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.2909      

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.3216       


## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.2909      

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.3216       

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.2909       

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.4092        

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.2909        

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.4092       

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.2909         

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.4215     

regr3 <- feols(hicp ~ market_cap, data = data, vcov = "HC1")
regr4 <- feols(gdp ~ relative_gdp_ppp_pc, data = data, vcov = "HC1")
etable(regr3, tex = T)
etable(regr4, tex = T)

summary(regr3)
summary(regr4)

# Data --------------------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/mp_multiplicator.xlsx", sheet = "data_H0")

data$inflation <- data$inflation * 100
data$gdp_growth <- data$gdp_growth * 100
data$hpi <- data$hpi * 100
data$relative_rgdp <- data$relative_rgdp * 100

data$stock_r <- NULL
data$volatility <- NULL
data$hpi <- NULL
data$fin_depth <- NULL

data <- data %>%
  filter(country != "Lithuania") %>%
  filter(country != "Latvia") %>%
  filter(country != "Estonia")

corstars(data[,c(2:12)], method = "pearson", removeTriangle = "upper")
rcorr(as.matrix(data[,c(2:12)]),  type = "pearson")

## AIC -------------------------------------------------------------------

formula.hicp <- hicp ~ .-gdp-country-cc
formula.gdp <- gdp ~ .-hicp-country-cc

hicp.aic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AIC"))

summary(hicp.aic$bidirection$AIC) #0.4238       

gdp.aic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("AIC"))

summary(gdp.aic$bidirection$AIC) #0.3284     

## AICc -------------------------------------------------------------------

hicp.aicc <- stepwise(formula = formula.hicp,
                      data = data,
                      type = "linear",
                      strategy = "bidirection",
                      metric = c("AICc"))

summary(hicp.aicc$bidirection$AICc) #0.3673    

gdp.aicc <- stepwise(formula = formula.gdp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("AICc"))

summary(gdp.aicc$bidirection$AICc) #0.3284        

## BIC -------------------------------------------------------------------

hicp.bic <- stepwise(formula = formula.hicp,
                     data = data,
                     type = "linear",
                     strategy = "bidirection",
                     metric = c("BIC"))

summary(hicp.bic$bidirection$BIC) #0.3568         

gdp.bic <- stepwise(formula = formula.gdp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("BIC"))

summary(gdp.bic$bidirection$BIC) #0.3284        

## SL -------------------------------------------------------------------

hicp.sl <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("SL"))

summary(hicp.sl$bidirection$SL) #0.3673          

gdp.sl <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("SL"))

summary(gdp.sl$bidirection$SL) #0.3284         

## HQ -------------------------------------------------------------------

hicp.hq <- stepwise(formula = formula.hicp,
                    data = data,
                    type = "linear",
                    strategy = "bidirection",
                    metric = c("HQ"))

summary(hicp.hq$bidirection$HQ) #0.4238           

gdp.hq <- stepwise(formula = formula.gdp,
                   data = data,
                   type = "linear",
                   strategy = "bidirection",
                   metric = c("HQ"))

summary(gdp.hq$bidirection$HQ) #0.3284        

## adjRsq -------------------------------------------------------------------

hicp.adjRsq <- stepwise(formula = formula.hicp,
                        data = data,
                        type = "linear",
                        strategy = "bidirection",
                        metric = c("adjRsq"))

summary(hicp.adjRsq$bidirection$adjRsq) #0.4238            

gdp.adjRsq <- stepwise(formula = formula.gdp,
                       data = data,
                       type = "linear",
                       strategy = "bidirection",
                       metric = c("adjRsq"))

summary(gdp.adjRsq$bidirection$adjRsq) #0.3443      

regr5 <- feols(hicp ~ inflation+bank_loans, data = data, vcov = "HC1")
regr6 <- feols(gdp ~ relative_gdp_ppp_pc, data = data, vcov = "HC1")
etable(regr5, tex = T)
etable(regr6, tex = T)

summary(regr5)
summary(regr6)

etable(regr1, regr3, regr5)
etable(regr2, regr4, regr6)

etable(regr1, regr3, regr5, tex = T)
etable(regr2, regr4, regr6, tex = T)

