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

paste("C:/Users/ernes/Desktop/MP Data/Replication/varsignr/", list.files("C:/Users/ernes/Desktop/MP Data/Replication/varsignr") , sep = '') %>% lapply(source)

# Data Preparation --------------------------------------------------------
data <- readxl::read_xlsx("C:/Users/ernes/Desktop/MP Data/Replication/Final Data/data_q.xlsx")
data$date <- as.Date(data$date, tryFormats = "%Y-%m-%d")

data$dlgdp <- c(NA, diff(log(data$gdp)))

data$hicp_d <- c(NA, diff(data$hicp))

#data <- data[40:100,] #2004Q4 - 2019Q4 T = 61
data <- data[17:99,] #1999Q1 - 2019Q3 T = 83

tseries::adf.test(data$hicp_d) # Stationary
tseries::kpss.test(data$hicp_d) # Stationary

tseries::adf.test(data$dlgdp) # Stationary
tseries::kpss.test(data$dlgdp) # Stationary

tseries::adf.test(data$ns_d) # Stationary
tseries::kpss.test(data$ns_d) # Stationary

# Shadow MPS --------------------------------------------------------------

VARselect(data[1:nrow(data), c("hicp_d", "dlgdp", "ns_d")], lag.max = 4)

var_shadow <- vars::VAR(data[1:nrow(data), c("hicp_d", "dlgdp", "ns_d")],
                        p = 1,
                        type = "const")
summary(var_shadow)
# Stable model

serial.test(var_shadow)
serial.test(var_shadow, type = "PT.adjusted")
# No autocorrelation

plot(irf(var_shadow, impulse = "ns_d", response = "hicp_d", n.ahead = 12, ortho = T, seed = 10))
plot(irf(var_shadow, impulse = "ns_d", response = "dlgdp", n.ahead = 12, ortho = T, seed = 10))
plot(irf(var_shadow, impulse = "ns_d", response = "ns_d", n.ahead = 12, ortho = T, seed = 10))

## Sign-restrictions -------------------------------------------------------

set.seed(1234)
sign_data1 <-data[1:nrow(data), c("ns_d", "hicp_d", "dlgdp")] %>% 
  ts()

signr <- rwz.reject(Y = sign_data1, nlags = 1, draws = 2000, subdraws = 1000, nkeep = 1000,
                    KMIN = 1, KMAX = 2, constant = T, steps = 21, constrained = c(+1, -2, -3))

signr[['SHOCKS']] %>%
  as_tibble() %>% 
  gather(time, value) %>% 
  mutate(time = str_remove_all(time, 'V'),
         time = as.numeric(time)) %>% 
  group_by(time) %>% 
  summarize(median = median(value),
            mean = mean(value)) %>% 
  arrange(time) %>% 
  gather(key, value, median, mean) %>% 
  ggplot(aes(x = time, value, color = key)) +
  geom_line()

shadow_mps <- signr[['SHOCKS']] %>%
  as_tibble() %>% 
  gather(time, value) %>% 
  mutate(time = str_remove_all(time, 'V'),
         time = as.numeric(time)) %>% 
  group_by(time) %>% 
  summarize(median = median(value),
            mean = mean(value)) %>% 
  arrange(time) %>% 
  gather(key, value, median, mean)

irfs1 <- signr$IRFS

irfs1


shadow_mps_plot <- signr[['SHOCKS']] %>%
  as_tibble() %>% 
  gather(time, value) %>% 
  mutate(time = str_remove_all(time, 'V'),
         time = as.numeric(time)) %>% 
  group_by(time) %>% 
  summarize(median = median(value)) %>% 
  arrange(time) %>% 
  gather(key, value, median)

data$shock <- c(NA, shadow_mps_plot$value)

ggplot(data, aes(x = date, y = shock)) +
  geom_line(color = "#1b1b1b", size = 1) +  # Dark gray line for time series
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Zero line
  labs(x = "Year", y = "(%-point)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Yearly x-axis labels
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    panel.grid.major = element_line(size = 0.3, linetype = "dotted", color = "gray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

## IRF Plots -------------------------------------------------------------
hicp <- irfs1[,,"hicp_d"]

ci <- 0.05
lower <- ci / 2
upper <- 1 - ci / 2
hall <- 0.32
hlower <- hall / 2
hupper <- 1 - hall / 2
fift <- 0.50
flower <- fift / 2
fupper <- 1 - fift / 2

hicp <- hicp %>% as_tibble() %>% 
  gather(time, value) %>% 
  mutate(time = str_remove_all(time, 'V'),
         time = as.numeric(time)) %>% 
  group_by(time) %>% 
  summarize(median = median(value),
            mean = mean(value),
            mat.u = quantile(value, upper),
            mat.l = quantile(value, lower),
            mat.hu = quantile(value, hupper),
            mat.hl = quantile(value, hlower),
            mat.fu = quantile(value, fupper),
            mat.fl = quantile(value, flower)
  ) %>% 
  arrange(time) %>% 
  gather(key, value, median, mean, mat.u, mat.l, mat.hu, mat.hl, mat.fu, mat.fl)

hicp_wide <- hicp %>%
  pivot_wider(
    names_from = key,         # The 'key' column contains the feature names (e.g., "median", "mean", etc.)
    values_from = value       # The 'value' column contains the values for each feature
  )

hicp_wide$t <- 1:nrow(hicp_wide) - 1

hicp_wide <- hicp_wide %>%
  mutate(facet_label = "Monetary Policy Shock → HICP inflation rate ")

prices_shadow <- ggplot(data = hicp_wide) +
  # Confidence Intervals (CIs) 
  geom_ribbon(aes(x = t, ymin = mat.l * 100, ymax = mat.u * 100), fill = "#bdbdbd", alpha = 0.5) +  # 95% CI
  geom_ribbon(aes(x = t, ymin = mat.hl * 100, ymax = mat.hu * 100), fill = "#a8a8a8", alpha = 0.5) +  # 68% CI
  
  # Impulse Response Function (IRF) Line
  geom_line(aes(x = t, y = median * 100), color = "#1b1b1b", size = 1) +
  
  # Horizontal Line at Zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Labels
  labs(
    x = "Quarters",
    y = "(%-point)"
  ) +
  
  # Scale X Axis with Custom Breaks
  scale_x_continuous(
    breaks = seq(min(hicp_wide$t), max(hicp_wide$t), by = 5)  
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

gdp <- irfs1[,,"dlgdp"]

gdp <- gdp %>% as_tibble() %>% 
  gather(time, value) %>% 
  mutate(time = str_remove_all(time, 'V'),
         time = as.numeric(time)) %>% 
  group_by(time) %>% 
  summarize(median = median(value),
            mean = mean(value),
            mat.u = quantile(value, upper),
            mat.l = quantile(value, lower),
            mat.hu = quantile(value, hupper),
            mat.hl = quantile(value, hlower),
            mat.fu = quantile(value, fupper),
            mat.fl = quantile(value, flower)
  ) %>% 
  arrange(time) %>% 
  gather(key, value, median, mean, mat.u, mat.l, mat.hu, mat.hl, mat.fu, mat.fl)

gdp_wide <- gdp %>%
  pivot_wider(
    names_from = key,         # The 'key' column contains the feature names (e.g., "median", "mean", etc.)
    values_from = value       # The 'value' column contains the values for each feature
  )

gdp_wide$t <- 1:nrow(gdp_wide) - 1

gdp_wide <- gdp_wide %>%
  mutate(facet_label = "Monetary Policy Shock → RGDP growth rate")

gdp_shadow <- ggplot(data = gdp_wide) +
  # Confidence Intervals (CIs) 
  geom_ribbon(aes(x = t, ymin = mat.l * 100, ymax = mat.u * 100), fill = "#bdbdbd", alpha = 0.5) +  # 95% CI
  geom_ribbon(aes(x = t, ymin = mat.hl * 100, ymax = mat.hu * 100), fill = "#a8a8a8", alpha = 0.5) +  # 68% CI
  
  # Impulse Response Function (IRF) Line
  geom_line(aes(x = t, y = median * 100), color = "#1b1b1b", size = 1) +
  
  # Horizontal Line at Zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Labels
  labs(
    x = "Quarters",
    y = "(100 x log)"
  ) +
  
  # Scale X Axis with Custom Breaks
  scale_x_continuous(
    breaks = seq(min(gdp_wide$t), max(gdp_wide$t), by = 5)  
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

shadow <- irfs1[,,"ns_d"]

shadow <- shadow %>% as_tibble() %>% 
  gather(time, value) %>% 
  mutate(time = str_remove_all(time, 'V'),
         time = as.numeric(time)) %>% 
  group_by(time) %>% 
  summarize(median = median(value),
            mean = mean(value),
            mat.u = quantile(value, upper),
            mat.l = quantile(value, lower),
            mat.hu = quantile(value, hupper),
            mat.hl = quantile(value, hlower),
            mat.fu = quantile(value, fupper),
            mat.fl = quantile(value, flower)
  ) %>% 
  arrange(time) %>% 
  gather(key, value, median, mean, mat.u, mat.l, mat.hu, mat.hl, mat.fu, mat.fl)

shadow_wide <- shadow %>%
  pivot_wider(
    names_from = key,         # The 'key' column contains the feature names (e.g., "median", "mean", etc.)
    values_from = value       # The 'value' column contains the values for each feature
  )

shadow_wide$t <- 1:nrow(shadow_wide) - 1

shadow_wide <- shadow_wide %>%
  mutate(facet_label = "Monetary Policy Shock → Shadow rate")

shadow_resp <- ggplot(data = shadow_wide) +
  # Confidence Intervals (CIs) 
  geom_ribbon(aes(x = t, ymin = mat.l * 100, ymax = mat.u * 100), fill = "#bdbdbd", alpha = 0.5) +  # 95% CI
  geom_ribbon(aes(x = t, ymin = mat.hl * 100, ymax = mat.hu * 100), fill = "#a8a8a8", alpha = 0.5) +  # 68% CI
  
  # Impulse Response Function (IRF) Line
  geom_line(aes(x = t, y = median * 100), color = "#1b1b1b", size = 1) +
  
  # Horizontal Line at Zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  
  # Labels
  labs(
    x = "Quarters",
    y = "(%-point)"
  ) +
  
  # Scale X Axis with Custom Breaks
  scale_x_continuous(
    breaks = seq(min(shadow_wide$t), max(shadow_wide$t), by = 5)  
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



gdp_shadow
prices_shadow
shadow_resp

grid.arrange(prices_shadow, gdp_shadow, shadow_resp, ncol = 1)

# Saving ------------------------------------------------------------------

shadow_mps_wide <- shadow_mps %>%
  pivot_wider(
    names_from = key,         # The 'key' column contains the feature names (e.g., "median", "mean", etc.)
    values_from = value       # The 'value' column contains the values for each feature
  )

shadow_mps <- as.data.frame(shadow_mps_wide$median)
colnames(shadow_mps)[colnames(shadow_mps) == "shadow_mps_wide$median"] <- "shadow"

df <- as.data.frame(data[, c(1, 6:7)])
df$new_shadow_mps <- c(NA, shadow_mps$shadow)

write.csv(df, "C:/Users/ernes/Desktop/MP Data/Replication/Final Data/new_shadow_shocks.csv")