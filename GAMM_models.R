library(mgcv) 
library(nlme)
library(gratia)
library(gridExtra)
library(patchwork)
library(viridis)

#### Import ALL.LS data frame created by adding all the output data
ALL.LS <- read_csv("C:/Users/sasin/Sync/Sasindu/Trent Metabolism/Lake Superior/Vachan_P_Output/Other_water quality/All.LS.csv")
ALL.LS <- ALL.LS[complete.cases(ALL.LS$abs.ER), ]



## Import and combine distance data
distance <- read.csv(file.choose(), header = T)
distance$Date<-as.POSIXct(paste(distance$Year,"-",distance$Month,"-",distance$Day," ",distance$Hour,
                                sep = ""), tz = "GMT")

daily_dist <- distance %>%
  group_by(Date) %>%
  summarise(Avg_DIST = mean(NEAR_DIST, na.rm = TRUE))

ALL.LS <- left_join(ALL.LS,daily_dist, by = "Date") %>% arrange(Date)

# import and combine RTRM data
Avg_rtrm <- read.csv(file.choose(), header = T)
Avg_rtrm$Date<- as.Date(Avg_rtrm$Date, format = "%Y-%m-%d")

ALL.LS <- left_join(ALL.LS,Avg_rtrm, by = "Date") %>% arrange(Date)

# Import and combine night time chlorophyll data
epi_chl_all <- read.csv(file.choose(), header = T)
ALL.LS <- merge(ALL.LS, Avg_rtrm, by = "Date", all.x = TRUE)





# Create the coplots
coplot(abs.ER ~ month | year, data = ALL.LS,
       xlab = "Month", ylab = "ER")

coplot(abs.ER ~ month | mission, data = ALL.LS,
       xlab = "Month", ylab = "ER")

coplot(GPP.mean ~ month | year, data = ALL.LS,
       xlab = "Month", ylab = "GPP")

coplot(NEP ~ month | year, data = ALL.LS,
       xlab = "Month", ylab = "NEP")

# Calculate correlation matrix
numeric_data <- ALL.LS[sapply(ALL.LS, is.numeric)]
correlation_matrix <- cor(numeric_data)
correlation_df <- as.data.frame(correlation_matrix)
print(correlation_df)

write.csv(correlation_df, "C:/Users/sasin/Sync/Sasindu/Trent Metabolism/Lake Superior/Vachan_P_Output/correlation_df.csv")

#conditional box plots for month
bwplot.GPP <- bwplot(GPP.mean ~ month, data = ALL.LS,
                     xlab = "Month", ylab = "GPP")
bwplot.ER <- bwplot(abs.ER ~ month, data = ALL.LS,
                    xlab = "Month", ylab = "ER")
bwplot.NEP <- bwplot(NEP ~ month, data = ALL.LS,
                     xlab = "Month", ylab = "NEP")

grid.arrange(bwplot.GPP, bwplot.ER, bwplot.NEP, ncol = 1)


#conditional box plots for year
ALL.LS$year <- factor(ALL.LS$year)
ALL.LS$mission <- factor(ALL.LS$mission)

bwplot.GPP.mi <- bwplot(GPP.mean ~ year, data = ALL.LS,
                     xlab = "Year", ylab = "GPP")

bwplot.ER.mi <- bwplot(abs.ER ~ year, data = ALL.LS,
                    xlab = "Year", ylab = "ER")
bwplot.NEP.mi <- bwplot(NEP ~ year, data = ALL.LS,
                     xlab = "Year", ylab = "NEP")

grid.arrange(bwplot.GPP.mi, bwplot.ER.mi, bwplot.NEP.mi, ncol = 1)

########### For ER #######
### formula to show relationshp between ER with temperature and Distance from shore
f1 <- formula(abs.ER ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr"))

f2 <- formula(abs.ER ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr")+
                s(Chl, bs = "cr"))
f3 <- formula(log(abs.ER) ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr"))


### autocorelation stucture selection 
m1 <- gamm(f1, method = "REML", data = ALL.LS, family = Gamma(link = "log"))

m2 <- gamm(f1, method = "REML", correlation = corARMA(form = ~1 | Date, p = 1),
           data = ALL.LS, family = Gamma(link = "log"))

m3 <- gamm(f1, method = "REML", correlation = corAR1(form = ~1 | Date),
           data = ALL.LS, family = Gamma(link = "log"))


AIC(m1$lme, m2$lme, m3$lme) # I chose correlation = corAR1


####
m3 <- gamm(f1, method = "REML",random=list(year=~1,month=~1),
           correlation = corAR1(form = ~1 | Date),
           data = ALL.LS, family = Gamma(link = "log"))


m4 <- gamm(f2, method = "REML",random=list(year=~1,month=~1),
           correlation = corAR1(form = ~1 | Date),
           data = ALL.LS, family = Gamma(link = "log"))

m5 <- gamm(f3, method = "REML",random=list(year=~1,month=~1),
           correlation = corAR1(form = ~1 | Date),
           data = ALL.LS)


AIC(m3$lme, m4$lme, m5$lme) # I chose m3

## Model summary and checks
m3$gam %>% summary()
gam.check(m3$gam)
appraise(m3$gam)
draw(m3)



### For GPP
f1.G <- formula(GPP.mean ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr"))

f2.G <- formula(GPP.mean ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr")+
                  s(Chl, bs = "cr"))

f3.G <- formula(log(GPP.mean) ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr"))


m3.G <- gamm(f1.G, method = "REML",random=list(year=~1,month=~1),
             correlation = corAR1(form = ~1 | Date),
             data = ALL.LS, family = Gamma(link = "log"))


m4.G <- gamm(f2.G, method = "REML",random=list(year=~1,month=~1),
             correlation = corAR1(form = ~1 | Date),
             data = ALL.LS, family = Gamma(link = "log"))

m5.G <- gamm(f3.G, method = "REML",random=list(year=~1,month=~1),
             correlation = corAR1(form = ~1 | Date),
             data = ALL.LS)

AIC(m3.G$lme, m4.G$lme) # I chose m3.G

## Model summary and checks
m3.G$gam %>% summary()
gam.check(m3.G$gam)
appraise(m3$gam)
draw(m3)


### For NEP

f1.nep <- formula((NEP) ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr"))

f2.nep <- formula(NEP ~ s(temperature, bs = "cr") + s(Avg_DIST, bs = "cr")+
                    s(Chl, bs = "cr"))


m3.nep <- gamm(f1.nep, method = "REML",random=list(year=~1,month=~1),
               correlation = corAR1(form = ~1 | Date),
               data = ALL.LS)

m4.nep <- gamm(f2.nep, method = "REML",random=list(year=~1,month=~1),
               correlation = corAR1(form = ~1 | Date),
               data = ALL.LS)

m5.nep <- gamm(f3.nep, method = "REML",random=list(year=~1,month=~1),
               correlation = corAR1(form = ~1 | Date),
               data = ALL.LS)

AIC(m3.nep$lme, m4.nep$lme) # I chose m3.nep

m3.nep$gam %>% summary()
gam.check(m3.nep$gam)
appraise(m3.nep$gam)
draw(m3.nep)



##### plot 
###### With Temperature #############
###### ER #########

# Create new data with temperature and Avg_DIST values for prediction
new_data <- data.frame(temperature = seq(min(ALL.LS$temperature), max(ALL.LS$temperature), length.out = 100),
                       Avg_DIST = mean(ALL.LS$Avg_DIST))  

# Predict abs.ER using the GAMM model
predictions <- predict(m3$gam, newdata = new_data, type = "response")

# Combine predicted values with new_data
predictions_df <- data.frame(new_data, abs_ER_predicted = predictions)



pred_df <- data.frame(new_data, predictions)
conf_intervals <- predict(m3$gam, newdata = new_data, se.fit = TRUE)$se.fit * qt(0.975, m3$gam$df.residual)  # Calculate 95% confidence intervals

# Combine predicted values and confidence intervals
predictions_df <- cbind(pred_df, lower_ci = pred_df$predictions - conf_intervals, upper_ci = pred_df$predictions + conf_intervals)

# Plot with confidence intervals
# Define the custom color scale
custom_colors <- c(plasma(5), plasma(10, begin = 0.5, end = 1))  # Use plasma palette, first 5 colors for 0-30, next 30 colors for 30-40

# Plot with confidence intervals
ER.gam.T <- ggplot() +
  geom_line(data = predictions_df, aes(x = temperature, y = predictions), color = "red", size = 1.2) +
  geom_ribbon(data = predictions_df, aes(x = temperature, ymin = lower_ci, ymax = upper_ci), fill = "lightcoral", alpha = 0.1) +
  geom_point(data = ALL.LS, aes(x = temperature, y = abs.ER, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_colors, limits = c(0, 40), name = "Mean daily RTRM") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.x = element_blank()
  ) +
  ylab(expression(bold("ER (g O"[2]~m^{-3}~day^{-1}*")"))) +
  labs(x = " ") +
  theme_minimal() +
  theme(legend.position = "none", legend.direction = "horizontal")

ER.gam.T

#### I used this one ###
# Define the custom color palette
custom_palette <- c(colorRampPalette(c("lightblue", "darkblue"))(8), rev(plasma(32, begin = 0.5, end = 1)))

# Plot with modified color palette
ER.gam.T <- ggplot() +
  geom_line(data = predictions_df, aes(x = temperature, y = predictions), color = "red", size = 1.2) +
  geom_ribbon(data = predictions_df, aes(x = temperature, ymin = lower_ci, ymax = upper_ci), fill = "lightcoral", alpha = 0.1) +
  geom_point(data = ALL.LS, aes(x = temperature, y = abs.ER, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Mean daily RTRM") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.x = element_blank()
  ) +
  ylab(expression(bold("ER (g O"[2]~m^{-3}~day^{-1}*")"))) +
  labs(x = " ") +
  theme_minimal() +
  theme(legend.position = "", legend.direction = "horizontal")

ER.gam.T

##### GPP #########

# Create new data with temperature and Avg_DIST values for prediction
new_data_G <- data.frame(temperature = seq(min(ALL.LS$temperature), max(ALL.LS$temperature), length.out = 100),
                         Avg_DIST = mean(ALL.LS$Avg_DIST))  

# Predict GPP using the GAMM model (m3.G)
predictions_G <- predict(m3.G$gam, newdata = new_data_G, type = "response")

# Combine predicted values with new_data_G
predictions_df_G <- data.frame(new_data_G, GPP_predicted = predictions_G)

# Calculate 95% confidence intervals
conf_intervals_G <- predict(m3.G$gam, newdata = new_data_G, se.fit = TRUE)$se.fit * qt(0.975, m3.G$gam$df.residual)

# Combine predicted values and confidence intervals
predictions_df_G <- cbind(predictions_df_G, lower_ci = predictions_df_G$GPP_predicted - conf_intervals_G, upper_ci = predictions_df_G$GPP_predicted + conf_intervals_G)


# Define the custom color scale
custom_palette <- c(colorRampPalette(c("lightblue", "darkblue"))(8), rev(plasma(32, begin = 0.5, end = 1)))

# Plot with confidence intervals
GPP.gam.T <- ggplot() +
  geom_line(data = predictions_df_G, aes(x = temperature, y = GPP_predicted), color = "darkgreen", size = 1.2) +
  geom_ribbon(data = predictions_df_G, aes(x = temperature, ymin = pmax(lower_ci, 0), ymax = upper_ci), fill = "lightgreen", alpha = 0.1) +
  geom_point(data = ALL.LS, aes(x = temperature, y = GPP.mean, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Mean daily RTRM") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.x = element_blank()
  ) +
  ylab(expression(bold("GPP (g O"[2]~m^{-3}~day^{-1}*")"))) +
  xlab("")+
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal")

GPP.gam.T

### Without correcting for CI

GPP.gam.T <- ggplot() +
  geom_line(data = predictions_df_G, aes(x = temperature, y = GPP_predicted), color = "darkgreen", size = 1.2) +
  geom_ribbon(data = predictions_df_G, aes(x = temperature, ymin = lower_ci, ymax = upper_ci), fill = "lightgreen", alpha = 0.1) +
  geom_point(data = ALL.LS, aes(x = temperature, y = GPP.mean, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Mean daily RTRM") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.x = element_blank()
  ) +
  ylab(expression(bold("GPP (g O"[2]~m^{-3}~day^{-1}*")"))) +
  xlab(expression(bold("Temperature (°C)")))+
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal")

GPP.gam.T





#### NEP ######

# Define the custom color scale
custom_palette <- c(colorRampPalette(c("lightblue", "darkblue"))(8), rev(plasma(32, begin = 0.5, end = 1)))

# Plot for NEP with custom color scale
NEP.gam.T <- ggplot() +
  geom_point(data = ALL.LS, aes(x = temperature, y = NEP, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Mean daily RTRM") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(family = "Times New Roman", size = 16, face = "bold"),  # Make x-axis title bold
    axis.text.x = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.x = element_blank()
  ) +
  ylab(expression(bold("NEP (g O"[2]~m^{-3}~day^{-1}*")"))) +
  xlab(expression(bold("Temperature (°C)"))) + # Bold x-axis label
  theme_minimal() +
  theme(legend.position = "", legend.direction = "horizontal")

NEP.gam.T


# Arrange plots vertically
stacked_plots.T <- GPP.gam.T / ER.gam.T / NEP.gam.T
stacked_plots.T

# for power point
side_by_side <- GPP.gam.T + ER.gam.T + plot_layout(ncol = 2)
side_by_side





# Save the combined plot with a white background
ggsave(
  filename = "C:/Users/sasin/OneDrive/Documents/Glider/Figures/stacked_plots.T2.png",
  plot = stacked_plots.T,
  device = "png",
  width = 15,
  height = 20,
  units = "cm",
  dpi = 1000,
  scale = 1.5,
  bg = "white"
)

# Save the combined plot with a white background for the presentations
ggsave(
  filename = "C:/Users/sasin/OneDrive/Documents/Glider/Figures/side_by_side_plots.T_pp.png",
  plot = side_by_side,
  device = "png",
  width = 30,
  height = 12,
  units = "cm",
  dpi = 1000,
  scale = 1.5,
  bg = "white"
)

ggsave(
  filename = "C:/Users/sasin/OneDrive/Documents/Glider/Figures/NEP.gam.T_pp.png",
  plot = NEP.gam.T,
  device = "png",
  width = 15,
  height = 10,
  units = "cm",
  dpi = 1000,
  scale = 1.5,
  bg = "white"
)
###### With Distance ######

###### ER #########

new_data <- data.frame(Avg_DIST = seq(min(ALL.LS$Avg_DIST), max(ALL.LS$Avg_DIST), length.out = 100),
                       temperature = mean(ALL.LS$temperature))  
predictions <- predict(m3$gam, newdata = new_data, type = "response", se.fit = TRUE)

# Extract predicted values and standard errors
predicted_values <- predictions$fit
standard_errors <- predictions$se.fit

# Calculate confidence intervals
confidence_intervals <- qt(0.975, df = m3$gam$df.residual) * standard_errors

# Combine predicted values and confidence intervals into a dataframe
predictions_df <- data.frame(Avg_DIST = new_data$Avg_DIST,
                             abs_ER_predicted = predicted_values,
                             lower_ci = predicted_values - confidence_intervals,
                             upper_ci = predicted_values + confidence_intervals)

# Plot with confidence intervals
custom_palette <- c(colorRampPalette(c("lightblue", "darkblue"))(10), rev(plasma(30, begin = 0.5, end = 1)))

# Plot with confidence intervals and custom colors
ER_gam_D <- ggplot() +
  geom_line(data = predictions_df, aes(x = Avg_DIST, y = abs_ER_predicted), color = "red", size = 1.2, alpha = 0.5) +
  geom_ribbon(data = predictions_df, aes(x = Avg_DIST, ymin = lower_ci, ymax = upper_ci), fill = "lightcoral", alpha = 0.2) +
  geom_point(data = ALL.LS, aes(x = Avg_DIST, y = abs.ER, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Mean daily RTRM") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 14, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text.y = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.y = element_blank() # Remove y-axis ticks
  ) +
  xlab(expression(bold(""))) +
  ylab(expression(bold("ER (g O"[2]~m^{-3}~day^{-1}*")"))) +
  theme_minimal() +
  theme(legend.position = "none", legend.direction = "horizontal") # Remove legend

ER_gam_D


### GPP#####

# Create the plot
# Define the custom color palette
custom_palette <- c(colorRampPalette(c("lightblue", "darkblue"))(10), rev(plasma(30, begin = 0.5, end = 1)))

# Modify the plot with the custom color palette
GPP_gam_D <- ggplot() +
  geom_point(data = ALL.LS, aes(x = Avg_DIST, y = GPP.mean, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Daily mean RTRM") +  
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 14, face = "bold"),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(family = "Times New Roman", size = 14),
    axis.text.x = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.x = element_blank()
  ) +
  xlab(expression(bold(""))) +
  ylab(expression(bold("GPP (g O"[2]~m^{-3}~day^{-1}*")"))) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal")

GPP_gam_D


#### NEP ###
# Predict NEP using the GAMM model
# Predict NEP using the GAMM model
predictions_nep <- predict(m3.nep$gam, newdata = new_data, type = "response")

# Combine predicted values with new_data
predictions_df_nep <- data.frame(new_data, NEP_predicted = predictions_nep)

# Calculate 95% confidence intervals
pred_df_nep <- data.frame(new_data, predictions_nep)
conf_intervals_nep <- predict(m3.nep$gam, newdata = new_data, se.fit = TRUE)$se.fit * qt(0.975, m3.nep$gam$df.residual)  

# Combine predicted values and confidence intervals
predictions_df_nep <- cbind(pred_df_nep, lower_ci = pred_df_nep$predictions_nep - conf_intervals_nep, upper_ci = pred_df_nep$predictions_nep + conf_intervals_nep)

# Plot with confidence intervals
custom_palette <- c(colorRampPalette(c("lightblue", "darkblue"))(10), rev(plasma(30, begin = 0.5, end = 1)))

NEP_gam_D <- ggplot() +
  geom_line(data = predictions_df_nep, aes(x = Avg_DIST, y = predictions_nep), color = "blue", size = 1.2, alpha = 0.5) +
  geom_ribbon(data = predictions_df_nep, aes(x = Avg_DIST, ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.2) +
  geom_point(data = ALL.LS, aes(x = Avg_DIST, y = NEP, color = avg_RTRM), size = 3, alpha = 0.5) +
  scale_color_gradientn(colors = custom_palette, limits = c(0, 40), name = "Mean daily RTRM") + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    legend.title = element_text(family = "Times New Roman", size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    axis.text.y = element_text(family = "Times New Roman", size = 14, vjust = 2),
    axis.ticks.y = element_blank() # Remove y-axis ticks
  ) +
  xlab(expression(bold("Mean Distance away from shore (m)"))) + # Bold x-axis label
  ylab(expression(bold("NEP (g O"[2]~m^{-3}~day^{-1}*")"))) +
  theme_minimal() +
  theme(legend.position = "", legend.direction = "horizontal") # Remove legend

NEP_gam_D


# Arrange plots vertically
stacked_plots.D <- GPP_gam_D / ER_gam_D / NEP_gam_D

# Display the stacked plots
stacked_plots.D

# for power point
side_by_side_D <- GPP_gam_D + ER_gam_D + plot_layout(ncol = 2)
side_by_side_D


# for power point NEP
library(gridExtra)
side_by_side_NEP <- grid.arrange(NEP.gam.T, NEP_gam_D, nrow = 1)
side_by_side_NEP

# Save the combined plot with a white background
ggsave(
  filename = "C:/Users/sasin/OneDrive/Documents/Glider/Figures/stacked_plots.D2.png",
  plot = stacked_plots.D,
  device = "png",
  width = 15,
  height = 20,
  units = "cm",
  dpi = 1000,
  scale = 1.5,
  bg = "white"
)

# Save the combined plot with a white background for the presentations
ggsave(
  filename = "C:/Users/sasin/OneDrive/Documents/Glider/Figures/side_by_side_NEP_pp.png",
  plot = side_by_side_NEP,
  device = "png",
  width = 30,
  height = 12,
  units = "cm",
  dpi = 1000,
  scale = 1.5,
  bg = "white"
)



