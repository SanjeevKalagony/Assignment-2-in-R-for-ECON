#check your working directory
getwd()

#import your .csv file to your Global Environment
wage <- read.csv("Wage60.csv", header = TRUE, sep = ",")

# Load required library for plotting
library(ggplot2)

# Display basic information about the dataset
head(wage)
str(wage)
summary(wage)

# ============================================================================
# PART A - QUESTION 1: Plot evolution of hourly nominal wages for males and females
# ============================================================================

# Create the main plot for Question 1
q1_plot <- ggplot(wage, aes(x = Year)) +
  geom_line(aes(y = Males, color = "Males"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = Females, color = "Females"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Males, color = "Males"), size = 2, shape = 16) +
  geom_point(aes(y = Females, color = "Females"), size = 2, shape = 17) +
  labs(title = "Evolution of Hourly Nominal Wages by Gender (1997-2019)",
       x = "Year",
       y = "Hourly Nominal Wage ($ per hour)",
       color = "Gender") +
  scale_color_manual(values = c("Males" = "blue", "Females" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1997, 2019, 2)) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"),
                                                  shape = c(16, 17))))

print(q1_plot)

# ============================================================================
# PART A - QUESTION 2: Plot evolution of hourly real wages in 2002 dollars
# ============================================================================

# Calculate real wages in 2002 dollars (CPI base year = 2002, base = 100)
wage$Males_Real <- (wage$Males / wage$CPI) * 100
wage$Females_Real <- (wage$Females / wage$CPI) * 100

# Create the plot for Question 2
q2_plot <- ggplot(wage, aes(x = Year)) +
  geom_line(aes(y = Males_Real, color = "Males"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = Females_Real, color = "Females"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Males_Real, color = "Males"), size = 2, shape = 16) +
  geom_point(aes(y = Females_Real, color = "Females"), size = 2, shape = 17) +
  labs(title = "Evolution of Hourly Real Wages by Gender (1997-2019)",
       subtitle = "Expressed in 2002 dollars",
       x = "Year",
       y = "Hourly Real Wage (2002 $ per hour)",
       color = "Gender") +
  scale_color_manual(values = c("Males" = "blue", "Females" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1997, 2019, 2)) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"),
                                                  shape = c(16, 17))))

print(q2_plot)

# Display real wage summary statistics
real_wage_stats <- data.frame(
  Year = wage$Year,
  Males_Nominal = wage$Males,
  Females_Nominal = wage$Females,
  Males_Real = wage$Males_Real,
  Females_Real = wage$Females_Real,
  CPI = wage$CPI
)

print("Real Wage Summary (first 10 years):")
print(head(real_wage_stats, 10))

print("Real Wage Summary (last 5 years):")
print(tail(real_wage_stats, 5))

# Calculate growth rates for comparison
nominal_growth_male <- ((wage$Males[nrow(wage)] - wage$Males[1]) / wage$Males[1]) * 100
nominal_growth_female <- ((wage$Females[nrow(wage)] - wage$Females[1]) / wage$Females[1]) * 100
real_growth_male <- ((wage$Males_Real[nrow(wage)] - wage$Males_Real[1]) / wage$Males_Real[1]) * 100
real_growth_female <- ((wage$Females_Real[nrow(wage)] - wage$Females_Real[1]) / wage$Females_Real[1]) * 100

growth_comparison <- data.frame(
  Gender = c("Males", "Females"),
  Nominal_Growth_Percent = c(nominal_growth_male, nominal_growth_female),
  Real_Growth_Percent = c(real_growth_male, real_growth_female)
)

print("Growth Rate Comparison (1997-2019):")
print(growth_comparison)

# ============================================================================
# PART A - QUESTION 3: Fit linear trends to real wage series
# ============================================================================

# Create time variable for trend analysis (1997 = 1, 1998 = 2, etc.)
wage$time <- wage$Year - 1996

# Fit linear trends to real wage series
trend_males <- lm(Males_Real ~ time, data = wage)
trend_females <- lm(Females_Real ~ time, data = wage)

# Display trend regression results
print("Linear Trend Analysis - Males:")
print(summary(trend_males))

print("Linear Trend Analysis - Females:")
print(summary(trend_females))

# Extract coefficients
male_intercept <- coef(trend_males)[1]
male_slope <- coef(trend_males)[2]
female_intercept <- coef(trend_females)[1]
female_slope <- coef(trend_females)[2]

# Create trend predictions
wage$Male_Trend <- male_intercept + male_slope * wage$time
wage$Female_Trend <- female_intercept + female_slope * wage$time

# Plot the trends
q3_plot <- ggplot(wage, aes(x = Year)) +
  geom_line(aes(y = Male_Trend, color = "Male Trend"), size = 1.5, linetype = "solid") +
  geom_line(aes(y = Female_Trend, color = "Female Trend"), size = 1.5, linetype = "dashed") +
  labs(title = "Linear Trends in Real Wages by Gender (1997-2019)",
       subtitle = "Fitted Linear Trends",
       x = "Year",
       y = "Hourly Real Wage Trend (2002 $ per hour)",
       color = "Trend Lines") +
  scale_color_manual(values = c("Male Trend" = "blue", "Female Trend" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1997, 2019, 2)) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"))))

print(q3_plot)

# Calculate and display trend analysis
trend_analysis <- data.frame(
  Gender = c("Males", "Females"),
  Annual_Change = c(male_slope, female_slope),
  Intercept_1997 = c(male_intercept, female_intercept),
  Predicted_2019 = c(male_intercept + male_slope * 23, female_intercept + female_slope * 23)
)

print("Trend Analysis Results:")
print(trend_analysis)

# Calculate wage gap trend
wage$Gap_Trend <- wage$Male_Trend - wage$Female_Trend
initial_gap <- wage$Gap_Trend[1]
final_gap <- wage$Gap_Trend[nrow(wage)]
gap_change <- final_gap - initial_gap

print(paste("Initial wage gap (1997):", round(initial_gap, 2), "dollars"))
print(paste("Final wage gap (2019):", round(final_gap, 2), "dollars"))
print(paste("Change in wage gap:", round(gap_change, 2), "dollars"))
print(paste("Annual change in wage gap:", round(gap_change/22, 4), "dollars per year"))

# ============================================================================
# PART A - QUESTION 4: Detrend real wages and analyze cyclical components
# ============================================================================

# Calculate cyclical components (detrended series)
wage$Male_Cycle <- wage$Males_Real - wage$Male_Trend
wage$Female_Cycle <- wage$Females_Real - wage$Female_Trend

# Create scatter plot to analyze comovement
q4_plot <- ggplot(wage, aes(x = Male_Cycle, y = Female_Cycle)) +
  geom_point(size = 3, color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.5) +
  labs(title = "Cyclical Components: Male vs Female Real Wages",
       subtitle = "Scatter Plot Analysis of Comovement",
       x = "Male Cyclical Component (2002 $ per hour)",
       y = "Female Cyclical Component (2002 $ per hour)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

print(q4_plot)

# Calculate correlation between cyclical components
cycle_correlation <- cor(wage$Male_Cycle, wage$Female_Cycle)
print(paste("Correlation between male and female cyclical components:", round(cycle_correlation, 4)))

# Fit regression line to analyze relationship
cycle_regression <- lm(Female_Cycle ~ Male_Cycle, data = wage)
print("Regression Analysis - Female Cycle vs Male Cycle:")
print(summary(cycle_regression))

# Display cyclical components data
cyclical_data <- data.frame(
  Year = wage$Year,
  Male_Real = wage$Males_Real,
  Female_Real = wage$Females_Real,
  Male_Trend = wage$Male_Trend,
  Female_Trend = wage$Female_Trend,
  Male_Cycle = wage$Male_Cycle,
  Female_Cycle = wage$Female_Cycle
)

print("Cyclical Components Summary:")
print(head(cyclical_data, 10))
print(tail(cyclical_data, 5))

# Create time series plot of cyclical components
q4_time_plot <- ggplot(wage, aes(x = Year)) +
  geom_line(aes(y = Male_Cycle, color = "Male Cycle"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = Female_Cycle, color = "Female Cycle"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Male_Cycle, color = "Male Cycle"), size = 2, shape = 16) +
  geom_point(aes(y = Female_Cycle, color = "Female Cycle"), size = 2, shape = 17) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  labs(title = "Cyclical Components Over Time",
       subtitle = "Detrended Real Wages (1997-2019)",
       x = "Year",
       y = "Cyclical Component (2002 $ per hour)",
       color = "Cyclical Components") +
  scale_color_manual(values = c("Male Cycle" = "blue", "Female Cycle" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1997, 2019, 2)) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dashed"),
                                                  shape = c(16, 17))))

print(q4_time_plot)

# Summary statistics for cyclical components
cycle_stats <- data.frame(
  Gender = c("Males", "Females"),
  Mean_Cycle = c(mean(wage$Male_Cycle), mean(wage$Female_Cycle)),
  Std_Dev_Cycle = c(sd(wage$Male_Cycle), sd(wage$Female_Cycle)),
  Min_Cycle = c(min(wage$Male_Cycle), min(wage$Female_Cycle)),
  Max_Cycle = c(max(wage$Male_Cycle), max(wage$Female_Cycle))
)

print("Cyclical Component Statistics:")
print(cycle_stats)