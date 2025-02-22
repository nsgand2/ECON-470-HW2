# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting
library(readr)     # For reading data
library(knitr)
library(tidyr)
library(MatchIt)    
library(WeightIt)   
library(sandwich)   
library(lmtest)     
library(Matching)   
  


# Load dataset
hcris_Data <- readRDS("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW2/data/output/HCRIS_DATA.rds")


# Question 1:

  # calculating total reports for each hopsital per year
  hospital_report_counts <- hcris_Data %>%
    group_by(year,street) %>%
    summarise(report_count = n(), .groups = "drop") %>%
    arrange(desc(report_count)) %>%
    filter(report_count > 1) %>%
    filter(street != "NA")
  
  total_report_count <- hospital_report_counts %>%
    group_by(year) %>%
    summarise(total = n())
  
  # graphing
  ggplot(data = total_report_count, aes(x=year, y=total)) +
    geom_line()
  
# QUESTION 2
  
  # how many unique hospital ids exist after combining provider numbers
  unique_provider_nos <- hcris_Data %>%
    group_by(provider_number) %>%
    count(provider_number) %>%
    summarize(total = n())
  # total - 9,323
  
  tibble(unique_hospital_ids = nrow(unique_provider_nos))


# Question 3

# Step 1: Filter out top and bottom 1% of total charges and apply log scale
filtered_hcris <- hcris_Data %>%
  filter(!is.na(tot_charges) & tot_charges > 0) %>%  # Ensure no negative or missing charges
  group_by(year) %>%
  mutate(
    lower_bound = quantile(tot_charges, 0.01),  # Bottom 1%
    upper_bound = quantile(tot_charges, 0.99)   # Top 1%
  ) %>%
  filter(tot_charges >= lower_bound & tot_charges <= upper_bound)  # Remove outliers

# Step 2: Create the violin plot with log-transformed y-axis
violin_plot <- ggplot(filtered_hcris, aes(x = as.factor(year), y = tot_charges)) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  scale_y_log10(labels = scales::comma) +  # Apply log scale and format y-axis with commas
  labs(
    title = "Distribution of Total Charges by Year (Top & Bottom 1% Removed, Log Scale)",
    x = "Year",
    y = "Total Charges (Log Scale)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Step 3: Display the plot
print(violin_plot)


# Question 4:

# Ensure the dataset is in a dataframe format
hcris_data <- as.data.frame(hcris_Data)

# Calculate estimated prices using the given formula
hcris_data <- hcris_data %>%
  mutate(discount_factor = 1 - tot_discounts / tot_charges,
         price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
         price_denom = tot_discharges - mcare_discharges,
         price = price_num / price_denom)

# Remove problematic values (negative and extreme outliers)
hcris_filtered <- hcris_data %>%
  filter(!is.na(price) & price > 0) %>%  # Remove missing and negative prices
  filter(price < quantile(price, 0.99, na.rm = TRUE))  # Remove extreme outliers (top 1%)

# Convert year to factor for categorical x-axis
hcris_filtered$year <- as.factor(hcris_filtered$year)

# Create violin plot of estimated prices per year
ggplot(hcris_filtered, aes(x = year, y = price)) +
  geom_violin(fill = "red", alpha = 0.5, adjust = 1.5) +  # Smooth density estimation
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.3) +  # Boxplot for additional clarity
  scale_y_continuous(trans = "log10") +  # Log scale to handle skewed data
  theme_minimal() +
  labs(title = "Distribution of Estimated Prices by Year",
       x = "Year",
       y = "Estimated Price (log scale)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability


# Question 5: 

hcris_2012 <- hcris_data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<


# Step 3: Calculate average price for penalized vs. non-penalized hospitals
average_prices <- hcris_2012 %>%
  group_by(penalty) %>%
  summarize(
    avg_price = mean(price, na.rm = TRUE)
  )

# Step 4: Print the result
print("Average Price by Penalty Status (2012):")
print(average_prices)


# Question 6

# Step 2: Compute price using discount factor
hcris_2012 <- hcris_2012 %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  )

# Step 4: Create **quartiles for bed size** using the same cutoff values as your friend
hcris_2012 <- hcris_2012 %>%
  mutate(
    bed_quartile = case_when(
      beds > 0 & beds <= 91 ~ 1,
      beds > 91 & beds <= 167 ~ 2,
      beds > 167 & beds <= 293.5 ~ 3,
      beds > 293.5 ~ 4
    ),
    bed_1 = ifelse(bed_quartile == 1, 1, 0),
    bed_2 = ifelse(bed_quartile == 2, 1, 0),
    bed_3 = ifelse(bed_quartile == 3, 1, 0),
    bed_4 = ifelse(bed_quartile == 4, 1, 0)
  )

# Step 5: Compute the **average price among treated (penalized) and control (non-penalized) groups for each quartile**
average_prices_by_quartile <- hcris_2012 %>%
  group_by(bed_quartile, penalty) %>%
  summarize(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = penalty, values_from = avg_price)  # Transform data to wide format

# Rename columns for clarity
colnames(average_prices_by_quartile) <- c("Bed Quartile", "Avg Price (Non-Penalized)", "Avg Price (Penalized)")

# Step 6: Display the table neatly
kable(average_prices_by_quartile, caption = "Average Price by Penalty Status Across Bed Size Quartiles")



# Question 7

# Ensure `hcris_2012` is properly ungrouped before selecting variables
hcris_2012 <- hcris_2012 %>% ungroup() %>% as.data.frame()

# Select relevant variables and filter missing values
lp.vars <- hcris_2012 %>%
  dplyr::select(beds, mcaid_discharges, penalty, ip_charges, 
                mcare_discharges, tot_mcare_payment, price, 
                bed_1, bed_2, bed_3, bed_4, bed_quartile) %>%
  filter(complete.cases(.))
# Remove 'penalty' and 'price' columns for Mahalanobis matching
lp.covs <- lp.vars %>% dplyr::select(-penalty, -price)

# ------------------------------------------------------
# A. Nearest Neighbor Matching (1-to-1) with Inverse Variance Distance
# ------------------------------------------------------


# Run Matching since we now have valid data
m.nn.var2 <- Matching::Match(
  Y = lp.vars$price,  
  Tr = lp.vars$penalty,  
  X = as.matrix(lp.vars[, c("bed_1", "bed_2", "bed_3", "bed_4")]),  
  M = 1,  
  Weight = 2,  
  estimand = "ATE",
  ties=FALSE)

# Extract ATE estimate
ate_nn_var2 <- m.nn.var2$est


# ------------------------------------------------------
# B. Nearest Neighbor Matching (1-to-1) with Mahalanobis Distance
# ------------------------------------------------------
m.nn.md <- Matching::Match(Y = lp.vars$price,
                           Tr = lp.vars$penalty,
                           X = lp.covs,
                           M = 1,
                           Weight = 2,
                           estimand = "ATE")


ate_mahal <- m.nn.md$est  # Extract ATE estimate

# ------------------------------------------------------
# C. Inverse Propensity Weighting (IPW)
# ------------------------------------------------------
logit.model <- glm(penalty ~ factor(bed_quartile) + mcaid_discharges + ip_charges + mcare_discharges +
                     tot_mcare_payment, family = binomial, data = lp.vars)

# Compute propensity scores
ps <- fitted(logit.model)

# Estimate ATE using inverse propensity weighting
m.nn.ps <- Matching::Match(Y = lp.vars$price,
                           Tr = lp.vars$penalty,
                           X = ps,
                           M = 1,
                           estimand = "ATE")

ate_ipw <- m.nn.ps$est  # Extract ATE estimate

# ------------------------------------------------------
# D. Simple Linear Regression Adjusting for Bed Quartiles
# ------------------------------------------------------
reg.dat <- lp.vars %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>%
  mutate(
    beds_diff = penalty * (beds - mean(beds)),
    mcaid_diff = penalty * (mcaid_discharges - mean(mcaid_discharges)),
    ip_diff = penalty * (ip_charges - mean(ip_charges)),
    mcare_diff = penalty * (mcare_discharges - mean(mcare_discharges)),
    mpay_diff = penalty * (tot_mcare_payment - mean(tot_mcare_payment))
  )

reg <- lm(price ~ penalty + beds + bed_1 + bed_2 + bed_3 + bed_4 + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment +
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data = reg.dat)

ate_reg <- coef(summary(reg))["penaltyTRUE", "Estimate"]


# ------------------------------------------------------
# Present Results in a Single Table
# ------------------------------------------------------
results <- tibble(
  Method = c("Nearest Neighbor (Inv. Var)", "Nearest Neighbor (Mahalanobis)", "Inverse Propensity Weighting", "Linear Regression"),
  ATE = c(ate_nn_var2, ate_mahal, ate_ipw, ate_reg)
)

kable(results, caption = "Estimated Treatment Effects by Method")


# Save workspace (optional)
save.image("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW2/submission3/hw_workspace.Rdata")
