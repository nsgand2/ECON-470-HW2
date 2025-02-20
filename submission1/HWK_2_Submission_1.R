# Load necessary libraries
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting
library(readr)     # For reading data

# Load dataset
data_all <- readRDS('C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW2/data/output/HCRIS_DATA.rds')

hcris_Data <- readRDS("C:/Users/Nikhita Gandhe/Documents/GitHub/ECON-470-HW2/data/output/HCRIS_DATA.rds")
hcris_data <- as.data.frame(HCRIS.numeric)
print(colnames(HCRIS.numeric))
[
# Question 1:
# Plot hospitals with multiple reports
fig.dup <- dup.count %>%
  ggplot(aes(x = as.factor(fyear), y = duplicates, group = 1)) + 
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) +
  labs(
    x = "Year",
    y = "Number of Hospitals",
    title = "Number of Hospitals Filing More Than One Report Per Year"
  ) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0, 300)) + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

# Print the plot
print(fig.dup)
]


# Question 2: 
# Remove duplicate reports by selecting unique combinations of provider_number and year
unique_hospitals <- final.hcris %>%
  distinct(provider_number, .keep_all = TRUE)  # Keep only unique provider_number

# Count the number of unique hospital IDs (Medicare provider numbers)
num_unique_hospitals <- unique_hospitals %>%
  summarize(total_unique_hospitals = n_distinct(provider_number))

# Print the result
print(num_unique_hospitals)


# Question 3
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Step 1: Filter out top and bottom 1% of total charges and apply log scale
filtered_hcris <- final.hcris %>%
  filter(!is.na(tot_charges) & tot_charges > 0) %>%  # Ensure no negative or missing charges
  group_by(fyear) %>%
  mutate(
    lower_bound = quantile(tot_charges, 0.01),  # Bottom 1%
    upper_bound = quantile(tot_charges, 0.99)   # Top 1%
  ) %>%
  filter(tot_charges >= lower_bound & tot_charges <= upper_bound)  # Remove outliers

# Step 2: Create the violin plot with log-transformed y-axis
violin_plot <- ggplot(filtered_hcris, aes(x = as.factor(fyear), y = tot_charges)) +
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

# Step 1: Filter for observations in 2012
hcris_2012 <- final.hcris %>%
  filter(fyear == 2012)

colnames(final.hcris)

# Step 2: Define penalty based on the sum of HRRP and HVBP amounts
# Penalty is defined as the sum of HRRP and HVBP being negative
hcris_2012 <- hcris_2012 %>%
  mutate(
    penalty = ifelse((hrrp_amount + hvbp_amount) < 0, "Penalized", "Non-Penalized")
  )

# Step 3: Calculate average price for penalized vs. non-penalized hospitals
average_prices <- hcris_2012 %>%
  group_by(penalty) %>%
  summarize(
    avg_price = mean(price, na.rm = TRUE)
  )

final.hcris <- final.hcris %>%
  filter(fyear == 2012) %>%
  mutate(
    penalty = ifelse((actual_hrrp_column + actual_hvbp_column) < 0, "Penalized", "Non-Penalized")
  )


# Step 4: Print the result
print("Average Price by Penalty Status (2012):")
print(average_prices)



