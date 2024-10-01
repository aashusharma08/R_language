# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Install the readr package (for reading CSVs)
install.packages("readr")



# Set the working directory
# Load the customer acquisition data from the full path
customer_data <- read.csv("C:/Users/Asus/Pictures/assignments/R/R CREDIT CARD/R case study 2 (Credit card)/Customer Acqusition.csv")
repayment <- read.csv("C:/Users/Asus/Pictures/assignments/R/R CREDIT CARD/R case study 2 (Credit card)/Repayment.csv")
spend <- read.csv("C:/Users/Asus/Pictures/assignments/R/R CREDIT CARD/R case study 2 (Credit card)/spend.csv")

# Check the structure of the loaded data
str(customer_data)


# Check the structure of the data to ensure it loaded correctly
str(customer_data)
str(repayment)
str(spend)




# Convert 'Month' columns to Date format in both 'repayment' and 'spend'
# Assuming 'Month' is in "dd-mmm-yy" format (e.g., "12-Jan-04")
repayment$Month <- dmy(repayment$Month)
spend$Month <- dmy(spend$Month)

# Drop unnecessary columns
repayment <- repayment %>% select(-SL.No., -X)
spend <- spend %>% select(-Sl.No.)



# Merge customer_data with repayment on 'Customer'
customer_repay <- customer_data %>%
  inner_join(repayment, by = "Customer")

# Merge the resulting dataset with spend on 'Customer' and 'Month'
final_data <- customer_repay %>%
  inner_join(spend, by = c("Customer", "Month"))

# Check the structure of the final_data to confirm the merge
str(final_data)

# View the first few rows of final_data
head(final_data)


# Extract full month name and year
final_data$Monthly <- format(final_data$Month, "%B")  # Full month name (e.g., January)
final_data$Yearly  <- format(final_data$Month, "%Y")  # Year (e.g., 2021)

# Optionally filter out rows with missing Month values (if needed)
final_data <- final_data %>% filter(!is.na(Month))

# Verify the final dataset
head(final_data)
str(final_data)

#1. In the above dataset,
#a. In case age is less than 18, replace it with mean of age values.

# Calculate the mean age for customers aged 18 or older
mean_age <- mean(final_data$Age[final_data$Age >= 18], na.rm = FALSE)

# Print the mean age to confirm
print(mean_age)


# Replace age values less than 18 with the mean age
final_data$Age <- ifelse(final_data$Age < 18, mean_age, final_data$Age)

# Check the summary of the Age.x column to confirm changes
summary(final_data$Age.x)


final_data

dim(final_data)


#b. In case spend amount is more than the limit, replace it with 50% of that customer’s limit.
#(customer’s limit provided in acquisition table is the per transaction limit on his card)


# Filter customers where Amount is greater than Limit and replace Amount with 50% of the Limit
# Filter rows where spend amount (Amount) is greater than the customer's limit
filtered_data <- final_data %>%
  filter(Amount.x > Limit)

# View the filtered data
head(filtered_data)

# Replace the spend amount with 50% of the customer's limit if the spend amount exceeds the limit
final_data <- final_data %>%
  mutate(Amount.x = ifelse(Amount.x > Limit, 0.5 * Limit, Amount.x))

# View the updated data
head(final_data)

# Filter the rows where the spend amount (Amount.x) is greater than the customer's limit
over_limit_customers <- final_data %>%
  filter(Amount.x > Limit)

# Filter the rows where the spend amount (Amount.x) is greater than the customer's limit
over_limit_customers <- final_data %>%
  filter(Amount.x > Limit)

# View the filtered data
over_limit_customers



#c. Incase the repayment amount is more than the limit, replace the repayment with the
#limit.


# Filter the rows where the repayment amount (Amount.y) is greater than the customer's limit
over_limit_repayments <- final_data %>%
  filter(Amount.y > Limit)

# View the filtered data
over_limit_repayments

# Check how many rows have repayments exceeding the limit
nrow(over_limit_repayments)

# Replace repayment amounts greater than the limit with the limit
final_data <- final_data %>%
  mutate(Amount.y = ifelse(Amount.y > Limit, Limit, Amount.y))

# Check if there are any remaining customers where the repayment amount exceeds the limit
over_limit_repayments <- final_data %>%
  filter(Amount.y > Limit)

# View the filtered data (should be empty if all have been replaced correctly)
over_limit_repayments


# Check if there are any rows left where the repayment amount exceeds the limit
nrow(over_limit_repayments)









#Ques-2. From the above dataset create the following summaries:
#a. How many distinct customers exist?

# Calculate the number of distinct customers in the 'Customer' column
distinct_customers <- n_distinct(final_data$Customer)
# Print the result
cat("The number of distinct customers:", distinct_customers, "\n")




# b. How many distinct categories exist?
# Count the number of customers from different segments
segment_counts <- final_data %>%
  count(Segment)
# View the counts
print(segment_counts)


# Plot the count of customers from different segments using ggplot2
ggplot(final_data, aes(x = Segment)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Customer Count by Segment", x = "Segment", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the number of distinct segments
distinct_segments <- n_distinct(final_data$Segment)

# Print the result
cat("The number of distinct segments:", distinct_segments, "\n")






#- c. What is the average monthly spend by customers?

# Replace spend amount with 50% of the limit if it exceeds the limit
final_data <- final_data %>%
  mutate(Amount.x = ifelse(Amount.x > Limit, 0.5 * Limit, Amount.x))

# Check the updated data
head(final_data)





##d- d. What is the average monthly repayment by customers?
# Group by 'Monthly' and summarize total spend and total repayment
monthly_summary <- final_data %>%
  group_by(Monthly) %>%
  summarise(
    total_spend = sum(Amount.x),
    total_repayment = sum(Amount.y)
  )

# View the monthly summary
print(monthly_summary)






#e-e. If the monthly rate of interest is 2.9%, what is the profit for the bank for each month?
#(Profit is defined as interest earned on Monthly Profit. Monthly Profit = Monthly repayment
#  – Monthly spend. Interest is earned only on positive profits and not on negative amounts)


# Create the 'Yearly' column by extracting the year from the 'Month' column
final_data$Yearly <- format(final_data$Month, "%Y")

# Monthly interest rate
interest_rate <- 2.9 / 100  # 2.9%

# Group by 'Monthly' and 'Yearly' and calculate the total spend and total repayment for each month
monthly_profit_data <- final_data %>%
  group_by(Monthly, Yearly) %>%
  summarise(
    total_spend = sum(Amount.x),
    total_repayment = sum(Amount.y),
    .groups = "drop"
  ) %>%
  # Calculate the monthly profit
  mutate(monthly_profit = total_repayment - total_spend) %>%
  # Calculate the profit based on the monthly interest rate (only apply to positive profit)
  mutate(bank_profit = ifelse(monthly_profit > 0, monthly_profit * interest_rate, 0))

# View the resulting data with calculated profits
print(monthly_profit_data)






# -f. What are the top 5 product types?

# Count the frequency of each product type and sort in descending order
top_5_product_types <- final_data %>%
  group_by(Type) %>%
  summarise(count = n()) %>%  # Count the number of transactions for each product type
  arrange(desc(count)) %>%     # Sort by frequency in descending order
  head(5)                      # Select the top 5 product types

# View the top 5 product types
print(top_5_product_types)

# Top 5 product types by total spend
top_5_product_types_by_spend <- final_data %>%
  group_by(Type) %>%
  summarise(total_spend = sum(Amount.x)) %>%  # Sum the spend for each product type
  arrange(desc(total_spend)) %>%              # Sort by total spend in descending order
  head(5)                                     # Select the top 5 product types

# View the top 5 product types by spend
print(top_5_product_types_by_spend)




# g-Which city is having maximum spend?

# Load necessary library
library(dplyr)

# Group by 'City' and calculate the total spend, then find the city with the maximum spend
city_max_spend <- final_data %>%
  group_by(City) %>%
  summarise(total_spend = sum(Amount.x)) %>%  # Sum the spend for each city
  arrange(desc(total_spend)) %>%              # Sort in descending order of spend
  slice(1)                                    # Select the top city with the maximum spend

# View the city with the maximum spend
print(city_max_spend)




#h. Which age group is spending more money?

# Define age groups (you can adjust the breaks and labels as needed)
final_data <- final_data %>%
  mutate(age_group = cut(Age, 
                         breaks = c(0, 25, 35, 45, 55, 65, 100), 
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+")))

# Group by age group and calculate the total spend
age_group_spend <- final_data %>%
  group_by(age_group) %>%
  summarise(total_spend = sum(Amount.x)) %>%  # Sum the spend for each age group
  arrange(desc(total_spend))                  # Sort by total spend in descending order

# View the age group with the maximum spend
print(age_group_spend)



#i. Who are the top 10 customers in terms of repayment?

# Calculate total repayment by customer and select the top 10 customers
top_10_customers_repayment <- final_data %>%
  group_by(Customer) %>%
  summarise(total_repayment = sum(Amount.y)) %>%  # Sum the repayment for each customer
  arrange(desc(total_repayment)) %>%              # Sort by repayment in descending order
  slice(1:10)                                     # Select the top 10 customers

# View the top 10 customers in terms of repayment
print(top_10_customers_repayment)


#3. Calculate the city wise spend on each product on yearly basis. Also include a graphical
#representation for the same.

# Calculate city-wise spend on each product on a yearly basis
city_product_yearly_spend <- final_data %>%
  group_by(City, Product, Yearly) %>%
  summarise(total_spend = sum(Amount.x), .groups = 'drop')  # Sum the spend for each city, product, and year

# View the result
print(city_product_yearly_spend)

# Plotting the city-wise spend on each product on a yearly basis
ggplot(city_product_yearly_spend, aes(x = Yearly, y = total_spend, fill = Product)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ City, scales = "free_y") +  # Create separate plots for each city
  labs(title = "City-wise Spend on Each Product (Yearly)",
       x = "Year", y = "Total Spend",
       fill = "Product") +
  theme_minimal()



#4. Create graphs for

# a. Monthly comparison of total spends, city wise

# Aggregate data by city and month (monthly comparison of total spends)
monthly_city_spend <- final_data %>%
  group_by(City, Monthly) %>%
  summarise(total_spend = sum(Amount.x), .groups = 'drop')  # Sum total spend for each city and month

# View the aggregated data
print(monthly_city_spend)

# Create a bar plot to visualize monthly comparison of total spends, city-wise
ggplot(monthly_city_spend, aes(x = Monthly, y = total_spend, fill = City)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create a bar plot with dodging for comparison
  labs(title = "Monthly Comparison of Total Spends, City-wise",
       x = "Month", y = "Total Spend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity




#b. Comparison of yearly spend on air tickets.

# Filter the data for air tickets
air_ticket_spend <- final_data %>%
  filter(Type == "AIR TICKET") %>%
  group_by(Yearly) %>%
  summarise(total_spend = sum(Amount.x), .groups = 'drop')  # Sum the spend on air tickets by year

# View the filtered data
print(air_ticket_spend)

# Create a bar plot for the yearly comparison of spend on air tickets
ggplot(air_ticket_spend, aes(x = Yearly, y = total_spend, fill = Yearly)) +
  geom_bar(stat = "identity") +
  labs(title = "Yearly Comparison of Total Spend on Air Tickets",
       x = "Year", y = "Total Spend on Air Tickets") +
  theme_minimal()



# c. Comparison of monthly spend for each product (look for any seasonality
#that exists in terms of spend)

# Group by 'Product' and 'Monthly' and calculate the total spend for each combination
monthly_product_spend <- final_data %>%
  group_by(Product, Monthly) %>%
  summarise(total_spend = sum(Amount.x), .groups = 'drop')  # Sum the spend for each product and month

# View the resulting data
print(monthly_product_spend)

# Reorder the 'Monthly' column to preserve chronological order
monthly_product_spend$Monthly <- factor(monthly_product_spend$Monthly, 
                                        levels = c("January", "February", "March", "April", "May", "June",
                                                   "July", "August", "September", "October", "November", "December"))

# Create a line plot to compare monthly spend for each product
ggplot(monthly_product_spend, aes(x = Monthly, y = total_spend, color = Product, group = Product)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) + # Add points to highlight spend in each month
  labs(title = "Monthly Spend Comparison for Each Product",
       x = "Month", y = "Total Spend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability





#5. Write user defined R function to perform the following analysis:
 # You need to find top 10 customers for each city in terms of their repayment amount by
#different products and by different time periods i.e. year or month. The user should be able
#to specify the product (Gold/Silver/Platinum) and time period (yearly or monthly) and the
#function should automatically take these inputs while identifying the top 10 customers.

# Load necessary library
library(dplyr)

# Define the function to find top 10 customers by city, product type, and time period
top_10_customers_by_repayment <- function(final_data, product_type, time_period = "yearly") {
  
  # Filter the data based on the product type
  filtered_data <- final_data %>%
    filter(Product == product_type)
  
  # Grouping logic based on time_period
  if (time_period == "yearly") {
    # Group by city, customer, product, and year, then calculate total repayment
    top_customers <- filtered_data %>%
      group_by(City, Customer, Yearly) %>%
      summarise(total_repayment = sum(Amount.y), .groups = 'drop') %>%
      arrange(City, desc(total_repayment)) %>%
      group_by(City) %>%
      slice(1:10)  # Select the top 10 customers for each city
    
  } else if (time_period == "monthly") {
    # Group by city, customer, product, and month, then calculate total repayment
    top_customers <- filtered_data %>%
      group_by(City, Customer, Monthly, Yearly) %>%
      summarise(total_repayment = sum(Amount.y), .groups = 'drop') %>%
      arrange(City, desc(total_repayment)) %>%
      group_by(City) %>%
      slice(1:10)  # Select the top 10 customers for each city
  } else {
    stop("Invalid time period. Please specify either 'yearly' or 'monthly'.")
  }
  
  # Return the result
  return(top_customers)
}

# Example usage:
# Get top 10 customers for the product 'Gold' on a yearly basis
top_customers_yearly <- top_10_customers_by_repayment(final_data, "Gold", "yearly")
print(top_customers_yearly)

# Get top 10 customers for the product 'Silver' on a monthly basis
top_customers_monthly <- top_10_customers_by_repayment(final_data, "Silver", "monthly")
print(top_customers_monthly)










