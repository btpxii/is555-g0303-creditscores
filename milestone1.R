library(tidyverse)
library(ggplot2)
library(janitor)

data_raw <- read_csv('https://www.dropbox.com/scl/fi/f2qvn6c59j3l3b81bthzk/01_train.csv?rlkey=x9d1f9hu0qu3yuilimil7ck33&dl=1')

data_raw


# BY JD The following code walks through the process of identifying NA values, analyzing numeric variables for outliers, and analyzing one categorical variable. 
## find na values and their distribution, this is better than a regular count of na values
# because it gives a more standardized measure of the extent of missingness within each variable
# 

missing_data <- colSums(is.na(data_raw)) / nrow(data_raw)
plot_nas <- ggplot(data.frame(variable = names(missing_data), missing_prop = missing_data), mapping = aes(x = variable,y=missing_prop))+
  geom_bar(stat = "identity", fill ="red")+
  labs(x = "variables", y = "proportion of missing values") +
  theme(axis.text.x= element_text(angle = 45,hjust =1))
plot_nas

#now we will find the distribution of numeric variables to find any outliers, remove NA to only account for actual 
# values

numeric_data <- data_raw %>%
  select_if(is.numeric) %>%
  filter_all(all_vars(!is.na(.)))

## CREDIT CARD NUMBER OUTLIERS
credit_card_number_plot <- ggplot(numeric_data, mapping = aes(y = Num_Credit_Card))+
  geom_histogram(binwidth = 40, fill = "red" )+
  labs(x = "Frequency", y ="FrequencyNumber of CCs", title = "Disttibution of Number of Credit Cards")+
  theme_bw()

credit_card_number_plot

# INTEREST RATE OUTLIERS

interest_rate_plot <- ggplot(numeric_data, mapping = aes(y = Interest_Rate))+
  geom_histogram(binwidth = 40, fill = "red" )+
  labs(x = "Frequency", y ="Interest Rates", title = "Distribution of Interest Rates")+
  theme_bw()

interest_rate_plot

# Num_Bank_Accounts outliers
bank_accounts_plot <- ggplot(numeric_data, mapping = aes(y = Num_Bank_Accounts))+
  geom_histogram(binwidth = 60, fill = "darkgreen" )+
  labs(x = "Frequency", y ="Num of Bank Accounts", title = "Distribution of Amounts of Bank Accounts")+
  theme_bw()

bank_accounts_plot

# Delay_from_due_date outliers


delay_due_plot <- ggplot(numeric_data, mapping = aes(y = Delay_from_due_date))+
  geom_density( fill = "darkgreen", color = "black")+
  labs(x = "Frequency", y ="Number of Days Delayed", title = "Distribution of Delays from Due Date")+
  theme_bw()

delay_due_plot

# Num_Credit_Inquiries outliers

credit_inquiries_plot <- ggplot(numeric_data, mapping = aes(y = Num_Credit_Inquiries))+
  geom_histogram( binwidth = 40,fill = "red")+
  labs(x = "Frequency", y ="Number of Inquiries", title = "Distribution of Credit Inquiries")+
  theme_bw()

credit_inquiries_plot

# Total_EMI_per_month outliers

EMI_plot <- ggplot(numeric_data, mapping = aes(y = Total_EMI_per_month))+
  geom_histogram( binwidth = 100,fill = "red")+
  labs(x = "Frequency", y ="Number of EMI", title = "Distribution of EMI per month")+
  theme_bw()


EMI_plot

#Credit_Utilization_Ratio outliers

CUR_plot <- ggplot(numeric_data, mapping = aes(x = Credit_Utilization_Ratio))+
  geom_density(fill = "cyan", color = "black")+
  labs(x = "Credit Utilization Ratio", y ="Frequency", title = "Distribution of Credit Utilization Ratio")+
  theme_bw()


CUR_plot

#Monthly_Balance outliers
MB_plot <- ggplot(numeric_data, mapping = aes(x = Monthly_Balance))+
  geom_histogram(fill = "cyan", color = "black")+
  labs(x = "Monthly Balance ", y ="Frequency", title = "Distribution of Monthly Balance")+
  theme_bw()

MB_plot

# categorical variable Payment Behavior:
ggplot(data_raw, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Behaviors", y = "Count", title = "Distribution of Payment Behavior")



#BY BRAYDEN: Code that explores the Credit_Score variable
data_raw %>% summary()
data_raw %>% group_by(Customer_ID) %>% summarise(count = n()) %>% arrange(desc(count))

# dependent variable EDA
data_raw %>% 
  group_by(Credit_Score) %>% 
  summarize("Counts" = n())

data_raw$Credit_Score %>% summary()

sum(is.na(data_raw$Credit_Score))

month_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

data_raw$Month_Number <- match(data_raw$Month, month_names)

uniq_cs_cust <- data_raw %>% 
  group_by(Customer_ID, Credit_Score) %>% 
  summarise(Count = n())
uniq_cs_cust

num_cs_ratings <- uniq_cs_cust %>% group_by(Customer_ID) %>% summarise(rating_count = n())

cs_change_cust <- data_raw %>% 
  select(Customer_ID, Month_Number, Credit_Score) %>% 
  group_by(Customer_ID, Credit_Score) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(Customer_ID, Month_Number) %>% 
  group_by(Customer_ID) %>% 
  mutate(Change = paste0(Credit_Score, "_", lead(Credit_Score))) %>% 
  filter(!grepl("_NA$", Change))
cs_change_cust

cx_three_cs <- data_grouped_2 %>% filter(Count_uniq == 3) %>% select(Customer_ID, Credit_Score)
cx_three_cs

# credit score labels, duplicates included, all months total
data_raw %>% 
  ggplot(mapping = aes(x = Credit_Score, fill=Credit_Score)) +
  geom_bar() +
  labs(title = "Count of Credit Score Labels",
       x = "Credit Score Label",
       y = "Count")

# number of credit score ratings per customer
num_cs_ratings %>% 
  ggplot(mapping = aes(x = rating_count)) +
  geom_bar(fill="blue") +
  labs(title = "Count of credit score ratings per customer",
       x = "Rating count",
       y = "Count of occurrences")

# credit score labels with duplicates per customer removed
uniq_cs_cust %>% 
  ggplot(mapping = aes(x = Credit_Score, fill=Credit_Score)) +
  geom_bar() +
  labs(title = "Count of Unique Credit Score Labels by Customer",
       x = "Credit Score Label",
       y = "Count")

# score changes
cs_change_cust %>% 
  ggplot(mapping = aes(x = Change, fill=Change)) +
  geom_bar() +
  labs(title = "Count of score change events for individual customers",
       x = "Change event",
       y = "Count")
#BY SELASSIE: 
# This tibble creates a new variable 'types_loan' by extracting distinct values of 'Type_of_Loan' from the 'data_raw' dataset
# then grouping the data by these loan types. Subsequently, a new variable 'unique_loans' is added to each group
#  representing the count of occurrences of each distinct loan type. The result is a dataset that shows each unique loan type along
# with its count withing the data raw dataset.
types_loan <- data_raw %>% 
  distinct(Type_of_Loan) %>% 
  group_by(Type_of_Loan) %>% 
  mutate(unique_loans = n())




# This table classifies the columns in our dataset as "Categorical," "Continuous," or "Other" based on their types. 
# The categorization is determined by checking if the column 'x' is either a factor or a character using the functions 
# is.factor(x) and is.character(x). If the column falls under either of these categories, it is labeled as 'Categorical'; 
# otherwise, it is classified as 'Continuous.' However, this process revealed potential issues in our dataset, such as the 
# 'Num_of_Delayed_Payment' column being incorrectly identified as 'Categorical' when it should be considered a 'Continuous' variable.

column_types <- sapply(data_raw, function(x) {
  if (is.factor(x) || is.character(x)) {
    return("Categorical")
  } else if (is.numeric(x)) {
    return("Continuous")
  } else {
    return("Other")
  }
})

# Display the summary
summary_df <- data.frame(Column = names(data_raw), Type = column_types)
print(summary_df)

















