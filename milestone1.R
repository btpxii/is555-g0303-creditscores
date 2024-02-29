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

# BY BRADEN:
#Stats Analysis
#Num_Credit_Inquires
Num_Credit_Inquiries_Anova <- aov(Num_Credit_Inquiries ~ Credit_Score, data = data_raw)
Num_Credit_Inquiries_Anova_Table <- summary(Num_Credit_Inquiries_Anova)
Num_Credit_Inquiries_Anova_Table_var1 <- Num_Credit_Inquiries_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Num_Credit_Inquiries_Anova_var2 <- sum(Num_Credit_Inquiries_Anova_Table[[1]][, "Sum Sq"])
Num_Credit_Inquiries_Anova_r_squared <- Num_Credit_Inquiries_Anova_Table_var1 / Num_Credit_Inquiries_Anova_var2

#Annual_Income - Unavailable due to NA's and skewness in Annual_Income
Annual_Income_Anova <- aov(Annual_Income ~ Credit_Score, data = na.omit(data_raw[, c("Annual_Income", "Credit_Score")]))

#Num_Bank_Accounts
Num_Bank_Accounts_Anova <- aov(Num_Bank_Accounts ~ Credit_Score, data = data_raw)
Num_Bank_Accounts_Anova_Table <- summary(Num_Bank_Accounts_Anova)
Num_Bank_Accounts_Anova_Table_var1 <- Num_Bank_Accounts_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Num_Bank_Accounts_Anova_var2 <- sum(Num_Bank_Accounts_Anova_Table[[1]][, "Sum Sq"])
Num_Bank_Accounts_Anova_r_squared <- Num_Bank_Accounts_Anova_Table_var1 / Num_Bank_Accounts_Anova_var2

#Num_Credit_Card
Num_Credit_Card_Anova <- aov(Num_Credit_Card ~ Credit_Score, data = data_raw)
Num_Credit_Card_Anova_Table <- summary(Num_Credit_Card_Anova)
Num_Credit_Card_Anova_Table_var1 <- Num_Credit_Card_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Num_Credit_Card_Anova_var2 <- sum(Num_Credit_Card_Anova_Table[[1]][, "Sum Sq"])
Num_Credit_Card_Anova_r_squared <- Num_Credit_Card_Anova_Table_var1 / Num_Credit_Card_Anova_var2

#Interest_Rate
Interest_Rate_Anova <- aov(Interest_Rate ~ Credit_Score, data = data_raw)
Interest_Rate_Anova_Table <- summary(Interest_Rate_Anova)
Interest_Rate_Anova_Table_var1 <- Interest_Rate_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Interest_Rate_Anova_var2 <- sum(Interest_Rate_Anova_Table[[1]][, "Sum Sq"])
Interest_Rate_Anova_r_squared <- Interest_Rate_Anova_Table_var1 / Interest_Rate_Anova_var2

#Num_of_Loan - Needs additioanl data cleaning
Num_of_Loan_Anova <- aov(Num_of_Loan ~ Credit_Score, data = data_raw)

#Type_of_Loan - Needs additioanl data cleaning
Type_of_Loan_Anova <- aov(Type_of_Loan ~ Credit_Score, data = data_raw)

#Delay_from_due_date
Delay_from_due_date_Anova <- aov(Delay_from_due_date ~ Credit_Score, data = data_raw)
Delay_from_due_date_Anova_Table <- summary(Delay_from_due_date_Anova)
Delay_from_due_date_Anova_Table_var1 <- Delay_from_due_date_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Delay_from_due_date_Anova_var2 <- sum(Delay_from_due_date_Anova_Table[[1]][, "Sum Sq"])
Delay_from_due_date_r_squared <- Delay_from_due_date_Anova_Table_var1 / Delay_from_due_date_Anova_var2

#Num_of_Delayed_Payment - Needs additional data cleaning
Num_of_Delayed_Payment_Anova <- aov(Num_of_Delayed_Payment ~ Credit_Score, data = data_raw)

#Changed_Credit_Limit - Needs additional data cleaning
Changed_Credit_Limit_Anova <- aov(Changed_Credit_Limit ~ Credit_Score, data = data_raw)

#Credit_Mix - Needs additional data cleaning
Credit_Mix_Anova <- aov(Credit_Mix ~ Credit_Score, data = data_raw)

#Outstanding_Debt - Needs additional data cleaning
Outstanding_Debt_Anova <- aov(Outstanding_Debt ~ Credit_Score, data = data_raw)

#Credit_Utilization_Ratio
Credit_Utilization_Ratio_Anova <- aov(Credit_Utilization_Ratio ~ Credit_Score, data = data_raw)
Credit_Utilization_Ratio_Anova_Table <- summary(Credit_Utilization_Ratio_Anova)
Credit_Utilization_Ratio_Anova_Table_var1 <- Credit_Utilization_Ratio_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Credit_Utilization_Ratio_Anova_var2 <- sum(Credit_Utilization_Ratio_Anova_Table[[1]][, "Sum Sq"])
Credit_Utilization_Ratio_r_squared <- Credit_Utilization_Ratio_Anova_Table_var1 / Credit_Utilization_Ratio_Anova_var2

#Credit_History_Age - Needs additional data cleaning
Credit_History_Age_Anova <- aov(Credit_History_Age ~ Credit_Score, data = data_raw)

#Payment_of_Min_Amount - Needs additional data cleaning
Payment_of_Min_Amount_Anova <- aov(Payment_of_Min_Amount ~ Credit_Score, data = data_raw)

#Total_EMI_per_month
Total_EMI_per_month_Anova <- aov(Total_EMI_per_month ~ Credit_Score, data = data_raw)
Total_EMI_per_month_Anova_Table <- summary(Total_EMI_per_month_Anova)
Total_EMI_per_month_Anova_Table_var1 <- Total_EMI_per_month_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Total_EMI_per_month_Anova_var2 <- sum(Total_EMI_per_month_Anova_Table[[1]][, "Sum Sq"])
Total_EMI_per_month_r_squared <- Total_EMI_per_month_Anova_Table_var1 / Total_EMI_per_month_Anova_var2

#Amount_invested_monthly - Needs additional data cleaning
Amount_invested_monthly_Anova <- aov(Amount_invested_monthly ~ Credit_Score, data = data_raw)

#Payment_Behaviour - Needs additional data cleaning
Payment_Behaviour_Anova <- aov(Payment_Behaviour ~ Credit_Score, data = data_raw)

#Monthly_Balance
Monthly_Balance_Anova <- aov(Monthly_Balance ~ Credit_Score, data = data_raw)
Monthly_Balance_Anova_Table <- summary(Monthly_Balance_Anova)
Monthly_Balance_Anova_Table_var1 <- Monthly_Balance_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Monthly_Balance_Anova_var2 <- sum(Monthly_Balance_Anova_Table[[1]][, "Sum Sq"])
Monthly_Balance_r_squared <- Monthly_Balance_Anova_Table_var1 / Monthly_Balance_Anova_var2

#Occupation - Not Available
Occupation_Anova <- aov(Occupation ~ Credit_Score, data = data_raw)

#Monthly_Inhand_Salary
Monthly_Inhand_Salary_Anova <- aov(Monthly_Inhand_Salary ~ Credit_Score, data = data_raw)
Monthly_Inhand_Salary_Anova_Table <- summary(Monthly_Inhand_Salary_Anova)
Monthly_Inhand_Salary_Anova_Table_var1 <- Monthly_Inhand_Salary_Anova_Table[[1]]["Credit_Score", "Sum Sq"]
Monthly_Inhand_Salary_Anova_var2 <- sum(Monthly_Inhand_Salary_Anova_Table[[1]][, "Sum Sq"])
Monthly_Inhand_Salary_r_squared <- Monthly_Inhand_Salary_Anova_Table_var1 / Monthly_Inhand_Salary_Anova_var2
#CATAGORICAL
#Payment_Behavior
pay_b_fill_colors <- c("Good" = "skyblue", "Standard" = "lightgreen", "Poor" = "salmon")
payment_behaviour_vs_credit_score <- data_raw %>%
  select(Payment_Behaviour, Credit_Score) %>%
  count(Credit_Score, Payment_Behaviour) %>%
  ggplot(aes(x = Payment_Behaviour, y = n, fill = Credit_Score)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Payment Behavior", y = "Count", title = "Count of Instances by Credit Score and Payment Behavior") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = pay_b_fill_colors) +
  guides(fill = guide_legend(title = "Credit Score"))

#Credit Mix
credit_mix_fill_colors <- c("Bad" = "orange", "Good" = "darkblue", "Standard" = "green", "_" = "grey")
credit_mix_vs_credit_score <- data_raw %>%
  select(Credit_Mix, Credit_Score) %>%
  count(Credit_Score, Credit_Mix) %>%
  ggplot(aes(x = Credit_Score, y = n, fill = Credit_Mix)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Credit Score", y = "Count", title = "Count of Instances by Credit Score and Credit Mix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = credit_mix_fill_colors) +
  guides(fill = guide_legend(title = "Credit Mix"))
#Num_of_Bank_Accounts
Num_of_Bank_Accounts_vs_Credit_Score <- data_raw %>%
  mutate(Credit_Score_Numeric = as.numeric(factor(Credit_Score, levels = c("Poor", "Standard", "Good"), ordered = TRUE))) %>%
  ggplot(aes(x = Num_Bank_Accounts, y = Credit_Score_Numeric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend line
  scale_y_continuous(breaks = 1:3, labels = c("Poor", "Standard", "Good")) +  # Custom y-axis labels
  labs(x = "Num Bank Accounts", y = "Credit Score", title = "Scatter Plot of Credit Score vs. Num Bank Accounts")

#NUMERIC
#Num_Credit_Inquires (with trend line)
num_credit_inquires_vs_credit_score <- data_raw %>%
  mutate(Credit_Score_Numeric = as.numeric(factor(Credit_Score, levels = c("Poor", "Standard", "Good"), ordered = TRUE))) %>%
  ggplot(aes(x = Num_Credit_Inquiries, y = Credit_Score_Numeric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend line
  scale_y_continuous(breaks = 1:3, labels = c("Poor", "Standard", "Good")) +  # Custom y-axis labels
  labs(x = "Num_Credit_Inquiries", y = "Credit_Score", title = "Scatter Plot of Credit Score vs. Num Credit Inquiries")














