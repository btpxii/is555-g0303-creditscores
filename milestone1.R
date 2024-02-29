library(tidyverse)
library(ggplot2)
library(janitor)

data_raw <- read_csv('https://www.dropbox.com/scl/fi/f2qvn6c59j3l3b81bthzk/01_train.csv?rlkey=x9d1f9hu0qu3yuilimil7ck33&dl=1')

data_raw

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



