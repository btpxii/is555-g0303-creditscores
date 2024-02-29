library(tidyverse)

data_raw <- read_csv('./is555-g0303-creditscores/01_train.csv')

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

