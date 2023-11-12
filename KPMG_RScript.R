                     # KPMG Virtual Internship (Sprocket Central Ltd) #

# Loading libraries

library(tidyverse)
library(janitor)
library(here)
library(skimr)

# DATA CLEANING PROCESS

#-Data location
setwd("D:/R Activity/KPMG")
#-Data loading
transactions <- read_csv("Transactions.csv")
customerdemo <- read_csv("CustomerDemographic.csv")
customeradd <- read_csv("CustomerAddress.csv")
#- Data overview
View(transactions)
View(customerdemo)
View(customeradd)
#- Converting row to column header
transactions <- transactions %>% row_to_names(1)
customerdemo <- customerdemo %>% row_to_names(1)
customeradd <- customeradd %>% row_to_names(1)
#- Standardizing column header
transactions <- transactions %>% clean_names()
customeradd <- customeradd %>% clean_names()
customerdemo <- customerdemo %>% clean_names()
#- Selecting columns 
transactions <- transactions %>% 
                select(transaction_id, product_id, customer_id, transaction_date, online_order,
                       order_status, brand, product_line, product_class, product_size,
                       list_price, standard_cost, product_first_sold_date)
customerdemo <- customerdemo %>% 
                select(customer_id, first_name, last_name, gender, past_3_years_bike_related_purchases,
                       dob, job_title, job_industry_category, wealth_segment, deceased_indicator,
                       owns_car, tenure)
customeradd <- customeradd %>% 
               select(customer_id, address, postcode, state, country, property_valuation)
#- Data overview after pre-cleaning
View(transactions)
View(customeradd)
View(customerdemo)

skim_without_charts(transactions)
skim_without_charts(customerdemo)
skim_without_charts(customeradd)
#- Changing datatypes
new_transactions <- transactions %>% 
                    mutate(transaction_id = as.numeric(transaction_id),
                           product_id = as.numeric(product_id),
                           customer_id = as.numeric(customer_id),
                           transaction_date = as.Date(transaction_date, format = "%d/%m/%y"),
                           list_price = as.numeric(list_price),
                           standard_cost = as.numeric(standard_cost),
                           product_first_sold_date = excel_numeric_to_date(as.numeric(product_first_sold_date)))
View(new_transactions)
skim_without_charts(new_transactions)

new_customerdemo <- customerdemo %>% 
                    mutate(customer_id = as.numeric(customer_id),
                           past_3_years_bike_related_purchases = as.numeric(past_3_years_bike_related_purchases),
                           dob = as_date(dob),
                           tenure =as.numeric(tenure))
View(new_customerdemo)
skim_without_charts(new_customerdemo)

new_customeradd <- customeradd %>% 
                   mutate(customer_id = as.numeric(customer_id),
                          property_valuation = as.numeric(property_valuation))
View(new_customeradd)
skim_without_charts(new_customeradd)
#- Removing null values
new_transactions <- new_transactions %>% drop_na()
new_customerdemo <- new_customerdemo %>% drop_na()

count(new_transactions)
count(new_customerdemo)
count(new_customeradd)
#- Replacing and Renaming values
new_customerdemo$gender <- str_replace(new_customerdemo$gender,
                                       "F","Female")
new_customerdemo$gender <- str_replace(new_customerdemo$gender,
                                       "M","Male")
new_customerdemo$gender <- str_replace(new_customerdemo$gender,
                                       "Femal","Female")
new_customerdemo$gender <- str_replace(new_customerdemo$gender,
                                       "U","Unidentified")
gender <- new_customerdemo %>% select(gender) %>% distinct()
print(gender)


new_customerdemo$deceased_indicator <- str_replace(new_customerdemo$deceased_indicator,
                                                   "Y","Yes")
new_customerdemo$deceased_indicator <- str_replace(new_customerdemo$deceased_indicator,
                                                   "N","No")
deceased <- new_customerdemo %>% select(deceased_indicator) %>% distinct()
print(deceased)


new_customeradd$state <- str_replace(new_customeradd$state,
                                     "New South Wales","NSW")
new_customeradd$state <- str_replace(new_customeradd$state,
                                     "Victoria","VIC")
state <- new_customeradd %>% select(state) %>% distinct()
print(state)
#- Detecting DOB outlier
outlier <- new_customerdemo %>% select(dob) %>% 
           filter(dob == "1843-12-21")
#- Deleting Outlier
new_customerdemo <- new_customerdemo[!(new_customerdemo$dob == "1843-12-21"),]

# DATA MERGING
sprocket_data <- new_transactions %>% 
                 inner_join(new_customerdemo, by = "customer_id") %>% 
                 inner_join(new_customeradd, by = "customer_id")
#- Overview of merged data
View(sprocket_data)
skim_without_charts(sprocket_data)
#- Creating new columns 
sprocket_data <- sprocket_data %>% 
                 mutate(profit = list_price - standard_cost ) 
                 #(profit column)

sprocket_data <- sprocket_data %>% 
                 mutate(transaction_month = month(transaction_date, label = T, abbr = F))
                 #(transaction_month column)

int <- interval(sprocket_data$dob,sprocket_data$transaction_date)
sprocket_data <- sprocket_data %>% 
                 mutate(age = round(time_length(int, "year"),0))
                 #(age column)

sprocket_data <- sprocket_data %>% 
                 mutate(age_group = case_when(age <= 25 ~ "young",
                                              age <= 55 ~ "Adult",
                                              TRUE ~ "Old"))
                 #(age_group column)

# DATA OVERVIEW
View(sprocket_data)
skim_without_charts(sprocket_data)

# SAVING DATA
write_csv(sprocket_data, "sprocket_clean_data")
