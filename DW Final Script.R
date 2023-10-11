# Goal: find out where to work after graduation, based on my target locations and my background.

# Read in the original data
job<- readr::read_csv('/Users/kang/Desktop/Data_wrangling/Final Project/job_descriptions.csv')
library(tidyverse)

# set my target location to work
target_country <- c('USA','UK', 'China', 'Singapore', 'Macao SAR, China', 'Hong Kong SAR, China')

# filter according to my job background
Job_clean <- job %>% 
  mutate(Job_year = year(`Job Posting Date`)) %>% 
  filter(Job_year == c(2022, 2023)
         , Qualifications != 'PhD'
         , Qualifications != 'MBA'
         , `Work Type` == 'Full-Time'
         , Country %in% target_country)

# filter working experience requirement
Job_clean <- Job_clean %>%
  mutate(sub = as.numeric(str_sub(Job_clean$Experience, 1, 1))) %>% 
  filter(sub == c(1, 0))

## Which country has the highest salary
# clean salary data
Job_clean <- Job_clean %>% 
  mutate(min_salary = as.numeric(str_sub(Job_clean$`Salary Range`, 2, 3))
         , ncharmax = nchar(Job_clean$`Salary Range`)
         , max_salary = as.numeric(str_sub(Job_clean$`Salary Range`, 7, ncharmax - 1))
         , mean_salary = (min_salary + max_salary)/2)

# explore mean salary by country 
job_location <- Job_clean %>% 
  group_by(Country) %>% 
  summarise(mean_salary = mean(mean_salary)) %>% 
  arrange(desc(mean_salary))

#set up a currency data frame
exchange_rate <- c(1, 1.21, 0.14, 0.73, 0.12, 0.13)
currency_data <- data.frame(Country = target_country,
                            rate = exchange_rate)

job_location <- merge(job_location, currency_data)

job_location$adjusted_salary <- job_location$mean_salary*job_location$rate

# Which country has the highest salary
job_location <- job_location %>% 
  arrange(desc(adjusted_salary))

# what if we add purchasing power parity into consideration (PPP)
# PPP data was gained from OECD website, since there are only 6 data to collect, I just copied the data instead of merging a whole ppp data.
ppp <- c(1, 0.7, 4.18, 0.84, 5.87, 5.87)
ppp_data <- data.frame(Country = target_country,
                            ppp = ppp)
job_location <- merge(job_location, ppp_data)

job_location$ppp_salary <- job_location$adjusted_salary*job_location$ppp

# Which country has the highest salary after ppp
job_location <- job_location %>% 
  arrange(desc(ppp_salary))

# Which country have more emphasize on 'Data'

library(stringr)
# count data
Job_clean$data_count <-  stringr::str_count(Job_clean$`Job Description`, '[Dd]ata')
+ stringr::str_count(Job_clean$skills, '[Dd]ata')
+ stringr::str_count(Job_clean$Responsibilities, '[Dd]ata')

data <- Job_clean %>% 
  group_by(Country) %>% 
  summarise(data_count = sum(data_count)) %>% 
  arrange(desc(data_count))

data
# China has more emphasize on data among this data set, compared with other target countries.

# Which month has more opportunity of hiring?

library(ggplot2)

Job_number <- Job_clean %>% 
  filter(Job_year == 2022) %>% 
  mutate(month = month(`Job Posting Date`)) %>% 
  group_by(Job_year, month) %>% 
  summarise(number = n()) %>% 
  arrange(Job_year, month) %>% 
  mutate(date = paste(Job_year, month, sep = '-'))

ggplot(Job_number, aes(month, number)) +
  geom_point()+
  theme_minimal()+
  geom_line()+
  labs(x = "Month", 
       y = "Number of Job Posting", 
       title = "Number of Job Posting in 2022")




