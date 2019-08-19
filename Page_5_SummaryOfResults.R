
###retrieve data from "retrieve_bankruptcy_data" file before running this code

library(tidyverse)
library(scales)

## Page 5 - Summary of Results 

# ASSET CASES

#bullet 1 #fact 1 #count asset cases closed from 2001 - 2018

case_closings <- nrow(bankrupt) #949,491 cases

#bullet 1 #fact 2 #number of years in period from 2001 - 2018

years <- length(unique(bankrupt$year_closed)) #18 years

#bullet 2 #fact 1 #asset case closings increased/decreased 

annual_case_closings <- bankrupt %>% 
  group_by(year_closed) %>% 
  summarise(Asset_cases_by_year = n())

ggplot(annual_case_closings, aes(x = factor(year_closed), y = Asset_cases_by_year)) +
  geom_col()

  #case closings INCREASED: 2001 - 2007 : 2009 - 2011 : 2012 - 2013
  #case closings DECREASED: 2007 - 2009 : 2011 - 2012 : 2013 - 2018

#B2-F2 #total case closings in 2018

closings_2018 <- nrow(bank_2018) #32,554 cases
  
#RECEIPTS

#B3-F1 #amount collected from 2001-2018

amount_collected <- comma(sum(bankrupt$total_gross_receipts)) # $44,066,675,362

#B3-F2 #years in period from 2001-2018

years <- length(unique(bankrupt$year_closed)) #18 years

#B4-F1 #trend in total receipts each year

yearly_receipts <-bankrupt %>% 
  group_by(year_closed) %>% 
  summarise(Total_receipts = sum(total_gross_receipts))

ggplot(yearly_receipts, aes(x = factor(year_closed), y = Total_receipts)) +
  geom_col()

  #spike in receipts from 2005 to 2006
  #relatively stable otherwise

#B4-F2 #year with lowest receipts

comma(min(yearly_receipts$Total_receipts)) # $1,251,555,278 #2002

view(yearly_receipts)

#B4-F3 #year with highest receipts

comma(max(yearly_receipts$Total_receipts)) # $3,475,952,556 #2015

view(yearly_receipts)

#SIZE OF CASES

#B5-F1 #proportion of small, medium, and large cases from year to year 

case_size <- bankrupt %>% 
  select(year_closed, size) %>% 
  group_by(year_closed, size) %>% 
  summarise(size_count = n())

  ggplot(case_size, aes(x = factor(year_closed), y = size_count, fill = fct_rev(factor(size)))) +
    geom_col(position = "fill")
  
  #proportion of small, medium, and large cases from year to year is stable
  
#B6-F1 #case size for the majority of cases
  
  case_size_majority <- case_size %>%  #remove high cases because they will never be majority
    filter(size != 'high') 
  
  ggplot(case_size_majority, aes(x = factor(year_closed), y = size_count, fill = size)) +
    geom_col(position = "fill") +
    geom_abline(slope = 0, intercept = 0.50)
  
  
    #small cases are the majority from 2001 - 2015
    #small and medium cases are almost equal in 2015 #25609 small vs 25608 medium
    #medium cases are the majority from 2016 - 2018
  
#B7-F1 #number of asset cases each year with >$500,000 in receipts
  
  large_asset_cases <- bankrupt %>% 
    filter(size == 'high') %>% 
    group_by(year_closed) %>% 
    summarise(case_count = n())
  
  range(large_asset_cases$case_count) #361 to 879 

#B7-F2 #proportion of receipts from large asset cases each year
  
  size_vs_receipts <- bankrupt %>% 
    group_by(year_closed, size) %>% 
    summarise(receipts = sum(total_gross_receipts, na.rm = T))

  ggplot(size_vs_receipts, aes(x = factor(year_closed), y = receipts, fill = size)) +
    geom_col(position = 'fill') +
    geom_abline(slope = 0, intercept = 0.50)
  
  #check to see if small + medium > high in 2004
  
  receipts_2004 <- size_vs_receipts %>% 
    filter(year_closed == 2004)

  sum(receipts_2004[1:2, 3]) > receipts_2004[3 , 3] #TRUE
  
    #Large cases account for more than half of all receipts in 2001, 2003, 2006 - 2018
    #Small and medium cases account for more than half of all receipts in 2002, 2004, and 2005
  