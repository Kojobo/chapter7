library(curl)
library(data.table)
library(stringr)
library(tidyverse)

#retrieve data from Dept of Justice site

bank_2000 <- fread("https://www.justice.gov/ust/file/452641/download")
bank_2001 <- fread("https://www.justice.gov/ust/file/452646/download")
bank_2002 <- fread("https://www.justice.gov/ust/file/452651/download")
bank_2003 <- fread("https://www.justice.gov/ust/file/452656/download")
bank_2004 <- fread("https://www.justice.gov/ust/file/452661/download")
bank_2005 <- fread("https://www.justice.gov/ust/file/452666/download")
bank_2006 <- fread("https://www.justice.gov/ust/file/452671/download")
bank_2007 <- fread("https://www.justice.gov/ust/file/452676/download")
bank_2008 <- fread("https://www.justice.gov/ust/file/452681/download")
bank_2009 <- fread("https://www.justice.gov/ust/file/452686/download")
bank_2010 <- fread("https://www.justice.gov/ust/file/452691/download")
bank_2011 <- fread("https://www.justice.gov/ust/file/452696/download")
bank_2012 <- fread("https://www.justice.gov/ust/file/452701/download")
bank_2013 <- fread("https://www.justice.gov/ust/file/452706/download")
bank_2014 <- fread("https://www.justice.gov/ust/file/452631/download")
bank_2015 <- fread("https://www.justice.gov/ust/file/ch7data2015.csv/download")
bank_2016 <- fread("https://www.justice.gov/ust/file/ch7data2016.csv/download")
bank_2017 <- fread("https://www.justice.gov/ust/file/ch7data2017.csv/download")
bank_2018 <- fread("https://www.justice.gov/ust/file/ch7data2018.csv/download")

#rename columns so that all years have consistent naming 

CH7_colnames <- colnames(bank_2018)


colnames(bank_2017) <- CH7_colnames
colnames(bank_2016) <- CH7_colnames
colnames(bank_2015) <- CH7_colnames
colnames(bank_2014) <- CH7_colnames
colnames(bank_2013) <- CH7_colnames
colnames(bank_2012) <- CH7_colnames
colnames(bank_2011) <- CH7_colnames
colnames(bank_2010) <- CH7_colnames
colnames(bank_2009) <- CH7_colnames
colnames(bank_2008) <- CH7_colnames
colnames(bank_2007) <- CH7_colnames
colnames(bank_2006) <- CH7_colnames
colnames(bank_2005) <- CH7_colnames
colnames(bank_2004) <- CH7_colnames
colnames(bank_2003) <- CH7_colnames
colnames(bank_2002) <- CH7_colnames
colnames(bank_2001) <- CH7_colnames
colnames(bank_2000) <- CH7_colnames

#combine all data tables from 2001 to 2018

bankrupt <-  rbind(bank_2001, bank_2002, bank_2003, bank_2004, bank_2005,
                   bank_2006, bank_2007, bank_2008, bank_2009, bank_2010,
                   bank_2011, bank_2012, bank_2013, bank_2014, bank_2015,
                   bank_2016, bank_2017, bank_2018)

#change column names to all lower case 

colnames(bankrupt) <- tolower(CH7_colnames)

#replace MASSACHUSETTS with MA

bankrupt$state <- str_replace(bankrupt$state, "MASSACHUSETTS", "MA")

#change all office names to upper case

bankrupt$office <- toupper(bankrupt$office)

#change all state abrev to upper case

bankrupt$state <- toupper(bankrupt$state)

#add size column and assign asset size category #small, medium, large

bankrupt$size <- cut(bankrupt$total_gross_receipts, 
                     breaks = c(-Inf, 4999, 499999, Inf),
                     labels = c("small", "medium", "high"))
