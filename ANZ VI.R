getwd()
setwd("C:\\Users\\hp\\Desktop\\Career Development\\Independant Projects")

install.packages("readxl")
library(readxl)

ANZ=read_xlsx("ANZ synthesised transaction dataset.xlsx")

install.packages("janitor")
library(janitor)

install.packages("pacman")
library(pacman)

p_load(dplyr,here)

glimpse(ANZ)

#Issues Found
# rename long-lat to TxnLong-Lat
# make the names better using clean_names()
# remove long_lat(after renaming) merchant_code and bpay_biller_code
# (Around 93% data missing in the columns)  

ANZ_cleaned1 = ANZ %>% remove_empty("cols")%>%remove_empty("rows") %>% 
  clean_names() %>% mutate(TXN_long_lat=long_lat) %>% 
  select(-long_lat,-bpay_biller_code,-merchant_code)

ANZ
glimpse(ANZ_cleaned1)

lapply(ANZ_cleaned1,count(is.na(ANZ_cleaned1)))

sum(is.na(ANZ_cleaned1$card_present_flag))

# For empty rows in all columns
na_count <-sapply(ANZ_cleaned1, function(y) sum(length(which(is.na(y)))))
na_count= data.frame(na_count)
na_count

# The details of the
# merchant (merchant_id,merchant_suburb,merchant_state,merchant_long_lat)
# are missing for 4326 transactions.

# These transactions are CNP(card not present) in nature- 
#a seperate analysis needed for this category.
 
# TO bring out time seperately for analysis

DateTime<- data.frame(do.call('rbind', strsplit(as.character(ANZ_cleaned1$extraction),'T',fixed=TRUE)))
DateTime
OnlyTime=data.frame(do.call('rbind', strsplit(as.character(DateTime$X2),'.',fixed=TRUE)))
OnlyTime


as.POSIXct(foo$X1,format="%H:%M:%S")

transaction_time=OnlyTime$X1

ANZ_cleaned1$transaction_time =transaction_time
ANZ_cleaned1=ANZ_cleaned1 %>% select(-extraction)
glimpse(ANZ_cleaned1)

#seperating the hours,minutes and seconds for analysis  
typeof(ANZ_cleaned1$transaction_time)
HMS1 <- data.frame(do.call('rbind', strsplit(as.character(ANZ_cleaned1$transaction_time),':',fixed=TRUE)))
HMS1
ANZ_cleaned1$Hr=HMS1$X1
ANZ_cleaned1$Min=HMS1$X2
ANZ_cleaned1$Sec=HMS1$X3

YMD1 <- data.frame(do.call('rbind', strsplit(as.character(ANZ_cleaned1$date),'-',fixed=TRUE)))
head(YMD1)
ANZ_cleaned1$Year=YMD1$X1
ANZ_cleaned1$Mon=YMD1$X2
ANZ_cleaned1$Day=YMD1$X3

glimpse(ANZ_cleaned1)


#statewise Transaction count

ggplot(data = ANZ_cleaned1) +
  geom_bar(mapping = aes(x=merchant_state))

# VIC>NSW>QLD>WA>SA>TAS

ggplot(data = ANZ_cleaned1) +
  geom_bar(mapping = aes(x=Mon))

#Pretty much same number of transactions in the three months (approximately 4000)

ANZ_cleaned1$time_SM <- as.numeric(ANZ_cleaned1$Hr)+as.numeric(ANZ_cleaned1$Min)/60
 

ANZ_cleaned1$time_SM

ANZ_cleaned1$timecuts <- cut(ANZ_cleaned1$time_SM,seq(0,24,0.05))
ANZ_cleaned1$timecuts

s1=ANZ_cleaned1 %>% group_by(time_SM) %>% summarise(total_amount_pr_time2=sum(amount))

s1$Average_Amt_Per_Day=s1$total_amount_pr_time2/92
s1$Average_Amt_Per_Week=s1$total_amount_pr_time2/13
max(s1$Average_Amt_Per_Day)

ggplot(s1,aes(x=time_SM,y=s1$Average_Amt_Per_Day)) + geom_line() +
  scale_x_continuous('Time',labels=c(0:24),breaks = c(0:24)) +
  scale_y_continuous('Amount Withdrawn',
                     labels=c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000),
                     breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000))

ggplot(s1,aes(x=time_SM,y=s1$Average_Amt_Per_Week)) + geom_line() +
  scale_x_continuous('Time',labels=c(0:24),breaks = c(0:24)) +
  scale_y_continuous('Amount Withdrawn',
                     labels=c(0,5000,10000,15000,20000,25000,30000,35000,40000),
                     breaks=c(0,5000,10000,15000,20000,25000,30000,35000,40000))

# ggplot(ANZ_cleaned1,aes(x=transaction_time,y=amount)) + geom_point() +
#   scale_x_continuous('Time') +
#   scale_y_continuous('Amount Withdrawn',labels=c(0,10000,20000,30000,40000,50000))
# 
# ANZ_cleaned1$timecuts <- cut(ANZ_cleaned1$jptime,seq(0,24,1))
# 
# ANZ_cleaned1 %>% 
#   group_by(timecuts) %>% 
#   summarise(total_amount_pr_time2=sum(amount)) %>%
#   ggplot(aes(x=as.factor(timecuts),y=total_amount_pr_time2)) + geom_bar(stat='identity') +
#   scale_x_discrete('Time') +
#   scale_y_continuous('Amount Withdrawn',labels=c(0,10000,20000,30000,40000,50000))


#Pretty much same  of transactions in the three months

#location wise insights

Median_Age_Statewise=ANZ_cleaned1 %>% group_by(merchant_state) %>% summarise(Median_Age_Customer=median(age))
Median_Age_Statewise
# merchant_state             Median_Age_Customer
# <chr>                        <dbl>
#   1 ACT                             34
# 2 NSW                             29
# 3 NT                              21
# 4 QLD                             26
# 5 SA                              34
# 6 TAS                             28
# 7 VIC                             28
# 8 WA                              25
# 9 NA                              29

Transactions_Statewise=ANZ_cleaned1 %>% group_by(merchant_state) %>% summarise(count=n())
Transactions_Statewise
# 
# merchant_state count
# <chr>          <int>
#   1 ACT               73
# 2 NSW             2169
# 3 NT               205
# 4 QLD             1556
# 5 SA               415
# 6 TAS               68
# 7 VIC             2131
# 8 WA              1100
# 9 NA              4326

Transactions_Suburbwise=ANZ_cleaned1 %>% group_by(merchant_suburb) %>% summarise(count_t=n()) %>% arrange(count_t)
Transactions_Suburbwise=Transactions_Suburbwise[order(Transactions_Suburbwise$count_t,decreasing=TRUE),]
Transactions_Suburbwise[1:6,]

# Top5 Suburbs
# merchant_suburb count_t
# Melbourne           255
# Sydney              233
# Southport            82
# Brisbane City        79
# Chatswood            55

  
  library(writexl)
write_xlsx(ANZ_cleaned1,"C:\\Users\\hp\\Desktop\\Career Development\\Independant Projects\\ANZ_Cleaned.xlsx",col_names = TRUE)
