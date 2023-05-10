#MKTG 2505 Final Project - Amazon

# setting Up
rm(list = ls())

# added libraries
library(dplyr)
library("tidyr")
library(car)
library(countrycode)
library(foreign)
library(DAAG)
library(epiDisplay)
library(ggplot2)
library(skimr)
library(doBy)
library(stargazer)
library(sandwich)
library(readxl)
library(AER)
library(tidyverse)
library(modelsummary)
library(expss)
library(Metrics)
library(caret)
library(stats)
library(janitor)
library(arules)
library(arulesViz)
library(factoextra)
library(dendextend)
library(tidytext)
library(tm)
library(topicmodels)
library(tidyr)
library(tidyverse)
library(ldatuning)

cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

### Import and Clean up Data Set:

# initial data set is titled 'bestsellers_with_categories'
amazon <- bestsellers_with_categories
amazon <- clean_names(amazon)

#_________________________________________________________________________________________________________________
#EDA & Plotting
#1, Generate total sales & revenue, assuming 2.5% of buyers leave reviews on Amazon based on market research
amazon$sales <- amazon$reviews * (1/0.025)
amazon$revenue <- amazon$sales * amazon$price

#Research shows that on Amazon Book Store, about 2.5% of the customers who purchased books leave a review. Therefore, using this as the basis, we multiply review variable by (1/0.025) to estimate total number of sales for each book.
#Then, we multiply number of sales by the listed price to estimate total revenue each book bring to Amazon. 
#Together, they make up the performance of each book on Amazon. 

#2, Box plot (fiction vs nonfiction mean price)
ggplot(amazon,aes(x=genre,y=price)) + geom_boxplot(aes(fill=genre))

#3, Stack plot (author by total revenue / total units of sales / rating)
author <- amazon %>% group_by(author) %>% summarise(n=n() , mean_rating = mean(user_rating), mean_sales = mean(sales), mean_revenue = mean(revenue))

author_top_rating <- amazon %>% group_by(author) %>% summarise(n=n() , mean_rating = mean(user_rating), mean_sales = mean(sales), mean_revenue = mean(revenue)) %>% slice_max(mean_rating,n=10)
author_top_sales <- amazon %>% group_by(author) %>% summarise(n=n() , mean_rating = mean(user_rating), mean_sales = mean(sales), mean_revenue = mean(revenue)) %>% slice_max(mean_sales,n=30)
author_top_revenue <- amazon %>% group_by(author) %>% summarise(n=n() , mean_rating = mean(user_rating), mean_sales = mean(sales), mean_revenue = mean(revenue)) %>% slice_max(mean_revenue,n=30)

#by rating
ggplot(author_top_rating,aes(x=reorder(author,-mean_rating),y=mean_rating, fill=author)) + 
  geom_col(position="stack") + ggtitle("Average Ratings by Author") + 
  xlab("Author") + ylab("mean rating") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# not an effective visual because the top 10 authors all have the same rating of 4.9

#by sales
ggplot(author_top_sales,aes(x=reorder(author,-mean_sales),y=mean_sales, fill=author)) + 
  geom_col(position="stack") + ggtitle("Average Sales by Author") + 
  xlab("Author") + ylab("mean sales") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

#by revenue
ggplot(author_top_revenue,aes(x=reorder(author,-mean_revenue),y=mean_revenue, fill=author)) + 
  geom_col(position="stack") + ggtitle("Average Revenue by Author") + 
  xlab("Author") + ylab("mean revenue") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

#4, Line Chart: Total Revenue for Amazon Books based on Genre from 2009-2019
groupbyyear <-amazon %>% group_by(year) %>% summarize(n=n())
# we notice that every year from 2009-2019 has 50 books
library(ggplot2)
library(scales)

groupbygenre <- amazon %>% group_by(genre) %>% 
  summarise(count = n(), mean_rating = mean(user_rating), mean_reviews = mean(reviews), 
            mean_price = mean(price), mean_sales = mean(sales), mean_revenue = mean(revenue)) 
# we notice that while fiction has 70 less books than nonfiction, it performs better both in terms of 
# revenue and in sales

df_nonfiction<-amazon[amazon$genre == 'Non Fiction',]
df_fiction<-amazon[amazon$genre == 'Fiction',]
gbyear_nonfiction<-df_nonfiction %>% group_by(year) %>% summarize(n=n(), mean_rev = mean(revenue),mean_reviews=mean(reviews),mean_sales=mean(sales))
gbyear_fiction<-df_fiction %>% group_by(year) %>% summarize(n=n(), mean_rev = mean(revenue),mean_reviews=mean(reviews),mean_sales=mean(sales))

# Average Unit Sales for Amazon Books Based on Genre from 2009-2019
ggplot(NULL,mapping=aes(x=year,y=mean_sales)) + geom_line(data=gbyear_nonfiction,aes(col = "Non Fiction")) +
  geom_line(data=gbyear_fiction,aes(col = "Fiction")) +
  labs(title = "Average Unit Sales By Genre",
       subtitle = "2009-2019",
       x = "Year",
       y = "Unit Sales") + theme_minimal() + scale_x_continuous(breaks = c(2009:2019)) +
  labs(color='Genre') 

sum(gbyear_nonfiction$mean_sales) # 3,978,526 units sold for nonfiction
sum(gbyear_fiction$mean_sales) # 6,945,744 units sold for fiction

# Average Revenue for Amazon Books based on Genre from 2009-2019
ggplot(NULL,mapping=aes(x=year,y=mean_rev)) + geom_line(data=gbyear_nonfiction,aes(col = "Non Fiction")) +
  geom_line(data=gbyear_fiction,aes(col = "Fiction")) +
  labs(title = "Average Revenue by Genre",
       subtitle = "2009-2019",
       x = "Year",
       y = "Revenue") + theme_minimal() + scale_x_continuous(breaks = c(2009:2019)) +
  labs(color='Genre') 

sum(gbyear_nonfiction$mean_rev) # $53,137,470 revenue earned for nonfiction
sum(gbyear_fiction$mean_rev) # $76,127,081 revenue earned for fiction

