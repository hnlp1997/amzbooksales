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

#_________________________________________________________________________________________________________________
#Analysis
### Method 1 - Regression Analysis
amazon$genre_factor <- as.factor(amazon$genre)

#does price have an effect on sales while controlling for other variables?
reg1<-lm(sales~price+genre_factor+user_rating+factor(year)+factor(author), data=amazon)
stargazer(reg1, type = "text", keep = c("genre_factor", "price","user_rating", "author"), se=list(cse(reg1)))

ggplot(amazon, aes(sales, price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sales", y = "Prices")

#validate with N-fold Validation
set.seed(2023)
trainfold <- trainControl(method="cv", number=10, savePredictions = TRUE)

modelfold <- train(sales~price, data=amazon, method = "lm", trControl=trainfold)
summary(modelfold)

modelfold$finalModel
modelfold$pred
modelfold$resample

#Both RMSE (around 350,000 - 500,000) and MAE (around 300,000) are relatively consistent, meaning that the prediction is replicated relatively accurately across all the folds. 

#let's double check using a linear model
reg2 <- lm(sales~price, data=amazon)
stargazer(reg2, type = "text", se=list(cse(reg2)))

# In the first technique, we decided to use multiple regression to uncover the effect of listed book price on number of sales of such book. The reason we did this is to understand whether how expensive a book is listed on Amazon have a positive or negative, or no effect on how well this book sells. Additionally, we include control variables such as genre (as a factor variable), buyer rating (out of 5 stars), and authors (as a factor variable). Together, the findings of this regression should inform us about elements that could potentially lead to a bestseller. 

# The regression shows that with each dollar increase in price, the number of sales decrease by around 5,664 units. This result is significant at 90% level. Additionally, Non-fiction books are predicted to perform better than fiction books, outselling them by 89,421 units on average, while being significant at 90% level as well. Notably, we found that buyer ratings are not found to lead to higher sales numbers, since the regression model found no significant relationship. 

# We want to validate it to see if this insight is indeed accurate. Using an N-fold technique, the model predicts similar results, with each dollar increase in price, the number of sales decrease by around 4,725 units, with the constant being 540,032.9. This result is shown to be 99% significant. Using a simple regression as comparison, the regression model predicts a very similar result to the n-fold model. More importantly, across all the folds, both RMSE (around 350,000 - 500,000) and MAE (around 300,000), meaning that the prediction is replicated accurately across all the folds.

#_________________________________________________________________________________________________________________
### Method 2 - Cluster Analysis
#Prepare Data for Cluster Analysis

#group by author
author <- amazon %>% group_by(author) %>% summarise(n=n() , mean_reviews = mean(reviews), mean_price = mean(price), mean_rating = mean(user_rating), mean_sales = mean(sales), mean_revenue = mean(revenue), genre=genre)
author <- author[!duplicated(author),]
df2 <- scale(author[c(2:7)])  
df3 <- as.data.frame(df2)

#correlations
cor(df2)

#find optimal numbers of clusters
#elbow score
fviz_nbclust(df3, kmeans, method = "wss") # suggests that 6-clusters is the best

#silhouette score
fviz_nbclust(df3,kmeans,method="silhouette") # suggests that 2-clusters is the best

#_________________________________________________________________________________________________________________
#Hierarchical Cluster
distance=dist(df3)
df3.hcluster <- hclust(distance)

plot(df3.hcluster)

dend <- as.dendrogram(df3.hcluster)   #save the cluster as a dendrogram
labels(dend) <- author$author 
dend <- color_labels(dend, k=10) 
dend <- color_branches(dend, k=10)

dend <- assign_values_to_leaves_nodePar(dend,19,"pch")
dend <- assign_values_to_leaves_nodePar(dend, 0.5,"lab.cex") 
plot(dend)

#Character clusters based on cutree
hcluster=cutree(df3.hcluster,4)   #specify 6 cuts here

#tabulate membership
table(hcluster)   #look at the cuts

#look at characteristics by cluster
aggregate(df3, list(hcluster),mean)

#_________________________________________________________________________________________________________________
#K-Means cluster analysis

#2 clusters
set.seed(123)
cluster.df3 <- kmeans(df3,2,nstart = 20)

#now we put the clustered data into the original dataframe 'amazon'
author$cluster2 <- cluster.df3$cluster #this line adds the cluster as a new column to df

#create customer by cluster
trans_cluster2 <- author %>% group_by(cluster2) %>% summarise(count=n(),price = mean(mean_price),
                                                              frequency = mean(n), 
                                                              rating = mean(mean_rating),
                                                              sales = mean(mean_sales),
                                                              revenue = mean(mean_revenue))

#look at cluster centers on each of the variables
cluster.df3$centers[1:2]
cluster_center_df <- data.frame("cluster"=c(1,2),
                                "center"=cluster.df3$centers[1:2])

#Is there a significant difference in mean value between clusters?
model3 <- aov(center~cluster,data = cluster_center_df)
summary(model3)

cluster.df3[["centers"]]

wss2 <- sum(cluster.df3$withinss)

#plot k-means
fviz_cluster(cluster.df3, data = author[c(2:7)],
             palette = c("#2E9FDF", "#b14c5f"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#6 clusters
cluster5.df3 <- kmeans(df3,6,nstart = 20)

#now we put the clustered data into the original dataframe df
author$cluster6 <- cluster5.df3$cluster   #this line adds the cluster as a new column to dt

trans_cluster2 <- author %>% group_by(cluster6) %>% summarise(count=n(),price = mean(mean_price),
                                                              frequency = mean(n), 
                                                              rating = mean(mean_rating),
                                                              sales = mean(mean_sales),
                                                              revenue = mean(mean_revenue))

#look at cluster centers on each of the variables
cluster5.df3$centers[1:6]
cluster5_center_df <- data.frame("cluster"=c(1,2,3,4,5,6),
                                 "center"=cluster5.df3$centers[1:6])

#Is there a significant difference in mean value between clusters?
model4 <- aov(center~cluster,data = cluster5_center_df)
summary(model4)

cluster5.df3[["centers"]]

wss3 <- sum(cluster5.df3$withinss)

#plot k-means
fviz_cluster(cluster5.df3, author[c(2:7)],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#b14c5f","#729e53","#D2B4DE"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

author$genre[author$cluster2==1]
author$genre[author$cluster6==6]

