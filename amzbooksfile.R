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

### Import and Clean up Data Set
amazon <- read_csv("/Users/hnluupham/Downloads/bestsellers with categories.csv")
amazon <- clean_names(amazon)
