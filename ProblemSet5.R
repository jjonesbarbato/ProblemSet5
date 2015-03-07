#Lesson 5 Problem Set
library(datasets)
library(dplyr)
library(ggplot2)
setwd("C:\\Users\\jeniferjones\\SkyDrive\\SlideRule_DS_Workshop\\EDA\\Multi-Var")

#Question 1 - Create histogram of diamond prices, faceted by color and filled by cut

ggplot(aes(x=price), data = diamonds) + 
  geom_histogram(aes(fill=cut)) + 
  facet_wrap(~color) + 
  scale_fill_brewer(type = 'qual') + 
  scale_x_log10() + 
  scale_y_continuous(limits = c(0,600))
ggsave(filename="Question1.png")
dev.off()

#Question 2 - Create scatterplot of diamond price vs table and color based on cut
ggplot(aes(x=table, y = price), data = diamonds) + 
  geom_point(aes(color=cut)) + 
  scale_fill_brewer(type = 'qual') + 
  scale_x_continuous(limits = c(50,80), breaks = seq(50,80,2)) + 
  ylim(0,20000)
ggsave(filename="Question2.png")
dev.off()

#Question 3 - Answering questions based on the graph 
#Typical table range for majority of ideal cut diaomds: --> 53 - 57
#Typical table range for majority of premium  cut diaomds: --> 58 - 61

#Question 4 - Scatterplot of diamond price vs volume and color by clarity
#Y-axis is log10 of price, omit top 1% of volumes 
ggplot(aes(x=volume, y = price), data = diamonds) + 
  geom_point(aes(color=clarity)) + 
  scale_fill_brewer(type = 'div') + 
  scale_y_log10() + 
  coord_cartesian(xlim=c(0, quantile(diamonds$volume, 0.99)))

ggsave(filename="Question4.png")
dev.off()

#Question 5 - Returning to facebook dataset - create prop_initiated to calc the proportion of friendships_initiated
filename<-"pseudo_facebook.tsv"
pf<-read.csv('pseudo_facebook.tsv', sep='\t')

pf$prop_initiated<- pf$friendships_initiated/pf$friend_count

#QUestion 6 - Line graph of media proportaion from prior question vs tenure and 
#color line by year_joined
#Rerun code to create tenure and year joined from the lesson
pf$year_joined<-2014 - ceiling((pf$tenure/365))
#Using ceiling to round up to the next year and floor to round down 
summary(pf$year_joined)
table(pf$year_joined)
#Output is a table with record count by year joined
#Now we have these we can use the cut function to group, often with quantile
#Cut into 4 bins - from 2004 - 2009, 2009-2011, 2011-2012 ,2012-2014
interval<-c("2004", "2009", "2011", "2012", "2014")
pf$year_joined_bucket<-cut((pf$year_joined), breaks = interval)

ggplot(aes(x= tenure, y = prop_initiated), data = pf) + 
  geom_line(aes(color = year_joined_bucket), stat="summary", fun.y = median)
ggsave(filename="Question6.png")
dev.off()

#Question 7 - Smooth the plot by using larger bins or adding a smoother
ggplot(aes(x= tenure, y = prop_initiated), data = pf) + 
  geom_smooth(aes(color = year_joined_bucket), stat="smooth", fun.y = median) 
ggsave(filename="Question7.png")
dev.off()

#Question 8 - Questions using the plot to answer 
#On average which group initiated the greatest proportion? Peole after 2012

#Question 9 - Questions using the plot to answer 
#For the group with largest proportion - what is avarege proportion? 0.6654
#WHy do you think this is higher? Because the number of users on facebook has increased over time and this 
#allows people to find more friends. 
#Finding the mean using the summary and subset functions 
summary(pf$prop_initiated, subset(pf, year_joined_bucket == "(2012,2014]"))

#Question 10 - Create scatter plot of price/carat ratio of diamonds
#x = cut, points colored by color and faceted by clarity 
#Learned more about jitter which was needed to move the points some so that I could see the seperations
ggplot(aes(x=cut, y = (price/carat)), data = diamonds) + 
  geom_point(aes(color=diamonds$color), position = position_jitter(width=0.4)) + 
  facet_wrap(~clarity) + 
  scale_fill_brewer(type = 'div') 
ggsave(filename="Question10.png")
dev.off()


