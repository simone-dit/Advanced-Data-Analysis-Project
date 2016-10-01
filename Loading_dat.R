setwd('C:/Users/Jack/Desktop/Columbia Masters/Fall 2016 Courses/Advanced Data Analysis/Group Project/hillary-clinton-emails-kaggle')

library(ggplot2)
library(dplyr)


#reading tables
Aliases = read.csv('Aliases.csv')
EmailReceivers = read.csv('EmailReceivers.csv')
Emails = read.csv('Emails.csv')
Persons = read.csv('Persons.csv')


#getting table dimensions
dims = as.data.frame(rbind(
	t(c('Aliases', dim(Aliases))),
	t(c('Email Receivers', dim(EmailReceivers))),
	t(c('Emails', dim(Emails))),
	t(c('Persons', dim(Persons)))
))
names(dims) = c('Table Name', 'Rows', 'Columns')
print(dims)


#getting table fields
names(Aliases)
names(EmailReceivers)
names(Emails)
names(Persons)


#getting the number of emails sent by person
T = merge(x = Emails, y = Persons, by.x = 'SenderPersonId',
	 by.y = 'Id')
T2 = table(T$Name)
sorted_freq = sort(T2, decreasing = TRUE)


#getting the number of emails received by person
T3 = merge(x = EmailReceivers, y = Persons, 
		 by.x = 'PersonId', by.y = 'Id')
T4 = table(T3$Name)
sorted_freq_rec = sort(T4, decreasing = TRUE)

