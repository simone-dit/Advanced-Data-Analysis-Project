setwd('C:/Users/Jack/Desktop/Columbia Masters/Fall 2016 Courses/Advanced Data Analysis/Group Project/hillary-clinton-emails-kaggle')

library(ggplot2)
library(dplyr)
library(magrittr)

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

#getting the number of emails sent by person
num.emails.sent <-
    group_by(Emails, SenderPersonId) %>%
        summarise(count=n()) %>%
        inner_join(Persons, by=c("SenderPersonId"="Id"))

#getting the number of emails received by person
num.emails.recvd <-
    group_by(EmailReceivers, PersonId) %>%
        summarise(count=n()) %>%
        inner_join(Persons, by=c("PersonId"="Id"))


ggplot(arrange(num.emails.sent, desc(count)), aes(x=reorder(Name, -count), y=count)) +
    geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_x_discrete(name="Sender") +
                scale_y_continuous(name="Number of e-mails sent")

ggplot(arrange(num.emails.recvd[num.emails.recvd$count > 3,], desc(count)), aes(x=reorder(Name, -count), y=count)) +
    geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_x_discrete(name="Receiver") +
                scale_y_log10(name="Number of e-mails received")
