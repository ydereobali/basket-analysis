########################################################################
# Title: Basket Associations
# Author: Yigit Dereobali
# Name: Ubiqum Module 2 Task 4
# Description: Find associations among products in the given baskets
# Date: 22.05.2019
# Version: 1.1
########################################################################

# LIBRARIES ####
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(data.table)
library(plotly)
library(stats)
library(arulesViz)
library(arules)


# IMPORT DATA ####
# import transaction data
mydata <- read.transactions("ElectronidexTransactions2017.csv",
                            format = "basket", sep=",",
                            rm.duplicates = TRUE , encoding = "utf8")
# import existing sales data for comparison
blackwell <- fread("ubiqum m2 3 existingproductattributes2017.csv")

# PRE ANALYSIS ####
# analyse transaction baskets
length (mydata)
inspect (mydata)
size (mydata)
LIST(mydata)
itemLabels(mydata)
sort(itemFrequency(mydata, type = "absolute"))



# PLOTS ####
# plot top 10 most frequent products absolute number
itemFrequencyPlot(mydata,  type = "absolute", horiz= TRUE, cex.names = 0.5, support = 0.2, topN = 10)
# find top 10 most frequent products relative number
itemFrequencyPlot(mydata, type = "relative", horiz= TRUE, cex.names = 0.5, topN = 10)
# print top 10 most frequent products and appearance count in baskets
sort(itemFrequency(mydata, type = "absolute"), decreasing = TRUE)[1:10]


# APRIORI ALGO ####
# apply apriori algo to transaction data to generate rules
Rules<- apriori(mydata, parameter = list(supp = 0.03, conf = 0.02, minlen = 2))
# remove redundant rules
Rules <- Rules[!is.redundant(Rules)]
# remove rules with =< 1 list
Rules <- Rules[quality(Rules)$lift > 1]
# sort rules by lift
Rules <- sort(Rules, by = "lift")
# inspect and plot rules
inspect(Rules)
summary(Rules)
plot(Rules, method="graph", control=list(type = "items", cex = 0.5))

# generate less rules by increasing the support level
Rules.Less<- apriori(mydata, parameter = list(supp = 0.05, conf = 0.02, minlen = 2)) # maxlen =
Rules.Less <- Rules.Less[!is.redundant(Rules.Less)]
Rules.Less <- Rules.Less[quality(Rules.Less)$lift > 1]
Rules.Less <- sort(Rules.Less, by = "lift")
inspect(Rules.Less)
summary(Rules.Less)
plot(Rules.Less, method="graph", control=list(type = "items", cex = 0.5))


# iMAC RULES ####
# check rules for iMac only
iMac.Rules <- subset(Rules, items %in% "iMac")
iMac.Rules <- iMac.Rules[1:10]
plot(iMac.Rules, method="graph", control=list(type="items", cex = 0.5)) 
inspect(iMac.Rules)


# HPLaptop RULES ####
# check rules for HP Laptop only
HPLaptop.Rules <- subset(Rules, items %in% "HP Laptop")
HPLaptop.Rules <- HPLaptop.Rules[1:10]
plot(HPLaptop.Rules, method="graph", control=list(type="items", cex = 0.5)) 
inspect(HPLaptop.Rules)


# Cyberpower RULES ####
# check rules for Cyberpower Gamer Desktop only
Cyberpower.Rules <- subset(Rules, items %in% "CYBERPOWER Gamer Desktop")
Cyberpower.Rules <- Cyberpower.Rules[1:10]
plot(Cyberpower.Rules, method="graph", control=list(type="items", cex = 0.5)) #method="grouped
inspect(Cyberpower.Rules)


# END ####
