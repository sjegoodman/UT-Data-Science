# Install and load packages
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# Use read.transactions to load the file
tr <- read.transactions('ElectronidexTransactions2017.csv', format = 'basket', sep=',')

#inspect and get to know the data
summary(tr)
inspect(tr[0:10])
length(tr)
size(tr)
LIST(tr[0:10])
itemLabels(tr)
itemLabels(tr2)
# Create a plot of most purchased items
itemFrequencyPlot(tr, topN=10)
itemFrequencyPlot(tr, topN=10, type='absolute', xlab = "Best Selling Products")

# Use the image function to visualeze a sample of 100 transactions
image(sample(tr, 100))

# Use hte apriori algorithm to find associations in the dataset
RulesName<- apriori(tr, parameter = list(supp = 0.01, conf = 0.4, minlen = 2, maxtime=60))

# Inspect the list of rules and sort by lift
inspect(sort(RulesName, by = "lift")[0:15])

# Inspect the list of rules by finding certain product name
ItemRules <- subset(RulesName, items %in% "HP Laptop")

inspect(sort(ItemRules, by = "lift")[0:15])

# Check if any rules are redundant
is.redundant(RulesName)

# Plot some of the rules
# A graph of the top 10 rules
plot(sort(RulesName, by = "lift")[0:4], method="graph", control=list(type="items")) 

# A graph of HP Laptop rules
plot(sort(ItemRules, by = "lift")[0:10], method="graph", control=list(type="items")) 

# New strategy - consolidate items into categories to see larger trends
tr2 <- read.transactions('ElectronidexTransactions2017-edited.csv', format = 'basket', sep=',')
#inspect and get to know the data
summary(tr2)
inspect(tr2[0:10])
length(tr2)
size(tr2)
LIST(tr2[0:10])
itemLabels(tr2)
# Create a plot of most purchased items
itemFrequencyPlot(tr2, topN=10)
itemFrequencyPlot(tr2, topN=10, type='absolute', xlab = "Best Selling Products")

# Use the image function to visualeze a sample of 100 transactions
image(sample(tr, 100))

# Use the apriori algorithm to find associations in the dataset
RulesName2<- apriori(tr2, parameter = list(supp = 0.06, conf = 0.6, minlen = 2, maxtime=60))
summary(RulesName2)

# Inspect the list of rules and sort by lift
inspect(sort(RulesName2, by = "lift"))

# Inspect the list of rules by finding certain product name
ItemRules2 <- subset(RulesName2, items %in% "iMac")

inspect(sort(ItemRules2, by = "lift")[0:5])

# Check if any rules are redundant
is.redundant(RulesName2)


# Plot some of the rules
# A graph of the top 10 rules
plot(sort(RulesName2, by = "lift")[0:3], method="graph", control=list(type="items")) 
plot(sort(RulesName2, by = "lift")[4:6], method="graph", control=list(type="items"))
