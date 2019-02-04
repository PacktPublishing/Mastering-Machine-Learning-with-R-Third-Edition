library(magrittr)

set.seed(270)
faults <- data.frame(
  serialNumber = sample(1:20, 100, replace = T),
  faultCode = paste("fc", sample(1:12, 100, replace = T), sep = "")
)
faults <- unique(faults)
# table(faults$serialNumber)
# table(faults$faultCode)

str(faults)

faults$indicator <- TRUE

faults_wide <- tidyr::spread(faults, key = faultCode, value = indicator)
# Drop the transaction ID
faults_matrix <- as.matrix(faults_wide[,-1])

# Clean up the missing values to be FALSE
faults_matrix[is.na(faults_matrix)] <- FALSE

faults_transactions <- as(faults_matrix, "transactions")
arules::itemFrequencyPlot(faults_transactions, topN = 10)

###
install.packages("arules")

install.packages("arulesViz")

library(arules)

data(Groceries)

str(Groceries)

arules::itemFrequencyPlot(Groceries, topN = 10, type = "absolute")

arules::itemFrequencyPlot(Groceries, topN = 15)


rules <-
  arules::apriori(Groceries, parameter = list(
    supp = 0.001,
    conf =
      0.9,
    maxlen = 4
  ))

rules

options(digits = 2)

rules <- arules::sort(rules, by = "lift", decreasing = TRUE)

arules::inspect(rules[1:5])

rules <- arules::sort(rules, by = "confidence", decreasing = TRUE)

arules::inspect(rules[1:5])

tab <- arules::crossTable(Groceries)

tab[1:3, 1:3]

tab["bottled beer","bottled beer"]

tab["bottled beer","canned beer"]

beer.rules <- arules::apriori(
  data = Groceries,
  parameter = list(support
                   = 0.0015, confidence = 0.3),
  appearance = list(default = "lhs",
                    rhs = "bottled beer"))

beer.rules

beer.rules <- arules::sort(beer.rules, decreasing = TRUE, by = "lift")

tab["bottled beer", "red/blush wine"]
tab["red/blush wine", "red/blush wine"]
48/189
tab["white wine", "white wine"]
tab["bottled beer", "white wine"]
22/187

library(arulesViz)
plot(beer.rules,
     method = "graph",
     measure = "lift",
     shading = "confidence")
