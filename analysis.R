
data <- read.csv("final-project/WtoData_20250209133853.csv")
head(data)

info <- data.frame(
  "Variable" = names(data),
  "Type" = sapply(data, class),
  "Unique Values" = sapply(data, function(x) length(unique(x))),
  "Missing Values" = sapply(data, function(x) sum(is.na(x)))
)
info

n <- length(unique(data$Reporting.Economy.Code))
n # originally 265
matrix <- matrix(0, nrow=n, ncol=n)
rownames(matrix) <- unique(data$Reporting.Economy.Code)
colnames(matrix) <- unique(data$Reporting.Economy.Code)

# TODO: can refer section 8 for clustering
