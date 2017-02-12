library(arules)

# Remove dupicates
RemoveDuplicates <- function(d) {
  return(d[!duplicated(d),])
}

# Replace all NA with value
ReplaceNa <- function(d) {
  # Calculate the mean without the NAs
  v <- mean(d[!is.na(d)])
  
  # Replace NAs with the mean
  d[is.na(d)] <- v
  return(d)
}

# Check for non outliers
IsNotOutlier <- function(d) {
  box_stats <- boxplot.stats(d)
  return(!is.element(d,box_stats$out))
}

# Discretize the data
DiscretizeData <- function(d, method = "interval") {
  
  # Get the boundaries
  cuts <- discretize(d, method = method, categories = 3, ordered = T, onlycuts = T)
  
  # Discretize
  temp <- ifelse((d < cuts[2]), 1, ifelse((d < cuts[3]), 2, 3))
  
  return(factor(x=temp, labels = c("A", "B", "C"), ordered = T))
}

# Read the data
iris.data <- read.csv("iris2.csv")

# Remove the duplicates
iris.data <- RemoveDuplicates(iris.data)

# Remove NA
iris.data$Sepal.Width <- ReplaceNa(iris.data$Sepal.Width)
iris.data$Sepal.Length <- ReplaceNa(iris.data$Sepal.Length)
iris.data$Petal.Width <- ReplaceNa(iris.data$Petal.Width)
iris.data$Petal.Length <- ReplaceNa(iris.data$Petal.Length)

# Remove outliers
iris.data <- iris.data[IsNotOutlier(iris.data$Sepal.Width),]
iris.data <- iris.data[IsNotOutlier(iris.data$Sepal.Length),]
iris.data <- iris.data[IsNotOutlier(iris.data$Petal.Width),]
iris.data <- iris.data[IsNotOutlier(iris.data$Petal.Length),]

# Save to csv file
write.csv(iris.data, file = "iris2_cleaned.csv", row.names = F,quote = F)

# Create two ordinal attributes
iris.data$Sepal.Width.int <- DiscretizeData(iris.data$Sepal.Width, "interval")
iris.data$Petal.Length.freq <- DiscretizeData(iris.data$Petal.Length, "frequency")

# Save the final csv file
write.csv(iris.data, file = "iris2_final.csv", row.names = F,quote = F)