carPricePrediction <- read.csv("carPricePrediction.csv", stringsAsFactors = FALSE)
attach(carPricePrediction)
DataSet <- carPricePrediction
#Dimension of original data set
dim(DataSet)

###DUPLICATES###
# Identify duplicate rows
duplicates <- DataSet[duplicated(DataSet), ]
#find no of duplicates 
dim(duplicates)
#remove duplicates
DataSet <- DataSet[!duplicated(DataSet), ]
dim(DataSet)

#data types of variables
str(DataSet)

###MESSY VARIABLES###
#1. Levy
#Levy data type is "chr"
# Replace "-" with NA
DataSet$Levy[DataSet$Levy == "-"] <- NA
# Convert to numeric
DataSet$Levy <- as.numeric(DataSet$Levy)
# Check the structure to confirm the change
str(DataSet$Levy)
# Check for remaining missing values
sum(is.na(DataSet$Levy))

#2. Engine.volume
library(dplyr)
# Create a new Turbo indicator variable (1 if Turbo is present, 0 otherwise)
DataSet <- DataSet %>%
  mutate(Turbo = ifelse(grepl("Turbo", Engine.volume), 1, 0))
DataSet <- DataSet %>%
  relocate(Turbo, .after = Engine.volume)
# Remove "Turbo" text and convert Engine.volume to numeric
DataSet$Engine.volume <- as.numeric(gsub(" Turbo", "", DataSet$Engine.volume))
DataSet$Turbo <- as.character(DataSet$Turbo)

#3. Mileage
# Remove "km" text and convert Mileage to numeric
DataSet$Mileage <- as.numeric(gsub(" km", "", DataSet$Mileage))
#change the variable name(Mileage to Mileage(km))
colnames(DataSet)[colnames(DataSet) == "Mileage"] <- "Mileage(km)"

#4. Doors
# Replace incorrect values with correct ones
DataSet$Doors[DataSet$Doors == "04-May"] <- "4"
DataSet$Doors[DataSet$Doors == "02-Mar"] <- "2"
# Handling ">5" (You can decide what to do with it, for now, setting it to NA)
DataSet$Doors[DataSet$Doors == ">5"] <- "5"
# Convert Doors back to numeric
DataSet$Doors <- as.numeric(DataSet$Doors)

#5. Model
# Select only categorical columns
categorical_vars <- DataSet %>% select(where(is.character) | where(is.factor))
# Get unique categories for each categorical variable
lapply(categorical_vars, unique)
# Define a threshold for "popular models" (e.g., models with more than 50 occurrences)
popular_models <- names(which(table(DataSet$Model) > 50))
# Replace rare models with "Other"
DataSet$Model <- ifelse(DataSet$Model %in% popular_models, DataSet$Model, "Other")
DataSet$Manufacturer_Model <- paste(DataSet$Manufacturer, DataSet$Model, sep = "_")
DataSet <- DataSet %>%
  relocate(Manufacturer_Model, .after =Manufacturer )
DataSet <- DataSet %>% select(-Model)

#6. converting data type of cylinders
DataSet$Cylinders <- as.factor(DataSet$Cylinders)

#7.  converting data type of cylinders
DataSet$Airbags <- as.factor(DataSet$Airbags)


#8. converting data type of doors
DataSet$Doors <- as.factor(DataSet$Doors)

#data types
str(DataSet)

###MISSING VALUES###
colSums(is.na(DataSet))
#Levy - 5709
#proportion of missing values
no_rows <- nrow(DataSet)
MissingProp <- (5709 / no_rows) * 100
MissingProp
# 30.16804% of data are missing therefore we can remove from data set
DataSet <- DataSet %>% select(-Levy)
str(DataSet)

###Data Splitting###
#install.packages("caTools")
library(caTools)
# Set a random seed for reproducibility
set.seed(123)
# Split data: 80% training, 20% testing
split <- sample.split(DataSet$Price, SplitRatio = 0.8)  # Assuming 'Price' is the target variable
trainingSet <- subset(DataSet, split == TRUE)
testingSet  <- subset(DataSet, split == FALSE)
# Check dimensions
dim(trainingSet)  # Should be ~80% of total rows
dim(testingSet)   # Should be ~20% of total rows

#######EDA######
#Basic Summary of Data
summary(trainingSet)  # Summary statistics
str(trainingSet)      # Check structure of dataset

dim(trainingSet)      # Check number of rows & columns
#install.packages("ggplot2")
library(ggplot2)









###UNIVARIATE ANALYSIS###
###Target variable###
###1.PRICE###

#Distribution of Price
ggplot(trainingSet, aes(x = Price)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "Price Distribution", x = "Price", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Check for Outliers
ggplot(trainingSet, aes(y = Price)) +
  geom_boxplot(fill = "tomato", color = "black", outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "Price Outliers", y = "Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


library(dplyr)



#First, check the highest price values
high_prices <- trainingSet[order(-trainingSet$Price), c("Manufacturer", "Manufacturer_Model", "Price")]
head(high_prices, 20)  # View top 20 most expensive cars
tail(high_prices,20) # View top 20 most cheap cars


### Removing unusual values####
# Define lower and upper bounds using 1st and 99th percentiles
lower_bound <- quantile(trainingSet$Price, 0.02)  # 2st percentile
upper_bound <- quantile(trainingSet$Price, 0.98)  # 98th percentile

# Print values
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# Remove extreme values outside the 1st and 99th percentiles
trainingSet<- trainingSet[trainingSet$Price >= lower_bound & trainingSet$Price <= upper_bound, ]


###Numerical predictors###
### 1. Mileage
#Histogram
ggplot(trainingSet, aes(x = `Mileage(km)`)) + 
  geom_histogram(fill = "skyblue", color = "black", bins = 50) + 
  labs(title = "Distribution of Mileage", x = "Mileage(km)", y = "Frequency") + 
  theme_minimal()

#summary statistics of Mileage
summary(trainingSet$Mileage)


library(ggplot2)
library(dplyr)

# Identify Outlier Thresholds
Q1 <- quantile(trainingSet$`Mileage(km)`, 0.25, na.rm = TRUE)  # 25th percentile
Q3 <- quantile(trainingSet$`Mileage(km)`, 0.75, na.rm = TRUE)  # 75th percentile
IQR_value <- Q3 - Q1  # Interquartile Range

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Check for Outliers in Mileage
ggplot(trainingSet, aes(y = `Mileage(km)`)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "Mileage Outliers", y = "Mileage (km)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Outliers: Mileage above the upper bound
outliers <- trainingSet$`Mileage(km)`[trainingSet$`Mileage(km)` > upper_bound]
outlier_count <- length(outliers)  # Count of outliers

# Calculate Outlier Percentage
total_rows <- nrow(trainingSet)
outlier_percentage <- (outlier_count / total_rows) * 100
outlier_percentage

# Output conclusion about percentage
if (outlier_percentage > 5) {
  message("Outlier percentage is greater than 5%. Further investigation is required.")
} else {
  message("Outlier percentage is less than 5%. Outliers can be removed.")
}

# Summary of Mileage
summary(trainingSet$`Mileage(km)`)
quantile(trainingSet$`Mileage(km)`, probs = c(0.95, 0.99), na.rm = TRUE)  # Check 95th and 99th percentiles

# Check column names
colnames(trainingSet)

# First, check the highest mileage values
high_mileage <- trainingSet %>%
  arrange(desc(`Mileage(km)`)) %>%
  select(Manufacturer, Manufacturer_Model,Prod..year, `Mileage(km)`)

head(high_mileage, 20)  # View top 20 cars with highest mileage
tail(high_mileage, 20)  # View top 20 cars with lowest mileage

# Remove rows where Mileage is an outlier
trainingSet_clean <- trainingSet %>%
  filter(`Mileage(km)` >= lower_bound & `Mileage(km)` <= upper_bound)

# Boxplot before outlier removal
ggplot(trainingSet, aes(y = `Mileage(km)`)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Boxplot of Mileage Before Removing Outliers", y = "Mileage (km)") +
  theme_minimal()

# Boxplot after outlier removal
ggplot(trainingSet_clean, aes(y = `Mileage(km)`)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  labs(title = "Boxplot of Mileage After Removing Outliers", y = "Mileage (km)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#distribution of price for the cleaned dataset
ggplot(trainingSet, aes(x = Price)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "Price Distribution", x = "Price", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#log transformation for price
trainingSet_clean$LogPrice <- log1p(trainingSet_clean$Price)  # log1p() handles zero values

library(ggplot2)

ggplot(trainingSet_clean, aes(x = LogPrice)) + 
  geom_histogram(fill = "skyblue", color = "black", bins = 50) + 
  labs(title = "Log-Transformed Price Distribution", x = "LogPrice", y = "Frequency") + 
  theme_minimal()


summary(trainingSet_clean$LogPrice)
trainingSet_clean <- trainingSet_clean[, -which(names(trainingSet_clean) == "Price")]



str(trainingSet)

#Detecting multicollinearity
cor(trainingSet_clean[, sapply(trainingSet_clean, is.numeric)])
library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("corrr")  
library(corrr)             
# Compute correlation matrix for numerical variables
cor_matrix <- cor(trainingSet_clean[, sapply(trainingSet_clean, is.numeric)], use = "complete.obs")
# Load ggcorrplot library
library(ggcorrplot)
# Plot correlation heatmap
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE)
#this plot shows there is a multicollinearity in mileage and cylinders 



### 2. Engine volume
#Histogram
ggplot(trainingSet_clean, aes(x = Engine.volume)) + 
  geom_histogram(fill = "skyblue", color = "black", bins = 50) + 
  labs(title = "Distribution of Engine volume", x = "Engine volume", y = "Frequency") + 
  theme_minimal()

#Boxplot
ggplot(trainingSet_clean, aes(y = Engine.volume)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Boxplot of Engine Volume", y = "Engine Volume (L)") +
  theme_minimal()


###Categorical predictors###
### 1. Manufacturer
#install.packages("wordcloud")
library(wordcloud)
wordcloud(words = names(table(trainingSet_clean$Manufacturer)), 
          freq = table(trainingSet_clean$Manufacturer), 
          max.words = 50, colors = brewer.pal(8, "Dark2"))
table(trainingSet_clean$Manufacturer)  # Count of each category

### 2. Manufacturer_Model
#wordcloud(words = names(table(trainingSet$Manufacturer_Model)), 
# freq = table(trainingSet$Manufacturer_Model), 
#max.words = 50, colors = brewer.pal(8, "Dark2"))

### 3. Category
ggplot(trainingSet_clean, aes(x = Category)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Car Category", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 4. Leather Interior
ggplot(trainingSet_clean, aes(x = Leather.interior)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Leather Interior Distribution", x = "Leather Interior", y = "Count") +
  theme_minimal()


library(dplyr)

# Create a summary table for Leather.interior counts
leather_counts <- trainingSet_clean %>%
  count(Leather.interior) %>%
  mutate(perc = round(n / sum(n) * 100, 1))  # Calculate percentage

# Create the pie chart with percentages
ggplot(leather_counts, aes(x = "", y = n, fill = Leather.interior)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  theme_void() +  # Remove background grids
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # Show percentages
  labs(title = "Pie chat of Leather Interior", fill = "Leather Interior") +
  scale_fill_manual(values = c("skyblue", "salmon"))  # Customize colors


### 5. Fuel Type
ggplot(trainingSet_clean, aes(x = Fuel.type)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Distribution of Fuel Type", x = "Fuel Type", y = "Count") +
  theme_minimal()

### 6. Turbo
ggplot(trainingSet_clean, aes(x = Turbo)) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Distribution of Turbo", x = "Turbo", y = "Count") +
  theme_minimal()


# Create a summary table for   turbo counts
turbo_counts <- trainingSet_clean %>%
  count(Turbo) %>%
  mutate(perc = round(n / sum(n) * 100, 1))  # Calculate percentage

# Create the pie chart with percentages
ggplot(turbo_counts, aes(x = "", y = n, fill = Turbo)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  theme_void() +  # Remove background grids
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # Show percentages
  labs(title = "Pie chat of Turbo", fill = "Turbo") +
  scale_fill_manual(values = c("lightgreen", "red"))  # Customize colors



### 7. Gearbox Type
ggplot(trainingSet_clean, aes(x = Gear.box.type)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Gearbox Type Distribution", x = "Gearbox Type", y = "Count") +
  theme_minimal()

### 8. Drive Wheels
ggplot(trainingSet_clean, aes(x = Drive.wheels)) +
  geom_bar(fill = "darkolivegreen4", color = "black") +
  labs(title = "Distribution of Drive Wheels", x = "Drive Wheels", y = "Count") +
  theme_minimal()

# Create a summary table for drive wheel counts
drivewheel_counts <- trainingSet_clean %>%
  count(Drive.wheels) %>%
  mutate(perc = round(n / sum(n) * 100, 1))  # Calculate percentage

# Create the pie chart with percentages
ggplot(drivewheel_counts, aes(x = "", y = n, fill = Drive.wheels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  theme_void() +  # Remove background grids
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # Show percentages
  labs(title = "Pie chat of drive wheel", fill = "drive wheel") +
  scale_fill_manual(values = c("darkolivegreen4", "darkorchid4", "darkorange")) 

### 9. Steering Wheel
ggplot(trainingSet_clean, aes(x = Wheel)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Steering Wheel Position", x = "Wheel Position", y = "Count") +
  theme_minimal()

# Create a summary table for wheel counts
wheel_counts <- trainingSet_clean %>%
  count(Wheel) %>%
  mutate(perc = round(n / sum(n) * 100, 1))  # Calculate percentage

# Create the pie chart with percentages
ggplot(wheel_counts, aes(x = "", y = n, fill = Wheel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  theme_void() +  # Remove background grids
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust = 0.5), size = 5) +  # Show percentages
  labs(title = "Pie chat of wheel", fill = "wheel") +
  scale_fill_manual(values = c("darkorchid4", "darkorange")) 

### 10. car color
ggplot(trainingSet_clean, aes(x = Color)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Car Color Distribution", x = "Color", y = "Count") +
  theme_minimal()
wordcloud(words = names(table(trainingSet$Color)), 
          freq = table(trainingSet$Color), 
          max.words = 50, colors = brewer.pal(8, "Dark2"))

### 11. Airbags
ggplot(trainingSet_clean, aes(x = as.factor(Airbags))) +
  geom_bar(fill = "brown", color = "black") +
  labs(title = "Distribution of Airbags", x = "Number of Airbags", y = "Count") +
  theme_minimal()



### 12. Cylinders
ggplot(trainingSet_clean, aes(x = factor(Cylinders))) + 
  geom_bar(fill = "orange", color = "black") + 
  labs(title = "Bar Chart of Cylinders", x = "Number of Cylinders", y = "Count") +
  theme_minimal()

###BIVARIATE ANALYSIS###
str(trainingSet_clean)
library(ggplot2)
###LogPrice with category predictors
# Manufacturer vs logPrice
ggplot(trainingSet_clean, aes(x = Manufacturer, y = LogPrice)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Manufacturer vs Log Price")


# Compute average LogPrice for each Manufacturer
avg_log_price <- trainingSet_clean %>%
  group_by(Manufacturer) %>%
  summarise(Average_LogPrice = mean(LogPrice, na.rm = TRUE))  # Handle missing values if any

# Draw a bar chart 
ggplot(avg_log_price, aes(x = reorder(Manufacturer, -Average_LogPrice), y = Average_LogPrice)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +  # Bar chart with color
  labs(title = "Average Log Price by Manufacturer", x = "Manufacturer", y = "Average Log Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))  # Keeps text vertical



# Manufacturer Model vs logPrice
ggplot(trainingSet_clean, aes(x = Manufacturer_Model, y = LogPrice)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Manufacturer Model vs Log Price")

# Category vs logPrice
#ggplot(trainingSet, aes(x = Category, y = LogPrice, fill = Category)) + 
# geom_boxplot() + 
#ggtitle("Category vs Log Price") +
#scale_fill_brewer(palette = "Pastel1")

ggplot(trainingSet_clean, aes(x = Category, y = LogPrice)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("category vs Log Price")

# Leather Interior vs logPrice
ggplot(trainingSet_clean, aes(x = Leather.interior, y = LogPrice, fill = Leather.interior)) + 
  geom_boxplot() + 
  ggtitle("Leather Interior vs Log Price") +
  scale_fill_manual(values = c("Yes" = "cyan4", "No" = "darkgoldenrod2"))

# Fuel Type vs logPrice
ggplot(trainingSet_clean, aes(x = Fuel.type, y = LogPrice, fill = Fuel.type)) + 
  geom_boxplot() + 
  ggtitle("Fuel Type vs Log Price") +
  scale_fill_brewer(palette = "Dark2")

# Turbo vs logPrice
ggplot(trainingSet_clean, aes(x = Turbo, y = LogPrice, fill = Turbo)) + 
  geom_boxplot() + 
  ggtitle("Turbo vs Log Price") +
  scale_fill_manual(values = c("0" = "gray", "1" = "cornflowerblue"))

# Gear Box Type vs logPrice
ggplot(trainingSet_clean, aes(x = Gear.box.type, y = LogPrice, fill = Gear.box.type)) + 
  geom_boxplot() + 
  ggtitle("Gear Box Type vs Log Price") +
  scale_fill_brewer(palette = "Set1")

# Drive Wheels vs logPrice
ggplot(trainingSet_clean, aes(x = Drive.wheels, y = LogPrice, fill = Drive.wheels)) + 
  geom_boxplot() + 
  ggtitle("Drive Wheels vs Log Price") +
  scale_fill_brewer(palette = "Paired")

# Wheel (left/right-hand drive) vs logPrice
ggplot(trainingSet_clean, aes(x = Wheel, y = LogPrice, fill = Wheel)) + 
  geom_boxplot() + 
  ggtitle("Wheel (Drive Side) vs Log Price") +
  scale_fill_manual(values = c("Left wheel" = "orange", "Right-hand drive" = "purple"))

# Color vs logPrice
#ggplot(trainingSet, aes(x = Color, y = LogPrice, fill = Color)) + 
# geom_boxplot() + 
#theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#ggtitle("Color vs Log Price") +
#scale_fill_brewer(palette = "Spectral")

ggplot(trainingSet_clean, aes(x = Color, y = LogPrice)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("color vs Log Price")

# Doors vs logPrice
ggplot(trainingSet_clean, aes(x = factor(Doors), y = LogPrice, fill = factor(Doors))) + 
  geom_boxplot() + 
  ggtitle("Number of Doors vs Log Price") +
  scale_fill_brewer(palette = "Blues")

# Airbags vs logPrice
#ggplot(trainingSet, aes(x = factor(Airbags), y = LogPrice, fill = factor(Airbags))) + 
# geom_boxplot() + 
#ggtitle("Number of Airbags vs Log Price") +
#scale_fill_brewer(palette = "Reds")

ggplot(trainingSet_clean, aes(x = factor(Airbags), y = LogPrice)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Airbags vs Log Price")

#log price vs cylinders
ggplot(trainingSet_clean, aes(x = factor(Cylinders), y = LogPrice, fill = factor(Cylinders))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot with red outliers
  ggtitle("Log Price Distribution by Cylinders") +
  xlab("Cylinders") +
  ylab("Log Price") +
  scale_fill_manual(values = colorRampPalette(c("pink", "red"))(length(unique(trainingSet_clean$Cylinders)))) +
  theme_minimal()

###LogPrice with numerical predictors
ggplot(trainingSet_clean, aes(x = Engine.volume, y = LogPrice, color = factor(Turbo))) + 
  geom_point(alpha = 0.6) + 
  ggtitle("Engine Volume vs Log Price (Turbo vs Non-Turbo)") +
  xlab("Engine Volume") +
  ylab("Log Price") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("Non-Turbo", "Turbo")) +
  labs(color = "Turbo") +
  theme_minimal()


# Mileage vs logPrice
ggplot(trainingSet_clean, aes(x = `Mileage(km)`, y = LogPrice, color = LogPrice)) + 
  geom_point(alpha = 0.6) + 
  #geom_smooth(method = "lm", col = "blue") + 
  ggtitle("Mileage vs Log Price") +
  scale_color_gradient(low = "darkolivegreen4", high = "orange")

# Production Year vs logPrice
ggplot(trainingSet_clean, aes(x = Prod..year, y = LogPrice, color = LogPrice)) + 
  geom_point(alpha = 0.6) + 
  ggtitle("Production Year vs Log Price") +
  scale_color_gradient(low = "purple", high = "orange")


#Production Year vs Mileage
ggplot(trainingSet_clean, aes(x = `Prod..year`, y = `Mileage(km)`, color = `Mileage(km)`)) + 
  geom_point(alpha = 0.6) + 
  ggtitle("Production Year vs Mileage") +
  xlab("Production Year") +
  ylab("Mileage (km)") +
  scale_color_gradient(low = "blue", high = "red") + 
  theme_minimal()







###Multivariate analysis
#Pair Plot (Scatterplot Matrix)
#install.packages("GGally")
library(GGally)  
colnames(trainingSet_clean)
ggpairs(trainingSet_clean[, c("LogPrice", "Mileage(km)", "Engine.volume", "Cylinders")])


#Faceted Plots (Visualizing Multiple Categories)
ggplot(trainingSet_clean, aes(x = LogPrice, fill = Gear.box.type)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "dodge") +
  facet_wrap(~ Gear.box.type) +
  theme_minimal() +
  labs(title = "Log Price Distribution by Gear Box Type", x = "Log Price", y = "Count")

getwd()
file.exists("carPricePrediction.csv")

# Load necessary libraries
library(mdatools)

# Splitting the dataset (80% training, 20% testing)
set.seed(1)  # Set seed for reproducibility
idx <- sample(1: nrow(trainingSet_clean), 0.2 * nrow(trainingSet_clean))  # 20% test data

# Step 1: Identify numeric variables (excluding categorical ones)
numeric_vars <- sapply(trainingSet_clean, is.numeric)

# Step 2: Remove 'ID', 'Price', and 'LogPrice' explicitly
exclude_vars <- c("ID", "Price", "LogPrice")
numeric_vars <- names(numeric_vars[numeric_vars])  # Keep only numeric column names
numeric_vars <- setdiff(numeric_vars, exclude_vars)  # Remove unwanted variables

# Step 3: Preserve categorical variables (they won't be used in Xc and Xt)
categorical_vars <- names(trainingSet_clean)[!names(trainingSet_clean) %in% numeric_vars]

# Step 4: Define training data (only numeric predictors)
Xc <- as.matrix(trainingSet_clean[-idx, numeric_vars, drop = FALSE])  # Features
yc <- trainingSet_clean[-idx, "LogPrice", drop = FALSE]  # Target variable

# Step 5: Define testing data (only numeric predictors)
Xt <- as.matrix(trainingSet_clean[idx, numeric_vars, drop = FALSE])  # Test Features
yt <- trainingSet_clean[idx, "LogPrice", drop = FALSE]  # Test Target

# Step 6: Ensure dimensions match
print(dim(Xc))
print(dim(Xt))

# Debug: Print column names to confirm categorical variables are removed
print("Selected Numeric Variables:")
print(colnames(Xc))

print("Excluded Categorical Variables:")
print(categorical_vars)

# Fit PLS model
Model1 <- pls(Xc, yc, scale = TRUE, cv = 5, info = "Car Price Prediction Model")  # 5-Fold Cross-Validation

# Model summary
summary(Model1)

# Model evaluation
plot(Model1)  # Model diagnostics
plotXScores(Model1, show.labels = FALSE)  # X Scores visualization
plotXYLoadings(Model1, show.labels = TRUE)  # XY Loadings plot







