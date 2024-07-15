# Set the working directory and verify it
setwd('D:\\SCMA\\Data')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue","sf")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68 (1).csv")


# a)Plotting a histogram and a barplot of the data to indicate the consumption district-wise for  AP

# Filtering for AP
df <- data %>%
  filter(state_1 == "AP")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Sub-setting the data
apnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(apnew)))

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
apnew$Meals_At_Home <- impute_with_mean(apnew$Meals_At_Home)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(apnew)))

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  apnew <- remove_outliers(apnew, col)
}

# Summarize consumption
apnew$total_consumption <- rowSums(apnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- apnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors , get codes from appendix of NSSO 68th ROund Data
district_mapping <- c("15" = "Pashim Midnapur", "11" = "North Twenty Four Parganas", "12" = " Hugli")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

apnew$District <- as.character(apnew$District)
apnew$Sector <- as.character(apnew$Sector)
apnew$District <- ifelse(apnew$District %in% names(district_mapping), district_mapping[apnew$District], apnew$District)
apnew$Sector <- ifelse(apnew$Sector %in% names(sector_mapping), sector_mapping[apnew$Sector], apnew$Sector)
View(apnew)

# ap_consumption stores the aggregate of the consumption district wise
ap_consumption <- aggregate(total_consumption ~ District, data = apnew, sum) 
View(ap_consumption)

# histogram to show the distribution of total consumption across different districts
hist(apnew$total_consumption, breaks = 15, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Andhra Pradesh State")

# barplot to visualize consumption per district with district names
??barplot
barplot(ap_consumption$total_consumption, 
        names.arg = ap_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) 


# b) Plotting total consumption on the Karnataka state map 


# Filtering for Karnataka
df_ka <- data %>%
  filter(state_1 == "KA")

# Sub-setting the data
ka_new <- df_ka %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(ka_new)))

# Impute missing values with mean for specific columns
ka_new$Meals_At_Home <- impute_with_mean(ka_new$Meals_At_Home)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(ka_new)))

# Finding outliers and removing them
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  ka_new <- remove_outliers(ka_new, col)
}

# Summarize consumption
ka_new$total_consumption <- rowSums(ka_new[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

district_summary <- summarize_consumption("District")
cat("District Consumption Summary:\n")
print(district_summary)

# mapping districts so that meging of the tables will be easier
district_mapping <- c(
  "1" = "Belagavi",
  "2" = "Bagalkote",
  "3" = "Vijayapura", 
  "4" = "Kalaburagi", 
  "5" = "Bidar",
  "6" = "Raichur",
  "7" = "Koppal",
  "8" = "Gadag",
  "9" = "Dharwad",
  "10" = "Uttara Kannada",
  "11" = "Haveri",
  "12" = "Ballari",
  "13" = "Chitradurga",
  "14" = "Davanagere",
  "15" = "Shivamogga",
  "16" = "Udupi",
  "17" = "Chikkamagaluru",
  "18" = "Tumakuru",
  "19" = "Kolar",
  "20" = "Bangalore",
  "21" = "Bengaluru Rural",
  "22" = "Mandya",
  "23" = "Hassan",
  "24" = "Dakshina Kannada",
  "25" = "Kodagu",
  "26" = "Mysuru",
  "27" = "Chamarajanagara",
  "28" = "Ramanagara",
  "29" = "Chikkaballapura"
)

ka_new$District <- as.character(ka_new$District)
ka_new$District <- district_mapping[ka_new$District]
#ka_new$District <- ifelse(ka_new$District %in% names(district_mapping), district_mapping[ka_new$District], ka_new$District)
View(ka_new)

# ka_consumption stores aggregate of total consumption district wise
ka_consumption <- aggregate(total_consumption ~ District, data = ka_new, sum) 
View(ka_consumption)

#Plotting total consumption on the Karnataka state 

Sys.setenv("SHkaE_RESTORE_SHX" = "YES") 

data_map <- st_read("D:\\SCMA\\Data\\KARNATAKA_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 

# merging ka_consumption and data_map tables
data_map_data <- merge(ka_consumption,data_map,by = "District") 
View(data_map_data)

# Plot without labeling district names
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") 

# Plot with labelled district names
ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")

