##Analysis of thermal tolerance assays - will need to edit after COI to include only Tapinoma

# Load necessary libraries
install.packages("car")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggsignif")
install.packages("googlesheets4")
library(ggplot2)
library(dplyr)
library(car)
library(ggsignif)
library(googlesheets4)

##upload meta ant collection data from google sheets as a csv file. The google sheet is publicly accessable (anyone with the link can view) 
meta_collection_data <- read.csv("https://docs.google.com/spreadsheets/d/1Ot1DKFNfEkkzb8YNEsjGYfNryo53a85Nukh4xA-BMcI/gviz/tq?tqx=out:csv")

rm(collection_data)

#Convert City and Neighborhood Grade to factors
meta_collection_data$City <- as.factor(meta_collection_data$City)
meta_collection_data$Neighborhood.Grade <- as.factor(meta_collection_data$Neighborhood.Grade)

#Total number of Tapinoma colonies collected by city 
meta_collection_data %>% 
  filter(Conf.Genus == "Tapinoma") %>% 
  count(City)
#Filter data to only include rows where avg_CToc (Col L) is not NA for both cities 
AvgCT_meta_collection_data <- meta_collection_data %>% filter(!is.na(Avg.CT))

# Check if filtering worked
head(AvgCT_meta_collection_data)
# Check how many non-NA avg.CToC values there are
meta_collection_data %>% 
  filter(!is.na(Avg.CT)) %>% 
  count()

#total count for each city
meta_collection_data %>% 
  filter(!is.na(Avg.CT)) %>% 
  count(City)

##total count for each city by Tapinoma that were assayed 
meta_collection_data %>% 
  filter(!is.na(Avg.CT), Conf.Genus == "Tapinoma") %>% 
  count(City)

#Philly only data 
Philly_AvgCT_collection_data <- meta_collection_data %>%
  filter(!is.na(Avg.CT), City == "Philadelphia")

#Baltimore only data
Baltimore_AvgCT_collection_data <- meta_collection_data %>%
  filter(!is.na(Avg.CT), City == "Baltimore")

##LINEAR REGRESSION FOR BOTH CITIES COMBINED

# Perform linear regression: avg_CToC ~ Neighborhood_Grade
meta_city_model <- lm(Avg.CT ~ Neighborhood.Grade, data = AvgCT_meta_collection_data)

# Display model summary
print(summary(meta_city_model))

#Visualize linear regression with a scatter plot
ggplot(AvgCT_meta_collection_data, aes(x = Neighborhood.Grade, y = Avg.CT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")


##LINEAR REGRESSION FOR PHILADELPHIA
philly_city_model <- lm(Avg.CT ~ Neighborhood.Grade, data = Philly_AvgCT_collection_data)

# Display model summary
print(summary(philly_city_model))

#Visualize linear regression with a scatter plot
ggplot(Philly_AvgCT_collection_data, aes(x = Neighborhood.Grade, y = Avg.CT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")


##LINEAR REGRESSION FOR BALTIMORE
# Perform linear regression: avg_CToC ~ Neighborhood_Grade
baltimore_city_model <- lm(Avg.CT ~ Neighborhood.Grade, data = Baltimore_AvgCT_collection_data)

# Display model summary
print(summary(baltimore_city_model))

#Visualize linear regression with a scatter plot
ggplot(Baltimore_AvgCT_collection_data, aes(x = Neighborhood.Grade, y = Avg.CT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")


##ANOVA and Turkey's HSD for Both cities

#ANOVA for both cities comvined 
meta_anova_res <- anova(meta_city_model)
print(meta_anova_res)

#There was significant difference found between the neighborhood grades. Running a Turkey Post Hoc Test
# If ANOVA is significant, perform Tukey's HSD
if (meta_anova_res$`Pr(>F)`[1] < 0.05) {
  print(TukeyHSD(aov(meta_city_model)))
}

##ANOVA for philadelphia
philly_anova_res <- anova(philly_city_model)
print(philly_anova_res)

# If ANOVA is significant, perform Tukey's HSD
if (philly_anova_res$`Pr(>F)`[1] < 0.05) {
  print(TukeyHSD(aov(philly_city_model)))
}

##ANOVA for Baltimore 
baltimore_anova_res <- anova(baltimore_city_model)
print(baltimore_anova_res)
