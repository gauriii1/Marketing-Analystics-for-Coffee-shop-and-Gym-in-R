library(readxl)
data <- read_excel("Projects/Marketing Analytics/Project files/gym.xlsx")
View(data)

# Display the data frame
head(data)

# Convert gender to numeric (Male = 1, Female = 0)
data$Gender <- ifelse(data$Gender == 'Male', 1, 0)

# Convert 'Workout_Type' from categorical to numeric
data$Workout_Type <- factor(data$Workout_Type, levels = c("Yoga", "HIIT", "Cardio", "Strength"))
data$Workout_Type <- as.numeric(data$Workout_Type)

# Display the data frame to confirm the conversion
head(data)

#scaling the data 
scaled_data = scale (data)

# Implementing Validation Techniques 
library(factoextra)
fviz_nbclust(scaled_data, kmeans, method = "wss")
fviz_nbclust(scaled_data, kmeans, method = "silhouette") 

# Implenenting K-means Clustering 
KMA_C = kmeans(scaled_data, centers = 3, nstart = 25)
print(KMA_C)

# Adding cluster results to original data
data$cluster = as.factor(KMA_C$cluster)
head(data)

# Demographics
library(dplyr)
demographics <- data %>%
  group_by(cluster) %>%
  summarise(
    Age = mean(Age),
    `Weight (kg)` = mean(`Weight (kg)`),
    `Height (m)` = mean(`Height (m)`),
    Max_BPM = mean(Max_BPM),
    Avg_BPM = mean(Avg_BPM),
    Resting_BPM = mean(Resting_BPM),
    `Session_Duration (hours)` = mean(`Session_Duration (hours)`),
    Calories_Burned = mean(Calories_Burned),
    Workout_Type = mean(Workout_Type),
    Fat_Percentage = mean(Fat_Percentage),
    `Water_Intake (liters)`= mean(`Water_Intake (liters)`)
  )
print(demographics)

# Understanding cluster composition
table(data$cluster)
aggregate(data$Age, by = list(cluster = data$cluster), mean)
aggregate(data$Resting_BPM, by = list(cluster = data$cluster), mean)
aggregate(data$Calories_Burned, by = list(cluster = data$cluster), mean)
aggregate(data$Fat_Percentage, by = list(cluster = data$cluster), mean)

# Visualizations 
library(factoextra)
fviz_cluster(KMA_C, data = scaled_data)

# Box plot
ggplot(data, aes(x = cluster, y = Fat_Percentage, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Fat Percentage by Cluster", x = "Cluster", y = "Fat Percentage") +
  theme_minimal() +
  scale_x_discrete(labels = c("1" = "Less Healthy", "2" = "Healthy", "3" = "Moderately Healthy")) # Custom labels

#Bar chart cluster analysis on Workout type 
# Ensure Workout_Type is treated as a factor with the correct labels
data$Workout_Type <- factor(data$Workout_Type, levels = c(1, 2, 3, 4), 
                            labels = c("Yoga", "HIIT", "Cardio", "Strength"))
ggplot(data, aes(x = Workout_Type, fill = cluster)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5) + # Adding count values
  labs(title = "Cluster Distribution Across Workout Types", x = "Workout Type", y = "Count") +
  theme_minimal()


# Marketing Strategies
# Load necessary library
library(ggplot2)

#Assigning health risk categories to clusters
data$Health_Risk <- factor(data$cluster, levels = c(1, 2, 3),
                           labels = c("Less Healthy", "Healthy", "Moderately Healthy"))

# Defining Marketing Strategies for each health risk category
marketing_strategies <- data.frame(
  Cluster = c("Less Healthy", "Healthy", "Moderately Healthy"),
  Strategy = c(
    "-Gentle Fitness Programs \n-Health and Wellness Education. \n-personalized Weight loss Plans.",
    "-Promote Premium Fitness Products \n-Healthy Recipe Plans & Meal Prep. \n-advanced workout gear.",
    "-Healthy Snacks & Supplements \n-Fitness Programs for Maintenance and Improvement."
  )
)

# Merging the strategies with the data
data_with_strategies <- merge(data, marketing_strategies, by.x = "Health_Risk", by.y = "Cluster", all.x = TRUE)

# Creating the bar plot with cluster distribution and marketing strategies
ggplot(data_with_strategies, aes(x = Health_Risk, fill = Health_Risk)) +
  geom_bar(stat = "count", show.legend = FALSE) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Display count above the bars
  labs(title = "Cluster Distribution with Marketing Strategy", 
       x = "Health Risk Cluster", y = "Number of Individuals") +
  theme_minimal() +
  facet_wrap(~Health_Risk, scales = "free", labeller = labeller(Health_Risk = c(
    "Less Healthy" = "-Gentle Fitness Programs \n -Health and Wellness Education.  \n -personalized Weight loss Plans.",
    "Healthy" = "-Promote Premium Fitness Products \n -Healthy Recipe Plans & Meal Prep. \n -advanced workout gear.",
    "Moderately Healthy" = "-Healthy Snacks & Supplements \n -Fitness Programs for Maintenance and Improvement."
  )))
