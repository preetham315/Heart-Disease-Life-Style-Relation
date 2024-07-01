
# Load the required libraries
library(tidyverse)
library(magrittr)
library(ggplot2)
library(viridis)
library(scales)


# Read in the dataset
heart_data <- read.csv("heart_2020_cleaned.csv")

# Explore the structure and summary of the dataset
str(heart_data)
summary(heart_data)

sum(is.na(heart_data))


#The density plot shows the estimated probability density function of the BMI 
#values in the dataset. The height of the curve at a given point represents the
#estimated probability that a randomly selected BMI value from the dataset will 
#fall within a small range of values around that point.

#In the context of the plot you mentioned, the density on the right-hand side 
#of the plot indicates that BMI values in the dataset are more concentrated in 
#that range, meaning there are more individuals with BMI values in that range 
#than in other ranges. Conversely, the lower density on the left-hand side of 
#the plot indicates that BMI values in that range are less common in the dataset.

#Overall, the density plot provides a more continuous and detailed 
#representation of the distribution of BMI values in the dataset than the 
#histogram, which is based on discrete bins.


# BMI Distribution
heart_data %>% 
  ggplot(aes(x = BMI, y = ..density..)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "white", fill = "#ADD8E6") +
  geom_density(color = "#0066CC") +
  labs(title = "Distribution of BMI", x = "BMI", y = "Density") +
  theme_minimal()


# Distribution of Age in the data set
heart_data %>% 
  count(AgeCategory) %>%
  ggplot(aes(x = AgeCategory, y = n, fill = AgeCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Age Categories", x = "Age Category", y = "Count") +
  theme_minimal()

#Sleep Distribution Age-wise
heart_data %>%
  ggplot(aes(x = SleepTime, fill = AgeCategory)) +
  geom_histogram(binwidth = 0.9, position = "identity", alpha = .7) +
  scale_fill_viridis_d() +
  labs(x = "Sleep Time (hrs)", y = "Frequency", fill = "Age Category") +
  theme_classic()

# Ditribution of female and male across different parameters

heart_data %>%
  pivot_longer(cols = c(Smoking, AlcoholDrinking, Stroke, KidneyDisease, SkinCancer)) %>%
  group_by(name, Sex, value) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = value, y = n, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#99C7D9", "#C2D69B", "#FAC090", "#FF9E9D", "#D3C0D2")) +
  facet_wrap(~ Sex, ncol = 2, scales = "free_x") +
  labs(x = "", y = "Count", fill = "") +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10), axis.title = element_text(size = 12),
        legend.position = "bottom", legend.title = element_blank())




# Visualize the distribution of continuous variables using histograms
heart_data %>% 
  select(BMI, DiffWalking, GenHealth, MentalHealth, PhysicalHealth, SleepTime) %>% 
  gather(key = "variable", value = "value") %>% 
  mutate(value = as.numeric(value)) %>% # Convert the value variable to numeric
  ggplot(aes(x = value, y = ..density..)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density()



# Visualize the relationship between continuous variables using scatterplots
heart_data %>% 
  select(AgeCategory, BMI, DiffWalking, GenHealth, MentalHealth, PhysicalHealth, SleepTime, HeartDisease) %>% 
  ggplot(aes(x = AgeCategory, y = BMI, color = HeartDisease)) +
  geom_point() +
  labs(title = "Scatterplot of age category and BMI by heart disease", 
       x = "Age category", y = "BMI", color = "Heart disease")


# Perform t-tests to compare the means of continuous variables by the presence of heart disease

#The p-value is less than 2.2e-16, which indicates that there is strong evidence
#to reject the null hypothesis that the means of BMI in individuals with and
#without heart disease are equal.
t.test(heart_data$BMI ~ heart_data$HeartDisease)

#we can reject the null hypothesis which states that there is no significant 
#difference in the mean MentalHealth score between the groups with and without heart disease.
t.test(heart_data$MentalHealth ~ heart_data$HeartDisease)

#we can reject the null hypothesis which states that there is no significant 
#difference in the mean PhysicalHealth score between the groups with and without heart disease.
t.test(heart_data$PhysicalHealth ~ heart_data$HeartDisease)

#we can reject the null hypothesis which states that there is no significant 
#difference in the mean Sleeptime score between the groups with and without heart disease.
t.test(heart_data$SleepTime ~ heart_data$HeartDisease)

# Perform chi-squared tests to compare the proportions of categorical variables 
#by the presence of heart disease

#very small p-value (less than 2.2e-16). This indicates strong evidence against 
# the null hypothesis, and suggests that there is a significant association between 
#smoking and heart disease in the sample.

#likewise for the rest of the variables as well

chisq.test(heart_data$Smoking, heart_data$HeartDisease)
chisq.test(heart_data$AlcoholDrinking, heart_data$HeartDisease)
chisq.test(heart_data$Stroke, heart_data$HeartDisease)
chisq.test(heart_data$Sex, heart_data$HeartDisease)
chisq.test(heart_data$Race, heart_data$HeartDisease)
chisq.test(heart_data$Diabetic, heart_data$HeartDisease)
chisq.test(heart_data$PhysicalActivity, heart_data$HeartDisease)
chisq.test(heart_data$Asthma, heart_data$HeartDisease)
chisq.test(heart_data$KidneyDisease, heart_data$HeartDisease)
chisq.test(heart_data$SkinCancer, heart_data$HeartDisease)

# Fit a logistic regression model to identify significant predictors of heart disease
# Converting the hear disease column to 0 and 1 
heart_data$HeartDisease <- ifelse(heart_data$HeartDisease == "No", 0, 1)

heart_model <- glm(HeartDisease ~ ., data = heart_data, family = binomial)

#Based on this output, BMI, Smoking(Yes), AlcoholDrinking(Yes), Stroke(Yes), 
#PhysicalHealth, MentalHealth, DiffWalking(Yes), Sex(Male), AgeCategory, 
#Diabetic(Yes), GenHealthFair, GenHealthGood, GenHealthPoor, GenHealthVeryGood, 
#SleepTime, and Asthma(Yes) are all statistically significant predictors of heart disease.


#AgeCategory25-29, DiabeticYes (during pregnancy), PhysicalActivityYes, RaceOther,
#and RaceWhite are not statistically significant predictors of heart disease.

#The z-score is used to assess the statistical significance of each variable in
#the model. The larger the absolute value of the z-score, the further the estimate
#of the coefficient is from zero, indicating a greater impact on the outcome variable

summary(heart_model)



################################################################################

# Heart_Data:Health_Metrics

heart_health <- read.csv("heart.csv")

str(heart_health)
summary(heart_health)

sum(is.na(heart_health))


ggplot(heart_health, aes(x = Age)) + 
  geom_histogram(bins = 20, fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Age")

ggplot(heart_health, aes(x = RestingBP)) + 
  geom_histogram(bins = 20, fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Resting Blood Pressure")

ggplot(heart_health, aes(x = Cholesterol)) + 
  geom_histogram(bins = 20, fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Serum Cholesterol")

ggplot(heart_health, aes(x = MaxHR)) + 
  geom_histogram(bins = 20, fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Maximum Heart Rate Achieved")

ggplot(heart_health, aes(x = Oldpeak)) + 
  geom_histogram(bins = 20, fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Oldpeak")

# Visualize the proportion of categorical variables
ggplot(heart_health, aes(x = Sex, fill = Sex)) +
  geom_bar(position = "dodge", width = 0.45) +
  scale_fill_manual(values = c("skyblue", "pink")) +
  labs(title = "Distribution of Sex")


ggplot(heart_health, aes(x = ChestPainType, fill = ChestPainType)) +
  geom_bar(position = "dodge", width = 0.45) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Chest Pain Type")

ggplot(heart_health, aes(x = FastingBS, fill = FastingBS)) +
  geom_bar(position = "dodge", width = 0.45) +
  scale_fill_manual(values = c("skyblue", "pink")) +
  labs(title = "Distribution of Fasting Blood Sugar")

ggplot(heart_health, aes(x = RestingECG, fill = RestingECG)) +
  geom_bar(position = "dodge", width = 0.45) +
  scale_fill_brewer(palette = "YlOrBr") +
  labs(title = "Distribution of Resting Electrocardiogram Results")

ggplot(heart_health, aes(x = ExerciseAngina, fill = ExerciseAngina)) +
  geom_bar(position = "dodge", width = 0.45) +
  scale_fill_manual(values = c("skyblue", "pink")) +
  labs(title = "Distribution of Exercise Induced Angina")

ggplot(heart_health, aes(x = ST_Slope, fill = ST_Slope)) +
  geom_bar(position = "dodge", width = 0.45) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Distribution of ST Slope")

# performing some statistical tests
#The 95% confidence interval suggests that the true mean age of individuals 
#without heart disease is between 4.16 and 6.54 years younger than the mean age 
#of individuals with heart disease.
t.test(Age ~ HeartDisease, data = heart_health)

#Can reject the null and state there is a significant different between 
# the values for patients with heart disease and without heart disease
t.test(RestingBP ~ HeartDisease, data = heart_health)

# even in this we can reject the null hypothesis as the p value is significantly low
t.test(Cholesterol ~ HeartDisease, data = heart_health)

# Can reject the null as there is a significant diference in the means for the
# patients with heart disease and with out heart disease
t.test(MaxHR ~ HeartDisease, data = heart_health)


heart_model_1 <- glm(HeartDisease ~ ., data = heart_health, family = binomial)

#Sex, ChestPainTypeATA, ChestPainTypeNAP, ChestPainTypeTA, Cholesterol, FastingBS,
#ExerciseAnginaY, Oldpeak, and ST_SlopeFlat these variables are statistically 
#significant in predicting the likelihood of having heart disease
summary(heart_model_1)



# 
# ## Plot one
# heart_data %>%
#   select(AgeCategory, BMI, DiffWalking, GenHealth, MentalHealth, PhysicalHealth, SleepTime, HeartDisease) %>%
#   mutate(HeartDisease = factor(HeartDisease)) %>%
#   ggplot(aes(x = AgeCategory, y = BMI, color = HeartDisease)) +
#   geom_point() +
#   scale_color_manual(values = c("#800001", "#4B4B4C")) +
#   labs(title = bquote(atop("BMI vs Age Category by Heart Disease", atop(bold(""), ""))),
#        x = "Age category", y = "BMI", color = "Heart disease") +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(size = .5, color = "black"))
# 
# 
# 
# ## plot 2
# heart_data %>%
#   group_by(Smoking, HeartDisease) %>%
#   summarise(count = n()) %>%
#   group_by(Smoking) %>%
#   mutate(percent = count / sum(count) * 100,
#          HD_Yes_percent = percent[HeartDisease == 1]) %>%
#   ggplot(aes(x = Smoking, y = percent, fill = as.factor(HeartDisease))) +
#   geom_col(position = "dodge", width = 0.5) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), breaks = seq(0, 100, 10)) +
#   geom_text(aes(label = paste0(round(percent), "%")), 
#             position = position_dodge(width = 0.5), size = 4, vjust = -.5,hjust= 0.3) +
#   labs(title = " Impact of Smoking on Heart Disease", 
#        x = "Smoking status", y = "Percentage",
#        fill = "Heart disease status") +
#   scale_fill_manual(values = c("#800001", "#4B4B4C"),
#                     labels = c("No", "Yes"),
#                     name = "Heart disease")
# 
# 
# # Visualization 3: Boxplot of Physical Health by Heart Disease
# 
# 
# heart_data %>%
#   group_by(HeartDisease) %>%
#   summarise(mean_PhysicalHealth = mean(PhysicalHealth)) %>%
#   ggplot(aes(x = as.factor(HeartDisease), y = mean_PhysicalHealth, fill = as.factor(HeartDisease))) +
#   geom_col() +
#   scale_fill_manual(values = c("#800001", "#4B4B4C"),
#                     labels = c("No", "Yes"),
#                     name = "Heart disease") +
#   labs(title = "Impact of Health Scores on heart disease", 
#        x = "Heart disease status", y = "Mean physical health score") 
# 
# 
# # Plot 4
# heart_data %>%
#   mutate(HeartDisease = as.factor(HeartDisease)) %>%
#   group_by(Sex, HeartDisease) %>%
#   summarise(count = n()) %>%
#   mutate(percent = count/sum(count) * 100) %>%
#   ggplot(aes(x = Sex, y = percent, fill = HeartDisease)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_text(aes(label = scales::percent(percent/100), 
#                 y = percent+2),
#             position = position_dodge(width = 0.9),
#             size = 3) +
#   ggtitle("Heart Disease by Gender") +
#   scale_fill_manual(values = c("#800001", "#4B4B4C"), 
#                     labels = c("No", "Yes"),
#                     name = "Heart Disease") +
#   labs(x = "Gender", y = "Percentage") +
#   scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100))
# 
# 
# #plot 5
# # library(gridExtra)
# # library(ggpubr)
# # 
# # # Subset the data for males and females
# # heart_health_male <- filter(heart_health, Sex == "M")
# # heart_health_female <- filter(heart_health, Sex == "F")
# # 
# # # Create a nested pie chart function
# # nested_pie <- function(data, title){
# #   ggpie(data = select(data, count, HeartDisease), values = "count", label = "HeartDisease", 
# #         ggtheme = theme_void(), fill = "HeartDisease",
# #         title = title, label_font = 12)
# # }
# # 
# # # Create the pie charts for males and females
# # ggarrange(
# #   ggplot(heart_health_male, aes(x = "", fill = ChestPainType)) +
# #     geom_bar(width = 1) +
# #     facet_wrap(~Sex) +
# #     coord_polar(theta = "y") +
# #     theme_void() +
# #     ggtitle("Chest Pain Type by Sex") +
# #     theme(legend.position = "bottom") +
# #     labs(fill = "Chest Pain Type"),
# #   
# #   nested_pie(data = heart_health_male %>% 
# #                group_by(Sex, ChestPainType, HeartDisease) %>% 
# #                summarise(count = n()),
# #              title = "Heart Disease Status"),
# #   
# #   ggplot(heart_health_female, aes(x = "", fill = ChestPainType)) +
# #     geom_bar(width = 1) +
# #     facet_wrap(~Sex) +
# #     coord_polar(theta = "y") +
# #     theme_void() +
# #     ggtitle("Chest Pain Type by Sex") +
# #     theme(legend.position = "bottom") +
# #     labs(fill = "Chest Pain Type"),
# #   
# #   nested_pie(data = heart_health_female %>% 
# #                group_by(Sex, ChestPainType, HeartDisease) %>% 
# #                summarise(count = n()),
# #              title = "Heart Disease Status"),
# #   
# #   nrow = 2, ncol = 2
# # )
# 
# #plot 5
# # Subset the data for males and females
# heart_health_male <- subset(heart_health, Sex == "M")
# heart_health_female <- subset(heart_health, Sex == "F")
# 
# # Define the color palette for heart disease status
# color_palette <- c("#800001", "#4B4B4C")
# 
# # Create a grouped bar chart for males
# ggplot(data = heart_health_male, aes(x = ChestPainType, fill = factor(HeartDisease))) +
#   geom_bar(position = position_dodge(), width = .8) +
#   scale_fill_manual(values = color_palette, labels = c("No", "Yes"),
#                     name = "Heart disease") +
#   labs(x = "Chest Pain Type", y = "Count", title = "Heart Disease by Chest Pain Type-Males") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(size = .5, color = "black")) +
#   geom_text(aes(label=..count..), stat='count', position=position_dodge(width=0.8), vjust=-0.5,hjust=0.7, size=4)
# 
# # Create a grouped bar chart for females
# ggplot(data = heart_health_female, aes(x = ChestPainType, fill = factor(HeartDisease))) +
#   geom_bar(position = position_dodge(), width = 0.8) +
#   scale_fill_manual(values = color_palette, labels = c("No", "Yes"),
#                     name = "Heart disease") +
#   labs(x = "Chest Pain Type", y = "Count", title = "Heart Disease by Chest Pain Type-Females") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(size = 0.5, color = "black")) +
#   geom_text(aes(label=..count..), stat='count', position=position_dodge(width=0.8), vjust=-0.5,hjust=0.7, size=4)
# 









