


library(dplyr) 
library(haven)


setwd("/Users/victoryikoabasi/Documents/Dissertation repository")

EWCS<- read_sav("ewcs6_2015_ukda_1904.sav")



MY_EWCS <- EWCS %>% select(Country, Q2a, Q2b, Q14, Q30a, Q30b, Q30c, Q30e, Q30g, Q30h, Q61a, Q61b, Q61c, Q61d, Q63a, Q63b, Q63c, Q63d, Q63e, Q63f, Q71c, Q73,
            Q74, Q75, Q78h, Q78i, Q79a, Q79b, Q79c, Q87a, Q87b, Q87c, Q87d, Q88, Q89e, Q90a, Q90b, Q90c)
                           




#data preparation
MY_EWCSS<- MY_EWCS %>% rename(gender = "Q2a", Age = "Q2b", Sector = "Q14", tiringpainfulpos = "Q30a", liftmovepeople = "Q30b",
                            carrymoveload = "Q30c", repetitivehandmoves = "Q30e", angryclients = "Q30g", traumaticexperiences = "Q30h", socialsupport = "Q61a",
                            Supportfrmmanager = "Q61b", consultedforworkobj = "Q61c", involvedinimprovements = "Q61d", leaderrespectsyou = "Q63a", leadergivesrecog ="Q63b", 
                            leadercollaboration = "Q63c", leaderhelps = "Q63d", leadergivesfeedback = "Q63e", leadersupportsdevelpmnt = "Q63f",  Mgtmeetings = "Q71c", Riskyhealth = "Q73", 
                            workeffectonhealth = "Q74", SRHS = "Q75", anxiety = "Q78h", fatigue = "Q78i", difficultysleeping = "Q79a", wakingrepeatedly = "Q79b", wakeexhausted = "Q79c",
                            Cheerful = "Q87a", calm = "Q87b", active = "Q87c", wakefreshnrested = "Q87d", jobsatisfaction = "Q88", motivatessuccess = "Q89e", vigour = "Q90a", dedication = "Q90b",
                            absorption = "Q90c") 


#variable cleaning
MY_EWCSS <- na.omit(MY_EWCSS)









#==========
#Western Europe as the focus of this research

# Define the numeric codes for Western European countries
western_european_codes <- c(1, 2, 10, 11, 14, 15, 18, 20, 22, 26, 28, 34)

# Define the labels for the numeric codes
country_labels <- c("Austria", "Belgium", "France", "Germany", "Ireland", "Italy", 
                    "Luxembourg", "Netherlands", "Portugal", "Spain", "Switzerland", 
                    "United Kingdom")


# Filter the dataset for Western European countries
MY_EWCSS <- MY_EWCSS %>% filter(Country %in% western_european_codes)

# Convert 'Country' to a factor with correct levels and labels
MY_EWCSS$Country <- factor(MY_EWCSS$Country, levels = western_european_codes, labels = country_labels)


# Inspect filtered dataset
print(nrow(MY_EWCSS))  # Print number of rows
head(MY_EWCSS)         # View first few rows

summary(MY_EWCSS)

table(MY_EWCSS$Country)

# Check throroghly for missing values
summary(MY_EWCSS)

total_missing <- sum(is.na(MY_EWCSS))
print(total_missing)

missing_per_column <- sapply(MY_EWCSS, function(x) sum(is.na(x)))
print(missing_per_column)

#==================
#Transformation and visualisation of variables 
library(ggplot2)

# Check the structure of dataframe
str(MY_EWCSS)

#gender and Age
#convert Age to categorical variable
class(MY_EWCSS$Age) # Age includes haven-labelled values changed to numerical-> create new value "agegroup" that includes corresponding character values

# Convert Age to numeric
MY_EWCSS$Age <- as.numeric(as.character(MY_EWCSS$Age))

# Check if conversion worked
class(MY_EWCSS$Age)

# Remove rows with Age values below 15
MY_EWCSS <- MY_EWCSS %>% filter(Age >= 15)

# Create age groups with Age starting from 15
MY_EWCSS <- MY_EWCSS %>%
  mutate(age_group = cut(Age,
                         breaks = c(14, 24, 34, 44, 54, 64, Inf),
                         labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                         right = FALSE))

# Verify the age group distribution
summary(MY_EWCSS$age_group)

#Check gender variable type
class(MY_EWCSS$gender)
# Convert gender to factor
MY_EWCSS$gender <- as.factor(MY_EWCSS$gender)


table(MY_EWCSS$age_group, MY_EWCSS$gender)

ggplot(MY_EWCSS, aes(x = age_group, y = age_group , fill= gender)) +
  geom_bar(position = "stack", stat = "identity")+
  theme(axis.title.y = element_blank())

ggsave("gender on age.png")

#Sector by country
# Convert Sector to factor
MY_EWCSS$Sector <- as_factor(MY_EWCSS$Sector)
table(MY_EWCSS$Sector)

table(MY_EWCSS$Country, MY_EWCSS$Sector)

ggplot(MY_EWCSS, aes(x = Country, fill = Sector)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sectors by Country", x = "Country", y = "Count") +
  scale_fill_discrete(name = "Sector") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#SRHS by age group and sector
class(MY_EWCSS$SRHS)
table(MY_EWCSS$SRHS)

# Convert SRHS to a factor variable with appropriate labels
MY_EWCSS$SRHS <- as_factor(MY_EWCSS$SRHS)

table(MY_EWCSS$SRHS, MY_EWCSS$age_group)

# Plot SRHS by age group
ggplot(MY_EWCSS, aes(x = age_group, fill = SRHS)) +
  geom_bar(position = "fill") +
  labs(title = "SRHS by Age Group",
       x = "Age Group",
       y = "Proportion",
       fill = "SRHS") +
  theme_minimal()

#SRHS by Country
table(MY_EWCSS$SRHS, MY_EWCSS$Country)

ggplot(MY_EWCSS, aes(y = Country, fill = SRHS)) +
  geom_bar(position = "fill") +
  labs(title = "SRHS by Country",
       x = "Proportion",
       y ="Country"  ,
       fill = "SRHS") +
  theme_minimal()




#=======
#check Cronbach's alpha and create composite score for SL, WE, SQ, JD, MH
library(psych)

# Perform PCA on items related to supportIVE Leadership
pca_SL <- prcomp(MY_EWCSS[, c("Supportfrmmanager", "leaderrespectsyou", "leadergivesrecog", "leadercollaboration", "leaderhelps", "leadergivesfeedback", 
                                  "leadersupportsdevelpmnt")], scale = TRUE)         

# Extract the first principal component as the new "LeaderSupport" variable
MY_EWCSS$supportiveleadership <- pca_SL$x[, 1]

# Check reliability of the items representing leader support

SupportiveLeadership <-MY_EWCSS[, c("Supportfrmmanager", "leaderrespectsyou", "leadergivesrecog", "leadercollaboration", "leaderhelps", "leadergivesfeedback", 
                                    "leadersupportsdevelpmnt")]

# Run the reliability analysis with check.keys=TRUE
alpha(SupportiveLeadership, check.keys = TRUE) #cronbach's alpha is 0.90

# create a composite variable
MY_EWCSS$supportive_leadership <- rowMeans(MY_EWCSS[, c("Supportfrmmanager", "leaderrespectsyou", "leadergivesrecog", "leadercollaboration", "leaderhelps", "leadergivesfeedback", 
                                                       "leadersupportsdevelpmnt")], na.rm = TRUE)

head(MY_EWCSS$supportive_leadership)

##############

#create a composite measure for psycological wellbeing (mental health)

#Reverse code negatively worded items anxiety and fatigue
#check variable type: should be numeric to reverse code
str(MY_EWCSS[, c("anxiety", "fatigue")])
summary(MY_EWCSS)

# Convert columns to numeric
MY_EWCSS <- MY_EWCSS %>%
  mutate(anxiety = as.numeric(as.character(anxiety)),
         fatigue = as.numeric(as.character(fatigue)))

#reverse
MY_EWCSS <- MY_EWCSS %>%
  mutate(
    anxiety = -anxiety,
    fatigue = -fatigue
  )
# Perform PCA on items related to Mental health
pca_MH <- prcomp(MY_EWCSS[, c("anxiety", "fatigue", "Cheerful", "calm", "active", "wakefreshnrested")], scale = TRUE)

# Extract the first principal component as the new mental health variable
MY_EWCSS$mentalhealth <- pca_MH$x[, 1]

# Check reliability of the items representing mental health with cronbach alpha
Mentalhealth <- MY_EWCSS[, c("anxiety", "fatigue", "Cheerful", "calm", "active", "wakefreshnrested")]

MH_Alpha <- alpha(Mentalhealth)

print(MH_Alpha)

# Rerun the reliability analysis with check.keys=TRUE
#alpha(Mentalhealth, check.keys = TRUE)

#create a composite variable
MY_EWCSS$mental_health <- rowMeans(MY_EWCSS[, c("anxiety", "fatigue", "Cheerful", "calm", "active", "wakefreshnrested")], na.rm = TRUE)

head(MY_EWCSS$mental_health)



summary(MY_EWCSS)
##############
#create a composite measure for job demands


# Perform PCA on items related to Job demands
pca_JD <- prcomp(MY_EWCSS[, c("tiringpainfulpos", "liftmovepeople", "carrymoveload", "repetitivehandmoves", "angryclients", 
                              "traumaticexperiences")], scale = TRUE)

# Extract the first principal component as the new General health variable
MY_EWCSS$jobdemand <- pca_JD$x[, 1]

# Check reliability of the items representing mental health with cronbach alpha
Jobdemand <- MY_EWCSS[, c("tiringpainfulpos", "liftmovepeople", "carrymoveload", "repetitivehandmoves", "angryclients", 
                          "traumaticexperiences")]
JD_alpha <- alpha(Jobdemand)

print(JD_alpha)

#create a composite variable
MY_EWCSS$job_demand <- rowMeans(MY_EWCSS[, c("tiringpainfulpos", "liftmovepeople", "carrymoveload", "repetitivehandmoves", "angryclients", "traumaticexperiences")],
                               na.rm = TRUE)

head(MY_EWCSS$job_demand)

# Visualise job demand by sector 
ggplot(MY_EWCSS, aes(x = Sector, y = job_demand, fill = Sector)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(title = "Composite Job demand Scores Across sectors",
       x = "Sector",
       y = "job_demand") +
  theme(legend.position = "none")

############
#create composite measure for employee engagement (AKA work engagement)

# Perform PCA on items related to work engagement
pca_WE <- prcomp(MY_EWCSS[, c("vigour", "dedication", "absorption")], scale = TRUE)

# Extract the first principal component as the new General health variable
MY_EWCSS$workengagement <- pca_WE$x[, 1]

# Check reliability of the items representing mental health with cronbach alpha
Workengagement <- MY_EWCSS[, c("vigour", "dedication", "absorption")]

WE_alpha <- alpha(Workengagement)

print(WE_alpha)

#create a composite variable
MY_EWCSS$work_engagement <- rowMeans(MY_EWCSS[, c("vigour", "dedication", "absorption")], na.rm = TRUE)

head(MY_EWCSS$work_engagement)

############

#create composite measure for sleep quality

# Perform PCA on items related to sleep quality
pca_SQ <- prcomp(MY_EWCSS[, c("difficultysleeping", "wakingrepeatedly", "wakeexhausted")], scale = TRUE)

# Extract the first principal component as the new pphysical health variable
MY_EWCSS$sleepquality <- pca_SQ$x[, 1]

# Check reliability of the items representing physical health with cronbach alpha
Sleepquality <- MY_EWCSS[, c("difficultysleeping", "wakingrepeatedly", "wakeexhausted")]

SQ_alpha <- alpha(Sleepquality)

print(SQ_alpha)

MY_EWCSS$sleep_quality <- rowMeans(MY_EWCSS[, c("difficultysleeping", "wakingrepeatedly", "wakeexhausted")], na.rm = TRUE)

head(MY_EWCSS$sleep_quality)

# Create the bar graph for composite sleep quality scores by age group
ggplot(MY_EWCSS, aes(x = age_group, y = sleep_quality, fill = age_group)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(title = "Sleep Quality Across Age Groups",
       x = "Age Group",
       y = "Sleep Quality") +
  theme(legend.position = "none")


############
#create composite measure for supportive environment
# Perform PCA on items related to supportive environment
#pca_SE <- prcomp(MY_EWCSS[, c("Supportfromcolleagues", "consultedforworkobj", "involvedinimprovements")], scale = TRUE)
# Extract the first principal component as the new pphysical health variable
#MY_EWCSS$supportiveenvironment <- pca_SE$x[, 1]
# Check reliability of the items representing physical health with cronbach alpha
#Supportiveenvironment <- MY_EWCSS[, c("Supportfromcolleagues", "consultedforworkobj", "involvedinimprovements")]
#SE_alpha <- alpha(Supportiveenvironment)
#print(SE_alpha)
#MY_EWCSS$supportiveenvironment <- rowMeans(MY_EWCSS[, c("Supportfromcolleagues", "consultedforworkobj", "involvedinimprovements")], 
                                  na.rm = TRUE
head(MY_EWCSS$supportiveenvironment)


##########







#============
# Calculate descriptive statistics for selected variables
descriptive_stats <- describe(MY_EWCSS[, c("gender", "Age", "Sector", "supportive_leadership", 
                                            "mental_health", "job_demand", "work_engagement", 
                                           "socialsupport", "SRHS", "sleep_quality")])

# Display descriptive statistics
print(descriptive_stats)



descriptivenew <- as.data.frame(descriptive_stats)
write_xlsx(descriptive_stats, "descriptivenew.xlsx")
str(MY_EWCSS)

write.xlsx(descriptive_stats, file = "descriptive_statistics.xlsx")


#============

#visualisation of Average scores of MH, SRHS, and WE by control variables
MY_EWCSS$SRHS <- as.numeric(as.factor(MY_EWCSS$SRHS))
# Mean of Outcome variables and work engagement by age
WE_by_age <- MY_EWCSS %>% group_by(age_group) %>% summarize(Average_WE = mean(work_engagement))
MH_by_age <- MY_EWCSS %>% group_by(age_group) %>% summarize(Average_MH = mean(mental_health))
SRHS_by_age <- MY_EWCSS %>% group_by(age_group) %>% summarize(Average_SRHS = mean(SRHS))
Mean_by_age <- rbind(cbind(setNames(WE_by_age, c("age_group", "Mean")), source = "Average_WE"),
                     cbind(setNames(MH_by_age, c("age_group", "Mean")), source = "Average_MH"),
                     cbind(setNames(SRHS_by_age, c("age_group", "Mean")), source = "Average_SRHS"))
write.csv(Mean_by_age, "Mean_by_age.csv")
ggplot(data = Mean_by_age) + 
  geom_col(mapping = aes(x= age_group, y = Mean, fill = source), position = position_dodge())

# Mean of Outcome variables and work engagement by gender
WE_by_gender <- MY_EWCSS %>% group_by(gender) %>% summarize(Average_WE = mean(work_engagement))
MH_by_gender <- MY_EWCSS %>% group_by(gender) %>% summarize(Average_MH = mean(mental_health))
SRHS_by_gender <- MY_EWCSS %>% group_by(gender) %>% summarize(Average_SRHS = mean(SRHS))
Mean_by_gender <- rbind(cbind(setNames(WE_by_gender, c("gender", "Mean")), source = "Average_WE"),
                     cbind(setNames(MH_by_gender, c("gender", "Mean")), source = "Average_MH"),
                     cbind(setNames(SRHS_by_gender, c("gender", "Mean")), source = "Average_SRHS"))
write.csv(Mean_by_gender, "Mean_by_age.csv")
ggplot(data = Mean_by_gender) + 
  geom_col(mapping = aes(x= gender, y = Mean, fill = source), position = position_dodge())

# Mean of Outcome variables and work engagement by sector
WE_by_sector <- MY_EWCSS %>% group_by(Sector) %>% summarize(Average_WE = mean(work_engagement))
MH_by_sector <- MY_EWCSS %>% group_by(Sector) %>% summarize(Average_MH = mean(mental_health))
SRHS_by_sector <- MY_EWCSS %>% group_by(Sector) %>% summarize(Average_SRHS = mean(SRHS))
Mean_by_sector <- rbind(cbind(setNames(WE_by_sector, c("Sector", "Mean")), source = "Average_WE"),
                        cbind(setNames(MH_by_sector, c("Sector", "Mean")), source = "Average_MH"),
                        cbind(setNames(SRHS_by_sector, c("Sector", "Mean")), source = "Average_SRHS"))
write.csv(Mean_by_sector, "Mean_by_sector.csv")
ggplot(data = Mean_by_sector) + 
  geom_col(mapping = aes(x= Mean, y = Sector, fill = source), position = position_dodge())




#===========
library(corrplot)
library(Hmisc)
# Calculate Pearson correlation matrix for the specified variables

# Select the columns for correlation analysis
data_for_corr <- MY_EWCSS %>%
  select(supportive_leadership, mental_health, work_engagement, socialsupport, SRHS)

# Compute the correlation matrix and p-values
correlation_results <- rcorr(as.matrix(data_for_corr))

# Extract correlation matrix
cor_matrix <- correlation_results$r

# Extract p-value matrix
p_matrix <- correlation_results$P

# Print the correlation matrix
print(cor_matrix)

# Print the p-value matrix
print(p_matrix)

# Create a correlation plot
corrplot(cor_matrix, method = "ellipse", type = "lower")

library(writexl)

corr_df <- as.data.frame(cor_matrix)
# Export to Excel
write_xlsx(corr_df, "output.xlsx")




#===========
#Multiple linear regression
library(dplyr)
library(psych)
library(car)
library(broom)
library(haven)

# Check the levels of Sector
levels(MY_EWCSS$Sector)

# regression results ommited private sector level so Convert Sector to character, trim whitespace, and convert back to factor
MY_EWCSS$Sector <- factor(trimws(as.character(MY_EWCSS$Sector)))

# Verify the levels
levels(MY_EWCSS$Sector)
#regression results also omitted one gendder level, so convert to numeric to showresults for all levels in gender
MY_EWCSS$gender <- as.numeric(as.factor(MY_EWCSS$gender))

# Regression of work engagemment on indpenedent and control variables
model1a <- lm(work_engagement ~ supportive_leadership + socialsupport + Age + gender + Sector + sleep_quality + job_demand
              , data = MY_EWCSS)
summary(model1a)
# Regression of mental health on indpenedent and control variables
model1b <- lm(mental_health ~ supportive_leadership + socialsupport + work_engagement + Age + gender + Sector + sleep_quality + job_demand
               , data = MY_EWCSS)
summary(model1b)

# Regression of self-rated health on indpenedent and control variables
model1c <- lm(SRHS ~ supportive_leadership + socialsupport + work_engagement + Age + gender + Sector + sleep_quality + job_demand
             , data = MY_EWCSS)
summary(model1c)


# Mediation analysis: 
#bootstrapping technique
#work engagement as mediator for mental health and supportive leadership
library(mediation)
library(dplyr)
require(flexplot)

# Fit the mediator model
model_m <- lm(work_engagement ~ supportive_leadership + socialsupport + Age + gender + Sector + sleep_quality + job_demand, data = MY_EWCSS)

# Fit the outcome model(MH)
model_y <- lm(mental_health ~ supportive_leadership + socialsupport + work_engagement + Age + gender + Sector + sleep_quality + job_demand, 
              data = MY_EWCSS)

set.seed(2022) #for reproducibilty
# Run the mediation analysis USING BOOTSTRAPPING
mediation_model1 <- mediate(
  model.m = model_m,
  model.y = model_y,
  treat = "supportive_leadership",
  mediator = "work_engagement",
  boot = TRUE,
  sims = 5000
)

# Summarize the results
summary(mediation_model1)

# Run the mediation analysis
mediation_model_ss <- mediate(
  model.m = model_m,
  model.y = model_y,
  treat = "socialsupport",
  mediator = "work_engagement",
  boot = TRUE,
  sims = 5000
)

# Summarize the results
summary(mediation_model_ss)



#work engagement as mediator for Self-rated health and Independent variables
# Fit the outcome model
model_SRHS <- lm(SRHS ~ supportive_leadership + socialsupport + work_engagement + Age + gender + Sector + sleep_quality + job_demand, 
              data = MY_EWCSS)

# Run the mediation analysis
mediation_model_SRHS1 <- mediate(
  model.m = model_m,
  model.y = model_SRHS,
  treat = "supportive_leadership",
  mediator = "work_engagement",
  boot = TRUE,
  sims = 5000
)
# Summarize the results
summary(mediation_model_SRHS1)

mediation_model_SRHS2 <- mediate(
  model.m = model_m,
  model.y = model_SRHS,
  treat = "socialsupport",
  mediator = "work_engagement",
  boot = TRUE,
  sims = 5000
)

# Summarize the results
summary(mediation_model_SRHS2)

# Plot the mediation model for supportive leadership and MH
plot(mediation_model1)

# Plot the mediation model for social support And MH
plot(mediation_model_ss)

# Plot the mediation model for SRHS with supportive leadership
plot(mediation_model_SRHS1)

# Plot the mediation model for SRHS with social support
plot(mediation_model_SRHS2)








