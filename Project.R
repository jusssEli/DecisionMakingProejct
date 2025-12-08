install.packages("RKaggle")  #Library where our dataset is located
install.packages("ggplot2")  #Library where our dataset is located
library(RKaggle)
library(ggplot2)
dataset = get_dataset("mohithsairamreddy/salary-data")  #retrieving/downloading our specific dataset we wanted to work with

# Due to different spellings of PhD in the db, reformat all fields (probably not necessary for all but better safe than sorry. Update: it was very necessary)
dataset$`Education Level` = tolower(dataset$`Education Level`)  # Make all lowercase
# Fix names (e.g replace master's degree w/ Master's)
dataset$`Education Level` = sub("phd", "PhD", dataset$`Education Level`)
dataset$`Education Level` = sub("master's", "Master's", dataset$`Education Level`)
dataset$`Education Level` = sub("Master's degree", "Master's", dataset$`Education Level`)
dataset$`Education Level` = sub("Bachelor's degree", "Bachelor's", dataset$`Education Level`)
dataset$`Education Level` = sub("bachelor's", "Bachelor's", dataset$`Education Level`)
dataset$`Education Level` = sub("high school", "High School", dataset$`Education Level`)

# 1. How does the average salary differ between men and women?
# Boxplot and Bar Chart
# Why?
# Boxplot: Shows median, spread, and any differences clearly between male and female salaries.
boxplot(Salary ~ Gender, data = dataset, main = "Salary Distribution by Gender", xlab = "Gender", ylab = "Salary")

# Bar Chart: Simple comparison of average salaries.
# aggregate to group data and apply a funciton to said data. Groups Salary based on (~) Gender
avg_gender_salary = aggregate(Salary ~ Gender, data = dataset, FUN = mean) # *What is fun? Here's an explanation https://www.youtube.com/watch?v=HG3UwIDAEb8
# names.arg uses the tables column values (Male, Female, Master's, PhD etc.). The max value is * 1.2 for whitespace at the top of graphs (not necessary, but stylish (: )
barplot(avg_gender_salary$Salary, names.arg = avg_gender_salary$Gender, main = "Average Salary by Gender", xlab = "Gender", ylab = "Average Salary", ylim = c(0, max(avg_gender_salary$Salary) * 1.2))


# 2. How do age and years of experience affect salary?
#Experience groups separated by 5 years
# cut is a fun and cool way to break the data into bins (groups)
dataset$Experience_Group = cut(dataset$`Years of Experience`, breaks = c(0, 5, 10, 15, 20, 30), labels = c("0-5", "6-10", "11-15", "16-20", "21+"))

# Histogram, Boxplot and Bar Chart
# Why?
# Boxplot: Shows how salary changes with increasing experience
boxplot(Salary ~ Experience_Group, data = dataset, main = "Salary by Experience Level", xlab = "Experience Group (Years)", ylab = "Salary")

# Bar Chart: Compares average salaries for each experience bracket.
avg_experience_salary = aggregate(Salary ~ Experience_Group, data = dataset, FUN = mean);
barplot(avg_experience_salary$Salary, names.arg = avg_experience_salary$Experience_Group, main = "Average Salary by Experience Level", xlab = "Experience Group", ylab = "Average Salary", ylim = c(0, max(avg_experience_salary$Salary) * 1.2))

# Histogram: Shows the distribution of ages within the dataset
hist(dataset$Age, main = "Age Distribution", xlab = "Age", ylab = "Frequency") # Frequency is the amount of times the age appears within a 5 year range. 25-30 is the highest. Heavily right skewed


# 3. How does education level affect salary and job fields?
# Salary vs Education Bar Chart, Frequency of Job Fields by Education Level Bar Chart, Boxplot
# Why? 
# Salary vs Education Bar Chart: Effective for comparing degrees
avg_education_salary = aggregate(Salary ~ `Education Level`, data = dataset, FUN = mean)
barplot(avg_education_salary$Salary, names.arg = avg_education_salary$`Education Level`,  main = "Average Salary by Education Level", xlab = "Education Level", ylab = "Average Salary", ylim = c(0, max(avg_education_salary$Salary) * 1.2))

# Frequency of Job Fields by Education Level Bar Chart: Shows which fields people with different degrees work in.
# Basically returns the Header and how many times it appears in the dataset
education_counts = table(dataset$`Education Level`) # used in barplot to get labels and heights
barplot(education_counts, main = "Number of People by Education Level", xlab = "Education Level", ylab = "Number of People")

# Boxplot: Shows salary distribution across education levels.
boxplot(Salary ~ `Education Level`, data = dataset,  main = "Salary Distribution by Education Level", xlab = "Education Level", ylab = "Salary")

# FUN is actually short for function and FUN = mean means to apply the mean() function on the specified data and use it for the calculation. 


# Linear Regression Between YOE and Salary
plot(Salary ~ `Years of Experience`, data = dataset,  main = "Salary vs Years of Experience", xlab = "Years of Experience", ylab = "Salary")
model_salary = lm(Salary ~ `Years of Experience`, data = dataset)
model_salary
abline(model_salary, col = "red")
summary(model_salary)

# Scatter Plot
# aes = aesthetics, geom_point() makes it a scatter plot 
ggplot(dataset, aes(x = `Years of Experience`, y = Salary)) + geom_point() + labs(title = "Salary vs Years of Experience", x = "Years of Experience", y = "Salary")
#Scatter Plot of age by salary
ggplot(dataset, aes(x = Age, y = Salary)) + labs(title = "Salary vs Age", x = "Age", y = "Salary") + geom_point()

#Summaries based on different factors
#Education
high_school <- dataset[dataset$`Education Level`== "High School", ]
summary(high_school)
bachelors <- dataset[dataset$`Education Level`== "Bachelor's", ]
summary(bachelors)
masters <- dataset[dataset$`Education Level`== "Master's", ]
summary(masters)
phds <- dataset[dataset$`Education Level`== "PhD", ]
summary(phds)#Education

males <- dataset[dataset$Gender== "Male", ]
summary(males)
females <- dataset[dataset$Gender== "Female", ]
summary(females)
others <- dataset[dataset$Gender== "Other", ]
summary(others)

#Checking if different parts are normal
qqnorm(dataset$Age)
qqline(dataset$Age)

qqnorm(dataset$`Years of Experience`)
qqline(dataset$`Years of Experience`)
