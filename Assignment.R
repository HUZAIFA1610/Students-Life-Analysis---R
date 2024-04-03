#MOHAMMAD ABU HUZAIFA, TP070586
#MANSIMRAN KAUR SOHI, TP066079
#JANANI A/P MUNAINDY, TP066035




### Data Import
assignment <- read.csv("/Users/nafizsp/Documents/Semester-3/Programming For Data Analysis/student_prediction.csv")
### View Dataset
View(assignment)




#Data Validation
#1 Validating R Packages
# Install and load the 'remotes' package
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
library(remotes)
# Install 'plyr' using 'remotes'
remotes::install_github("hadley/plyr")
# Load the 'plyr' package
install.packages("plyr")
library(plyr) 




### Data Exploratory
#1 Print The Data Types Using Class() Function
class(assignment)

#2 Print First Six Rows of Data Frame Using head() Function
head(assignment)

#3 Print Last n Number of Rows Using tail() Function
tail(assignment, 5)

#4 Return Column Names of Data Frame Using names() Function
names(assignment)

#5 Get Number of Rows & Columns of Data Frame Using dim(), nrow(), and ncol() Function
dim(assignment)                              # Print Columns & Rows number
nrow(assignment)                             # Print Rows Number
ncol(assignment)                             # Print Columns Number

#6 Explore Structure of Data Frame Columns Using str() Function
str(assignment)

#7 Calculate Descriptive Statistics Using summary() Function
summary(assignment)




### Data Cleaning/Pre-Processing
#1 Checking Missing Values Using sum() and is.na() Function
sum(is.na(assignment))

#2 View All Column Name Using names() Function
names(assignment)

#3 Rename Column Name Using rename() Function
install.packages("plyr")
library("plyr")
rename(assignment, c("KIDS" = "MARITAL_STATUS", "LIKES_DISCUSS" = "ATTENDING_DISCUSSION", "EXP_GPA" = "EXPECTED CGPA"))
head(assignment)

#4 Format Missing Values
assignment[assignment == ""]

#5 Remove Rows With Missing Values Using na.omit() Function
assignment <- na.omit(assignment)






# Analysing Part for TP070586 (MOHAMMAD ABU HUZAIFA)
#1 UNIVARIATE
install.packages("ggplot2")
library(ggplot2)

summary(assignment$ATTEND)

ggplot(assignment, 
       aes(x= ATTEND, y = after_stat(count/sum(count)))) +
  geom_bar(fill = "purple",
           color = "black") +
  labs(x = "ATTENDANCE",
       y = "Percent",
       title = "Distribution of ATTENDANCE") +
  scale_y_continuous(labels = scales::percent)





#2 BIVARIATE
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library(ggplot2)

plotdata <- assignment %>%
  group_by(GRADE, ATTEND) %>%
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
plotdata

ggplot(plotdata, 
       aes(x = factor(GRADE,
                      levels = c("0", "1", "2", "3", "4", "5", "6", "7"),
                      labels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA")),
           y = pct,
           fill = factor(ATTEND, 
                         levels = c("1", "2", "3"),
                         labels = c("always", "sometimes", "never")))) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = scales::percent) +
  geom_text(aes(label = lbl), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "ATTENDANCE", 
       fill="ATTEND",
       x = "OUTPUT GRADE",
       title = "RELATIONSHIP BETWEEN ATTENDANCE AND OUTPUT GRADE") +
  theme_minimal()






#3 MULTIVARIATE
assignment$ATTEND <- factor(assignment$ATTEND, levels = c(1,2,3), 
                            labels = c("always","sometimes","never"))
assignment$GRADE <- factor(assignment$GRADE, levels = c(0,1,2,3,4,5,6,7), 
                           labels = c("Fail","DD","DC","CC","CB","BB","BA","AA"), 
                           ordered = TRUE)
assignment$LIVING <- factor(assignment$LIVING, levels = c(1,2,3,4), 
                            labels = c("rental","dormitory","with family","other"))

ggplot(assignment, aes(x = ATTEND, fill = LIVING)) +
  geom_bar() +
  facet_wrap(~GRADE) +
  labs(title = "Relationship between GRADE, ATTEND, and LIVING",
       x = "ATTEND",
       y = "GRADE",
       fill = "LIVING")





#4 CHI-SQUARE TEST
x <- xtabs(~GRADE+ATTEND, data=assignment)
plot(x, main="Chi-test between ATTENDANCE and output grade", 
     sub="assignment", col="brown")

tbl <- table(assignment$ATTEND, assignment$GRADE)
chisq.test(tbl)




#5 CORRELATION BETWEEN TWO CATEGORICAL VARIABLES
install.packages("polycor")
library("polycor")
x <- c(assignment$GRADE)
y <- c(assignment$ATTEND)

polychor(x, y)



# Analysing Part for TP066079 (MANSIMRAN KAUR SOHI)
#1 UNIVARIATE
install.packages("dplyr")
library("dplyr")

summary(assignment$NOTES)

plotdata <- assignment %>%
  count(NOTES)
plotdata
ggplot(plotdata,
       aes(x= reorder(NOTES, n), y = n)) +
  geom_bar(stat = "identity",
           fill = "yellow",
           color = "black") +
  labs(x = "TAKING NOTES",
       y = "Frequency",
       title = "Taking Notes Distribution")


# BIVARIATE
library(scales)

ggplot(assignment,
       aes(x = factor(GRADE,
                      levels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA")),
           fill = factor(NOTES, 
                         levels = c("1", "2", "3"),
                         labels = c("never", "sometimes", "always")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = percent) +
  labs(y = "TAKING NOTES",
       fill = "GRADE",
       x = "OUTPUT GRADE",
       title = "RELATION BETWEEN TAKING NOTES AND OUTPUT GRADE") +
  theme_minimal()



# MULTIVARIATE
library(ggplot2)
ggplot(assignment, aes(x = NOTES, y = LIVING, color = GRADE)) +
  geom_point() +
  facet_grid(GRADE ~ .) +
  labs(title = "Scatter Plot Matrix of NOTES, LIVING, and GRADE",
       x = "NOTES", y = "LIVING", color = "GRADE")



#Chi_Square Test
x <- xtabs(~GRADE+NOTES, data=assignment)
plot(x, main="Chi-test between taking notes and output grade", sub="assignment", col="darkgreen")

tbl <- table(assignment$NOTES, assignment$GRADE)
chisq.test(tbl)



# CORRELATION BETWEEN TWO CATEGORICAL VARIABLES
install.packages("polycor")
library("polycor")
x <- c(assignment$GRADE)
y <- c(assignment$NOTES)

polychor(x, y)




# Analysing Part for TP066035 (JANANI A/P MUNAINDY)
# Analyse 1 - Univariate Analysis
# Assuming df is your dataframe
hist(assignment$READ_FREQ_SCI,
     main='Distribution of Reading Frequency in Science Journals',
     xlab='Reading Frequency', col='lightgreen')
hist(assignment$GRADES, 
     main='Distribution of Grades', 
     xlab='Grades', col='black')


# Analyse 2 - Bivariate Analysis
# Visualize the relationship between reading frequency and grades
plot(assignment$READ_FREQ_SCI,
     assignment$GRADES,
     main='Scatter Plot: Reading Frequency vs. Grades', 
     xlab='Reading Frequency', 
     ylab='Grades', col='blue')


# Analyse 3 - Multivariate Analysis
# Assuming you want to model the relationship between reading frequency, study hours, and grades
linear_model <- lm(GRADE ~ READ_FREQ_SCI + STUDY_HRS, data = assignment)
# Summary of the linear regression model
summary(linear_model)

# Visualize the relationship using a scatter plot with regression line
plot(assignment$READ_FREQ_SCI,
     assignment$GRADE,
     main='Scatter Plot: Reading Frequency vs. Grades',
     xlab='Reading Frequency',
     ylab='Grades', col='blue')
abline(linear_model, col = 'red')

# Visualize the relationship for study hours
plot(assignment$STUDY_HRS,
     assignment$GRADE,
     main='Scatter Plot: Study Hours vs. Grades',
     xlab='Study Hours',
     ylab='Grades', col='green')
abline(lm(GRADE ~ STUDY_HRS,
          data = assignment),
       col = 'orange')



# Analyse 4 - Chi-square Test
# Install and load the vcd package
install.packages("vcd")
library(vcd)
# Create a contingency table
cont_table <- table(assignment$READ_FREQ_SCI, assignment$EXP_GPA)
# Perform Chi-square test
chi_result <- chisq.test(cont_table)
# Print the result
print(chi_result)
# Visualize with a mosaic plot
mosaic(cont_table, shade = TRUE)



# Analyse 5 - Correlation between Categorical Variables
# Install and load the vcd package
install.packages("vcd")
library(vcd)
# Assuming you have a categorical variable like "EXP_GPA"
# Create a contingency table
cont_table_cat <- table(assignment$STUDY_HRS, assignment$EXP_GPA)
# Perform Chi-square test
chi_result_cat <- chisq.test(cont_table_cat)
# Print the result
print(chi_result_cat)
# Visualize with a mosaic plot
mosaic(cont_table_cat, shade = TRUE)





# EXTRA FEATURES
#1 DECISION TREE
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree_model <- rpart(GRADE ~ ATTEND + NOTES + READ_FREQ_SCI + STUDY_HRS,
                    data = assignment,
                    method = "class")
rpart.plot(tree_model, yesno = 2, type = 3, extra = 1)




#2 K-Nearest Neighbors Algorithm
install.packages("caret")
install.packages("lattice")
library(ggplot2)
library(lattice)
library(caret)
train_control <- trainControl(method = "cv", number = 10)
knn_model <- train(GRADE ~ ATTEND + NOTES + READ_FREQ_SCI + STUDY_HRS,
                   data = assignment,
                   method = "knn",
                   trControl = train_control)
print(knn_model)




#3 Naive Bayes
# Set working directory
setwd("/Users/nafizsp/Documents/Semester-3/Programming For Data Analysis")

# Read the CSV file
assignment <- read.csv("student_prediction.csv")

# Divide data into training and test datasets
library(caret)
set.seed(1234)
data_partition <- createDataPartition(y = assignment$GRADE, p = 0.8, list = FALSE)
train <- assignment[data_partition, ]
test <- assignment[-data_partition, ]

# Create a Naive Bayes model using training data
library(e1071)
nb_model <- naiveBayes(GRADE ~ ATTEND + NOTES + READ_FREQ_SCI + STUDY_HRS, data = train)

# Predict test data
nb_model
nb_pred <- predict(nb_model, test)
nb_pred

# Measure the performance of your model
library (caret)
table(nb_pred, test$GRADE)

# Visualize the confusion matrix
plot(table)



