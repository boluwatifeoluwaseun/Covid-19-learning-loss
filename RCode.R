# learning_loss
#set working directory in desktop
setwd("~/Desktop/R Files")

# load tidyverse package to read csv files
install.packages("tidyverse")
library(tidyverse)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
install.packages("car")
library(car)
install.packages("lmtest")
library(lmtest)


# Read the data set file
learning_loss <- read_csv("learning_loss.csv")

#Data Inspection and Preparation
dim(learning_loss)
#There were 41 observations and 24 variables

#View basic info of the data
head(learning_loss)
View(learning_loss)

#Find the number of missing values in the dataset = 2
is.na(learning_loss)
which(is.na(learning_loss))
length(which(is.na(learning_loss)))

#Subset data to contain 13 selected variables using the select function
df <- select(learning_loss, country, loss, weeks, gdp, private, internet, hlo, stringency, schooling, region_code, oecd, covid, high)
View(df)

#key Descriptive Statistics
#Summary Statistics for learning loss column
summary(df)
summary(df$loss)
range(df$loss)
max(df$loss) - min(df$loss)
quantile(df$loss, 0.75) - quantile(df$loss, 0.25)
var(df$loss)
sd(df$loss)

#Data Visualisation
#Visualisation One
#Frequency distribution and histogram for learning loss
#Find the range for the data to decide the width of the interval
min(df$loss)
max(df$loss)
length(df$loss)

#Create a sequence of numbers between -0.1 - 0.8 using the function seq by a width of 0.1
intervals <- seq(-0.1,0.8,0.1)
intervals

#Histogram for learning loss
hist(df$loss, breaks = intervals, right = TRUE, main = "histogram of learning loss", xlab = "Learning loss", ylab = "Frequency", ylim = c(0,14), col = "light grey")

#cut the data into the intervals we created to see the frequencies
loss.cut <- cut(df$loss, breaks = intervals, left = FALSE, right = TRUE )
loss.cut

#use the table function to calculate frequencies
lossfrequency <- table(loss.cut)
lossfrequency
View(lossfrequency)

#use prop.table()function to calculate relative frequency
lossProp <- prop.table(lossfrequency)
View(lossProp)

#add frequency text to histogram
text(-0.05,2,1)
text(0.05,8,7)
text(0.15,12,11)
text(0.25,14,13)
text(0.35,5,4)
text(0.45,4,3)
text(0.55,2,1)
text(0.75,2,1)

#Visualisation Two
# Average learning loss for various regions
# Calculate the average learning loss per region
average_loss_by_region <- df %>%
  group_by(region_code) %>%
  summarize(mean_loss = mean(loss, na.rm = TRUE))
  View(average_loss_by_region)
# Bar plot of average learning loss by region
ggplot(average_loss_by_region, aes(x = reorder(region_code, -mean_loss), y = mean_loss, fill = region_code)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_loss,2)), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Purples", type = "seq", guide = FALSE, direction = -1) +
  labs(title = "Average Learning Loss by Region",
       x = "Region",
       y = "Average Learning Loss") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line.x = element_line(size = 0.5, color = "black"),
        axis.line.y = element_line(size = 0.5, color = "black")) +
  coord_cartesian(ylim = c(0, 0.6)) +
  ggtitle("Average Learning Loss by Region") 

#Visualisation Three
# Learning loss by high vs low income countries
# Calculate the average learning loss for high-income and low-income countries
average_loss_by_income <- df %>%
  group_by(high) %>%
  summarize(mean_loss = mean(loss, na.rm = TRUE))
View(average_loss_by_income)
# Grouped bar plot of average learning loss by income group
ggplot(average_loss_by_income, aes(x = factor(high), y = mean_loss, fill = factor(high))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_loss, 2)), 
            position = position_stack(vjust = 1.05), 
            size = 3) +
  scale_x_discrete(labels = c("Low Income", "High Income")) +
  scale_fill_manual(values = c("#0066CC", "#DCDCDC"), name = "Income") +
  labs(title = "Average Learning Loss by Income",
       x = "Income",
       y = "Average Learning Loss") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.5))

#Visualisation Four
# Average learning loss for OECD and non0ECD countries
# Calculate the average learning loss for OECD and non-OECD countries
average_loss_by_oecd <- df %>%
  group_by(oecd) %>%
  summarize(mean_loss = mean(loss, na.rm = TRUE))

# Replace oecd values with more descriptive labels
average_loss_by_oecd$oecd <- ifelse(average_loss_by_oecd$oecd == 1, "OECD", "Non-OECD")

# Bar plot of average learning loss for OECD and non-OECD countries with value labels and no grid lines
ggplot(average_loss_by_oecd, aes(x = oecd, y = mean_loss, fill = oecd)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Non-OECD" = "steelblue", "OECD" = "lightblue"), name = "Country Group") +
  geom_text(aes(label = round(mean_loss, 2)), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Average Learning Loss for OECD and Non-OECD Countries",
       x = "Country Group",
       y = "Average Learning Loss (Standard Deviations)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.5))

#Visualisation Five
#box and whisker plot 1 for outlier identification
box_loss <- boxplot(df$loss,main="Box Plot of Learning Loss", ylab = "Learning loss", xlab = "countries")

#Visualisation Six
#scatter plot to visualise the relationship between learning loss and weeks school closed
plot(df$weeks, df$loss, pch = 18, cex = 0.9, col = "#7986CB", xlab = "Weeks Schools Closed", ylab = "Learning Loss", main = "Learning Loss VS Weeks School Closed")
abline(lm(df$loss~df$weeks), col = "red")

#Visualisation Seven
#Correlational Analysis 
#Create a new table for the numerical variables
Numeric_vars <- df[, c("loss", "weeks","gdp","private","internet","hlo","stringency","schooling", "covid")]
#find the correlation matrix for the variables
cor_matrix <- cor(Numeric_vars)
cor_matrix

#Create a Correlational matrix heat map to visualise
palette <- colorRampPalette(brewer.pal(n = 12, name = "Set3"))(25)
# Create a heat map of the correlation matrix
ggcorrplot(cor_matrix, type = "full", hc.order = TRUE, 
           lab = TRUE, lab_size = 3, tl.cex = 12, 
           title = "Correlation Matrix Heatmap",
           colors = palette)


#Regression
#split the data into TData and VData(80% and 20%)
TData <- df[1:32,]
VData <- df[33:41,]
View(TData)
View(VData)

#Start building the multiple linear regression models
#select all variables with moderate correlation with learning loss +/- 0.4
modelA <- lm(loss ~ weeks + gdp + covid + internet + schooling, data = TData)
summary(modelA)       
#Adjusted r-square 0.502
#check for multicollinearity
vif(modelA)

#gdp had the largest insignificant p-value, it will be removed from our model
modelB <- lm(loss ~ weeks + covid + internet + schooling, data = TData)
summary(modelB)
#Adjusted r-square 0.5193
#check for multicollinearity
vif(modelB)

#internet had the largest insignificant p-value, it will be removed from our model
modelC <- lm(loss ~ weeks + covid + schooling, data = TData)
summary(modelC)
#adjusted r-square dropped to 0.4942
#check for multicollinearity
vif(modelC)

#scholing had the largest insignificant p-value, it will be removed from our model
modelD <- lm(loss ~ weeks + covid, data = TData)
summary(modelD)
#adjusted r-square is 0.4924, all variables are significant

#testing for interaction between covid and weeks
TData$interactionCW <- TData$weeks*TData$covid

#continue model development with interaction
modelE <- lm(loss ~ weeks + covid + interactionCW, data = TData)
summary(modelE)
#adjusted r-square is 0.4822

#remove covid
modelF <- lm(loss ~ weeks + interactionCW , data = TData)
summary(modelF)
#adjusted r-square is 0.4826 and all variables are significant

#test for assumptions for modelB
#using the resid function to get the residuals
residmodelB <- resid(modelB)

#Visualisation Eight
#plot residuals against learningloss(X) to examine their distribution
plot(residmodelB~ TData$loss, xlab = "Learning loss", ylab = "Residuals (ModelB)", main = "ModelB Residual Plot")
abline(0,0)

# Perform the Breusch-Pagan test to test for homoscedasticity
bp_test <- bptest(modelB)
print(bp_test)

# Perform the Shapiro-Wilk test
shapiro_test <- shapiro.test(residmodelB)
print(shapiro_test)
#Visualisation Nine
#plot residuals density function to check for normality of errors
plot(density(residmodelB))

#Cross validation
#Add interactionCW column to the VData
VData$interactionCW <- VData$weeks*VData$covid
#Model A
Pred1 <- predict(modelA, VData)
Pred1
sqrt(mean((VData$loss-Pred1)^2))

#Model B
Pred2 <- predict(modelB, VData)
Pred2
sqrt(mean((VData$loss-Pred2)^2))

#Model C
Pred3 <- predict(modelB, VData)
Pred3
sqrt(mean((VData$loss-Pred3)^2))

#Model D
Pred4 <- predict(modelD, VData)
Pred4
sqrt(mean((VData$loss-Pred4)^2))

#Model E
Pred5 <- predict(modelE, VData)
Pred5
sqrt(mean((VData$loss-Pred5)^2))

#Model F
Pred6 <- predict(modelF, VData)
Pred6
sqrt(mean((VData$loss-Pred6)^2))






#APPENDIX
#Visualisation 2b
#Frequency distribution and bar chart for categorical data(region code)
regionFrequency <- table(df$region_code)
regionFrequency
View(regionFrequency)

#Relative frequency
regionProp <- prop.table(regionFrequency)
regionProp
View(regionProp)

#Used the information from the frequency distribution table to create a barplot
regionNames <- c("Advanced Economies", "Sub-Saharan Africa","Latin America and the Caribbean", "East Asia and the Pacific", "Europe and Central Asia", "South Asia")
regionColor <- c("#1A237E","#3949AB","#5C6BC0","#7986CB", "#9FA8DA","#C3CAEA")
no_ofcountries <- c(18,7,5,4,4,3)
barplot(no_ofcountries,col =regionColor, main = "No of countries in each region", ylab = "No of countries", xlab ="Region code", ylim = c(0,20))

#add abline and legend
abline(h=0, col = "black")
legend(x="topright",fill = regionColor, horiz = FALSE, legend = regionNames, cex = 0.6, inset = 0)

#add text of each frequency
text(0.6,19, 18)
text(1.9,8,7)
text(3.1,6,5)
text(4.3,5,4)
text(5.5,5,4)
text(6.7,4,3)


#Visualisation 3b
#Frequency distribution and column chart for high and low income countries
highFrequency <- table(df$high)
highFrequency
View(highFrequency)

#Relative frequency
highProp <- prop.table(highFrequency)
highProp
View(highProp)

#Used the information from the frequency distribution table to create a barplot
highColor <- c("#DCDCDC","#0066CC")
barplot(highFrequency, main = "Number of low-income and high-income countries", ylab = "Count", xlab = "Country", ylim = c(0,30), col = highColor)

#add abline and legend
abline(h=0, col = "black")
# Add legend with custom labels
legend(x = "topright", fill = highColor, legend = c("Low", "High"), bty = "n", cex = 1.0)
#add text of each frequency
text(0.6,25, 24)
text(1.85,18,17)


#Visualisation 4b
#Frequency distribution and column chart for OECD and nonOECD countries
oecdFrequency <- table(df$oecd)
oecdFrequency
View(oecdFrequency)

#Relative frequency
oecdProp <- prop.table(oecdFrequency)
oecdProp
View(oecdProp)

#Used the information from the frequency distribution table to create a barplot
highColor2 <- c("#3949AB", "#DCDCDC")
barplot(oecdFrequency, main = "Number of nonOECD and OECD countries", ylab = "Count", xlab = "Country", ylim = c(0,30), col = highColor2)

#add abline and legend
abline(h=0, col = "black")
# Add legend with custom labels

legend(x = "topright", fill = highColor2, legend = c("nonOECD", "OECD"), bty = "n", cex = 0.8)
#add text of each frequency
text(0.6,18, 17)
text(1.85,25,24)

