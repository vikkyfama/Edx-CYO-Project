##########################################################
# Loading the packages required for analysis and modelling
##########################################################

packages <- c("ggplot2", "tidyverse", "tidyr", "caret", "ranger", "randomForest")
lapply(packages, function(x) {
if (!require(x, character.only = TRUE)) {
install.packages(x, dependenvies = TRUE)
library(x, character.only = TRUE)
}
})

#Import dataset into R for analysis#
Vgames_sales <- read_csv("../input/video-game-sales-with-ratings/Video_Games_Sales_as_at_22_Dec_2016.csv")
library(readr)
Vgames_sales <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
View(Vgames_sales)

#Checking for na values in the data#
colSums(is.na(Vgames_sales))

#Removing records with missing values#
Vg_sales <- Vgames_sales[complete.cases(Vgames_sales), ]
colSums(is.na(Vg_sales))

#Checking to see there are no more na values#
str(Vg_sales)

#Analyzing the Year_of_Release column, removing na values and converting to an integer#
unique(Vg_sales$Year_of_Release)
sales <- sales[sales$Year_of_Release != "N/A", ]
sales <- Vg_sales[Vg_sales$Year_of_Release != "N/A", ]
vg_sales <- Vg_sales[Vg_sales$Year_of_Release != "N/A", ]
vg_sales$Year_of_Release <- as.integer(vg_sales$Year_of_Release)

#Checking and eliminating na values in other variables#
sum(vg_sales$Publisher=="N/A")
vg_sales <- vg_sales[vg_sales$Publisher != "N/A", ]
sum(vg_sales$Developer == "N/A")
sum(vg_sales$Rating == "N/A")

#Checking sales variables for outliers#
summary(vg_sales$NA_Sales)
summary(vg_sales$EU_Sales)
summary(vg_sales$JP_Sales)
summary(vg_sales$Other_Sales)
summary(vg_sales$Global_Sales)

#Converting userScore to a numeric variable and scaling it to match the criticScore variable#
vg_sales$User_Score <- as.numeric(vg_sales$User_Score)
summary(vg_sales$User_Score)
vg_sales$User_Score <- vg_sales$User_Score * 10

#Analyzing the Rating variable and ignoring the ratings with insignificant number of ratings#
vg_sales %>% count(Rating)
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "AO", "M", Rating))
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
vg_sales <- vg_sales %>% mutate(Rating = ifelse(Rating == "RP", "E", Rating))
vg_sales %>% count(Rating)

#Exploring the Global Sales variable#
ggplot(vg_sales) + geom_histogram(aes(Global_Sales), fill = "blue")
ggplot(vg_sales) + geom_histogram(aes(Global_Sales), fill = "blue") +
scale_x_log10()

#Exploring the Year_of_Release  variable#
vg_sales %>% group_by(Year_of_Release) %>%
count() %>% ggplot() +
geom_bar(aes(Year_of_Release, n), stat = "identity",
fill = "blue") + theme(axis.text.x = element_text(angle = 90))

#comparing sales in each year and the release numbers each year#
color <- c("Titles released" = "red", "Global sales" = "blue")
vg_sales %>% group_by(Year_of_Release) %>%
summarise(sales = sum(Global_Sales), count = n()) %>%
ggplot() + geom_line(aes(Year_of_Release, count, group = 1, color = "Titles released")) +
geom_line(aes(Year_of_Release, sales, group = 1, color = "Global sales")) +
xlab("Year of Release") + ylab("Titles released") +
theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
scale_color_manual(values = color) + labs(color = "")
summary(vg_sales$Year_of_Release)

#comparing sales in different geographical areas#
vg_sales %>% gather(area, sales, NA_Sales:Other_Sales,
factor_key = TRUE) %>%
group_by(area, Year_of_Release) %>%
summarise(sales = sum(sales)) %>%
ggplot() +
geom_line(aes(Year_of_Release, sales, group = area, color = area)) +
xlab("Year of release") + ylab("Sales") + labs(color = "") +
theme(legend.text = element_text(size = 7),
legend.position = "bottom",
axis.text.x = element_text(angle = 90))

#Assessing sales per platform#
vg_sales %>% group_by(Platform) %>%
summarise(sales = sum(Global_Sales)) %>% ggplot() +
geom_bar(aes(reorder(Platform, sales), sales), stat = "identity",
fill = "blue") +
xlab("Platform") + ylab("Global sales") +
coord_flip()

#Grouping all platforms into 3 main platform#
vg_sales <- vg_sales %>% mutate(platform2 = case_when(
Platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA") ~ "Nintendo",
Platform %in% c("X360", "XB", "XOne") ~ "XBox",
Platform %in% c("PS3", "PS4", "PS2", "PS", "PSP", "PSV") ~ "PS",
Platform == "PC" ~ "PC",
Platform == "DC" ~ "Sega"
))

#Plotting global sales for each platform#
vg_sales %>% group_by(platform2, Year_of_Release) %>%
summarise(sales = sum(Global_Sales)) %>%
ggplot() +
geom_line(aes(Year_of_Release, sales, group = platform2, color = platform2)) +
xlab("Year of release") + ylab("Global Sales") + labs(color = "") +
theme(legend.text = element_text(size = 7),
axis.text.x = element_text(angle = 90, hjust = 1,
vjust = 0.5, size = 6))

#Assessing sales for Genre#
vg_sales %>% group_by(Genre) %>%
summarise(sales = sum(Global_Sales)) %>%
ggplot() +
geom_bar(aes(reorder(Genre, sales), sales), stat = "identity",
fill = "blue") +
ylab("Global Sales") + xlab("Genre") +
theme(axis.text.x = element_text(angle = 90,
hjust = 1, vjust = 0.5)) +
coord_flip()

#Assessing sales for Developer#
vg_sales %>% group_by(Developer) %>%
summarise(sales = sum(Global_Sales)) %>%
arrange(desc(sales)) %>% slice(1:10) %>%
ggplot() +
geom_bar(aes(reorder(Developer, sales), sales), stat = "identity", fill = "blue") +
theme(axis.text.x = element_text(angle = 90)) +
xlab("Developer") + ylab("Global sales")

################################
# MODELLING
################################

# Creating list for top Publishers and Developers #
publishers_top <- (vg_sales %>% group_by(Publisher) %>%
summarise(sales = sum(Global_Sales)) %>% arrange(desc(sales)) %>%
top_n(10) %>% distinct(Publisher))$Publisher
developers_top <- (vg_sales %>% group_by(Developer) %>%
summarise(sales = sum(Global_Sales)) %>% arrange(desc(sales)) %>%
top_n(10) %>% distinct(Developer))$Developer
vg_sales <- vg_sales %>%
mutate(publisher_top = ifelse(Publisher %in% publishers_top, TRUE, FALSE),
developer_top = ifelse(Developer %in% developers_top, TRUE, FALSE))
vg_sales <- vg_sales %>% group_by(Name) %>% mutate(num_of_platforms = n()) %>% ungroup(Name)

#creating train and test set for algorithm training#
set.seed(1982, sample.kind = "Rounding")
test_index <- createDataPartition(vg_sales$Global_Sales, p = 0.9, list = FALSE)
train_set <- vg_sales[-test_index, ]
test_set <- vg_sales[test_index, ]

#Ensuring all values of the categorical variables are included in the training set#
totalData <- rbind(train_set, test_set)
for (f in 1:length(names(totalData))) {
levels(train_set[, f]) <- levels(totalData[, f])
}

### RMSE measures the difference between actual observations and predictions
### and is defined as follows: 
RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Linear Regression Model#
model_lm <- train(log(Global_Sales) ~ Critic_Score +
User_Score + Genre +
Year_of_Release + Platform + Critic_Count +
User_Count + Rating +
publisher_top + developer_top +
num_of_platforms, method = "lm", data = train_set)

#Calculate prediction and RMSE #
test_set$predicted_lm <- predict(model_lm, test_set)
rmse_results <- data.frame(Method = "Linear regression",
RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_lm))

#SVM Linear Model#
model_svm <- train(log(Global_Sales) ~ Critic_Score +
User_Score + Genre +
Year_of_Release + Platform + Critic_Count +
User_Count + Rating +
publisher_top + developer_top +
num_of_platforms, method = "svmLinear",
data = train_set)

#Calculate prediction and RMSE #
test_set$predicted_svm <- predict(model_svm, test_set)
rmse_results <- rmse_results %>%
add_row(Method = "SVM Linear",
RMSE = RMSE(log(test_set$Global_Sales),
test_set$predicted_svm))

# Random Forest Model using cross validation #
cntrl <- trainControl(method = "repeatedcv", number = 10,
repeats = 3)
tunegrid <- expand.grid(.mtry=c(1:5),
.min.node.size = seq(1, 5, 1),
.splitrule = c("extratrees", "variance"))
model_rf <- train(log(Global_Sales) ~ Critic_Score +
User_Score + Genre +
Year_of_Release + Platform + Critic_Count +
User_Count + Rating +
publisher_top + developer_top +
num_of_platforms, data = train_set,
method = "ranger", trControl = cntrl,
tuneGrid = tunegrid)

#Calculate prediction and RMSE #
test_set$predicted_rf <- predict(model_rf, test_set)
rmse_results <- rmse_results %>% add_row(Method = "Random forest", RMSE = RMSE(log(test_set$Global_Sales), test_set$predicted_rf))

#Compare Models Results #
rmse_results

