##############################################################################

# IMPORTANT FOREWORD - REQUIREMENTS

# Dear Readers, 

# In this project, there are 6 files, which you can find in my GitHub repository: 
# https://github.com/Dev-P-L/Recommendation-System . The 6 files are: 
# Recommendation_Script.R, Recommendation_Report.Rmd, Recommendation_Report.pdf, 
# movie_user_genres_time_results.csv, time_genres_movie_user_results.csv and README.md.  

# Recommendation_Script.R has been run in 
# RStudio Version 1.1.456 - © 2009-2018 RStudio, Inc. 

# Recommendation_Report.Rmd has been knitted to HTML in 
# RStudio Version 1.1.456 - © 2009-2018 RStudio, Inc.

# The version of R that I use on my PC is
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit).

# The operating system on my PC is Windows 10.

# I cannot guarantee Recommendation_Script.R running on other versions. Neither can I 
# guarantee Recommendation_Report.Rmd being knitted to HTML on other versions.
  
# Before running Recommendation_Script.R or knitting Recommendation_Report.Rmd to HTML, 
# please adapt the working directory and choose a working directory on your machine. 
# It has to be done on line 73 in Recommendation_Script.R and on line 211 
# in the file Recommendation_Report.Rmd. This is essential.
  
# When you have indicated in the code a working directory on your machine, then, and 
# only then, you can run Recommendation_Script.R and knit Recommendation_Report.Rmd to HTML. 
# The file Recommendation_Script.R must absolutely be run before the file 
# Recommendation_Report.Rmd can be knitted to HTML. Indeed, data from MovieLens 
# (edx and the validation set) are downloaded ONLY by Recommendation_Script.R, 
# are saved in the chosen working directory and are available there 
# for Recommendation_Report.Rmd to retrieve them and use them. 

# Recommendation_Script.R takes approximately 3 quarters of an hour to run on my laptop 
# and it takes more or less 35 minutes to knit Recommendation_Report.Rmd (knit to HTML) 
# on my laptop. But I need to close all other applications before launching these programs 
# in order to avoid blue screens! 
  
# The code in both programs contains instructions to download the following packages
# if they are not available on your machine: tidyverse, caret, lubridate, broom 
# and kableExtra. Data files will also be downloaded. 

# Good luck!

##############################################################################

## CLEANING USER INTERFACE FOR RAM MANAGEMENT

# The function invisible() is used to prevent technical information showing up 
# in Recommendation_Report.Rmd and, most important, in Recommendation_Report.pdf. That piece of information 
# would have no operational impact. 

# Clearing plots
invisible(if(!is.null(dev.list())) dev.off())

# Cleaning workspace
rm(list=ls())

# Cleaning console
cat("\014")

#################################################################################

## SETTING WORKING DIRECTORY

setwd("C:/Users/Acer/projects/Recommendation-System")

#################################################################################

## LOADING PACKAGES

# I use the suppressPackageStartupMessages() and the suppressMessages() functions to avoid 
# drowning in messages. Warnings are not suppressed except for kableExtra whose loading 
# generates a "warning" just about the package having been loaded with version R 3.5.2 
# P.S. kableExtra helps reshape tables.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

################################################################################

## DOWNLOADING DATA AND STRUCTURING DATASETS

# Download is prepared with the tempfile() function which gives a name and a path 
# to the temporary file that will receive the downloaded data.

dl <- tempfile()

# Downloading with the option "quiet = TRUE" avoids messages and progress device.

suppressMessages(download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", 
                 dl, quiet = TRUE))

# Data relative to ratings (userId, movieId, rating and timestamp)
# will be reshaped with the read.table() function from a table format file
# into a data frame. The gsub() function replaces pattern included in "::" 
# by "\t" (tab) in ratings.dat. The col.names() function gives names to variables. 

ratings <- read.table(text = 
                      gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

# Data relative to movies will be reshaped into the matrix "movies".
# The str_split_fixed() function will split strings, the pattern included in "\\::" being 
# the pattern to split up by. The colnames() function gives names to the 3 variables. 

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# For RAM management, I'll systematically remove all objects as soon as they become 
# useless, even graphs. Here, I do it for dl.

rm(dl)

# "movies" will be formatted from a matrix into a data frame with movieIds as numerical 
# values and "title" and "genres" as characters (and not factors). 
 
movies <- as.data.frame(movies) %>% 
          mutate(movieId = as.numeric(levels(movieId))[movieId],
          title = as.character(title), genres = as.character(genres))

# We can now join the data frame "ratings" and the data frame "movies" 
# into the  data frame "movielens". The left_join() function will take over all rows 
# from the first data frame (ratings) even when there are several rows for 
# the same movie (because there are several user ratings for that movie).  

movielens <- left_join(ratings, movies, by = "movieId")
rm(ratings, movies)

# We will now build up a validation set that will be 10% of movielens and a set called edx 
# with the rest. Reproducibility of results is assured by function set.seed(1).   

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
rm(test_index, movielens)

# Making sure that all values from userId and movieId in validation set are also in edx set:
# the semi_join() function returns only rows from temp that have a match in edx.

validation <- temp %>% semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")

# The rows from temp that have not been taken over from temp into the validation set 
# are sent into the data set "removed". The anti_join() function does the job. 

removed <- suppressMessages(anti_join(temp, validation))
rm(temp)

# Adding removed to edx.

edx <- rbind(edx, removed)
rm(removed)

# The validation set will now be saved and removed. It will only be used
# at the very final step of the process when applying the final model. 

write.csv(validation, "validation.csv")
rm(validation)

# Let's save the edx set so that we can remove it from workspace for RAM 
# management reasons when we only use subcomponents from edx for training or testing. 

write.csv(edx, "edx.csv")

# We will extract from edx a training set and a test set. We will call them 
# "edx_train" and "edx_test" to avoid any confusion with the validation set.
# We will do that by splitting edx in halves: please see Recommendation_Report.Rmd and 
# Recommendation_Report.pdf for motivation. 

set.seed(1)
ind <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
edx_train <- as.data.frame(edx[ind,]) %>%
  mutate(title = as.character(title), genres = as.character(genres))
edx_test <- as.data.frame(edx[-ind,]) %>%
  mutate(title = as.character(title), genres = as.character(genres))
rm(ind)

# Let's put into the dataset "removed_movieId" all movieIds from edx_test that are not 
# in edx_train.  In edx_test, we'll only keep movieIds that are in edx_train. 

removed_movieId <- anti_join(edx_test, edx_train, by = "movieId")
edx_test <- semi_join(edx_test, edx_train, by = "movieId")
edx_train <- bind_rows(edx_train, removed_movieId)
rm(removed_movieId)

# Let's do the same, mutatis mutandis, for userIds.

removed_userId <- anti_join(edx_test, edx_train, by = "userId")
edx_test <- semi_join(edx_test, edx_train, by = "userId")
edx_train <- bind_rows(edx_train, removed_userId)
rm(removed_userId)

# Let's save edx_train and edx_test so that we can remove them from workspace
# for RAM management reasons as soon as one of them is not useful.

write.csv(edx_train, "edx_train.csv")
rm(edx_train)
write.csv(edx_test, "edx_test.csv")
rm(edx_test)

############################################################

## EXPLORATORY ANALYSIS

# Number of rows in edx

df <- data.frame(Number_of_rows_in_edx = nrow(edx))
df %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                   full_width = F, font_size = 12) %>% column_spec(1, width = "3in")
rm(df)

# Variables in edx
# First: determining the role of the variables (dependent or independent)

vector_status <- c("Independent_variable", "Independent_variable", 
                   "Dependent_variable", "Independent_variable", 
                   "Independent_variable", "Independent_variable")

# Second: building up a table with the names of the variables and their status (dependent
# or independent) 

df <- data.frame(Variables_in_edx = as.character(colnames(edx)), 
                 Status = as.character(vector_status)) %>% arrange(Status)                 
df %>% kable() %>% kable_styling(bootstrap_options = "bordered",
                   full_width = F, font_size = 12) %>% column_spec(1:2, width = "2.5in")
rm(vector_status, df)

# THE DEPENDENT VARIABLE IN EDX: RATING
# First: the mean and the median of the variable rating in edx

mu_edx <- mean(edx$rating)
df <- data_frame(Mean_of_Ratings_in_edx = mu_edx, 
                 Median_of_Ratings_in_edx = median(edx$rating)) 
df %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                 full_width = F, font_size = 12)
rm(df)

# Second: barplot of edx ratings (in crimson: mean)

graph <- edx %>% select(rating) %>% group_by(rating) %>% 
  summarize(n_in_thousands = n() / 1000) %>% as.data.frame() %>% 
  ggplot(aes(rating, n_in_thousands)) + 
  geom_bar(stat = "identity", width = 0.1, fill = "#007ba7") + 
  geom_vline(xintercept = mu_edx, linetype = "dotted", color = "#c90016", size = 2.5) +
  ggtitle("Barplot of edx Ratings") +
  xlab("Values of Ratings") + ylab("Count of Ratings (in thousands)") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
graph
rm(edx, graph, mu_edx)

# FIRST DEPENDENT VARIABLE CANDIDATE: TIMESTAMP. IS TIME RELEVANT 
# AS AN INDEPENDENT VARIABLE, AS A PREDICTOR?

# Let's convert timestamp, using the lubridate package. We'll work on edx_train instead of 
# edx for 2 reasons: managing RAM and leaving an opportunity to test on edx_test. 

temp <- read.csv("edx_train.csv") %>% select(timestamp, rating) %>% 
  mutate(date_month = round_date(as_datetime(timestamp), unit = "month")) %>%
  mutate(date_week = round_date(as_datetime(timestamp), unit = "week")) %>%
  mutate(date_day = round_date(as_datetime(timestamp), unit = "day")) %>%
  mutate(date_hour = round_date(as_datetime(timestamp), unit = "hour")) %>%
  as.data.frame()
mu <- mean(temp$rating)

# We also build up smaller data frames with means per periodicity. When possible, 
# these smaller data frames will be used instead of edx_train (renamed temp), 
# for RAM management reasons.

temp_month <- temp %>% group_by(date_month) %>% 
                       summarize(avgs_month = mean(rating)) %>% as.data.frame()
temp_week <- temp %>% group_by(date_week) %>% 
                       summarize(avgs_week = mean(rating)) %>% as.data.frame()
temp_day <- temp %>% group_by(date_day) %>% 
                       summarize(avgs_day = mean(rating)) %>% as.data.frame()  
temp_hour <- temp %>% group_by(date_hour) %>% 
                       summarize(avgs_hour = mean(rating)) %>% as.data.frame() 

# Information about means will also be integrated into temp: indeed, in some cases, we will
# need information about means at the level of each observation from temp (edx_train). 
  
temp <- temp %>% 
  left_join(temp_month, by ="date_month") %>%
  left_join(temp_week, by ="date_week") %>%
  left_join(temp_day, by ="date_day") %>%
  left_join(temp_hour, by = "date_hour") %>% as.data.frame()

# Let's get started with exploring monthly averages. On the graph, a green line
# is representing the general mean of ratings from temp (edx_train). Analytical comments 
# are available in Recommendation_Report.Rmd and Recommendation_Report.pdf. 

graph <- temp_month %>% ggplot(aes(x = date_month)) + 
  geom_point(aes(y = avgs_month), size = 3, col = "#007ba7") +
  geom_hline(yintercept = mu, linetype=2, size = 2.5, col = "#50c878") +
  ggtitle("Monthly Averages of edx_train Ratings") +
  xlab("Dates with Monthly Periodicity") + ylab("Monthly Aver. of edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Weekly averages: on the left graph, the y scale is too large for visualization 
# due to outliers, which are also illustrated on an adjacent boxplot. 

par(mfrow = c(1, 2))
graph <- plot(temp_week$date_week, temp_week$avgs_week, col = "#007ba7", 
         main = "Weekly Aver. with Outliers", 
         xlab = "Dates", ylab = "Weekly Aver. of edx_train Ratings")

box<- boxplot(temp_week$avgs_week, col = "white", outcol = "#c90016", staplecol = "#50c878",
              main = "Boxplot of Weekly Aver.", ylab = "Weekly Aver. of edx_train Ratings")

invisible(dev.off())
rm(graph)

# For visualization purposes, let's widen the main band of points by "pulling" outliers 
# towards the center, stopping at the level of the extremes of the whiskers 
# from the boxplot above.

upper_whisker <- box$stats[5]
lower_whisker <- box$stats[1]
rm(box)  
  
avgs_week_coerced <- temp_week$avgs_week
avgs_week_coerced[avgs_week_coerced > upper_whisker] <- upper_whisker
avgs_week_coerced[avgs_week_coerced < lower_whisker] <- lower_whisker
temp_week <- temp_week %>% mutate(avgs_week_coerced = avgs_week_coerced) 
rm(avgs_week_coerced, upper_whisker, lower_whisker)

graph <- temp_week %>% ggplot(aes(x = date_week)) + 
  geom_point(aes(y = avgs_week_coerced), col = "#007ba7", alpha = 0.50) +
  geom_hline(yintercept = mu, linetype = 2, size = 2.5, col = "#50c878") +
  ggtitle("Weekly Rating Means - Outliers Coerced between Whiskers") +
  xlab("Dates with Weekly Periodicity") + ylab("Weekly Means of edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Daily averages

par(mfrow = c(1, 2))
graph <- plot(temp_day$date_day, temp_day$avgs_day, col = "#007ba7",
              main = "Daily Aver. with Outliers", 
              xlab = "Dates", ylab = "Daily Aver. of edx_train Ratings")

box<- boxplot(temp_day$avgs_day, col = "white", outcol = "#c90016", staplecol = "#50c878", 
              main = "Boxplot of Daily Aver.", ylab = "Daily Aver. of edx_train Ratings")

invisible(dev.off())
rm(graph)

upper_whisker <- box$stats[5]
lower_whisker <- box$stats[1]
rm(box)  

avgs_day_coerced <- temp_day$avgs_day
avgs_day_coerced[avgs_day_coerced > upper_whisker] <- upper_whisker
avgs_day_coerced[avgs_day_coerced < lower_whisker] <- lower_whisker
temp_day <- temp_day %>% mutate(avgs_day_coerced = avgs_day_coerced) 
rm(avgs_day_coerced, upper_whisker, lower_whisker)

graph <- temp_day %>% ggplot(aes(x = date_day)) + 
  geom_point(aes(y = avgs_day_coerced), col = "#007ba7", alpha = 0.25) +
  geom_hline(yintercept = mu, linetype = 2, size = 2.5, col = "#50c878") +
  ggtitle("Daily Rating Means - Outliers Coerced between Whiskers") +
  xlab("Dates with Daily Periodicity") + ylab("Daily Means of edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Visual evidence of time effect provided by daily averages can be highlighted 
# by using e.g. a smoothing spline. With the option df = 10, I have determined 
# the equivalent number of degrees of freedom and regulated the smoothness degree. 
# In blue on the graph hereunder figure the fitted values from the smoothing spline 
# that is applied to the daily averages of ratings. 

s <- smooth.spline(x = temp_day$date_day, y = temp_day$avgs_day, df = 10)
temp_day <- temp_day %>% mutate(avgs_day_spline_10df = fitted(s))
rm(s)

graph <- temp_day %>% ggplot(aes(x = date_day)) + 
  geom_point(aes(y = avgs_day_coerced), col = "#708090", alpha = 0.25) +
  geom_line(aes(y = avgs_day_spline_10df), size = 2.5, col = "#007ba7") +
  geom_hline(yintercept = mu, linetype=2, size = 2.5, col = "#50c878") +
  ggtitle("Daily Rating Means - Outliers Coerced between Whiskers") +
  xlab("Dates with Daily Periodicity") + ylab("Daily Means of edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)
temp_day <- temp_day %>% select(- avgs_day_coerced)

# Let's add to temp the results from the smoothing spline that has been applied
# to the daily averages of ratings from temp (edx_train).

buffer <- temp_day %>% select(date_day, avgs_day_spline_10df) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_day") %>% as.data.frame()
rm(buffer)

# Let's move to hourly averages. Representing hourly averages on the same kind of graph 
# as daily averages would lead to some visual fuzziness. Let's only represent the results 
# from a smoothing spline applied to hourly averages and the general average of ratings. 

s <- smooth.spline(x = temp_hour$date_hour, y = temp_hour$avgs_hour, df = 10)
temp_hour <- temp_hour %>% mutate(avgs_hour_spline_10df = fitted(s))
rm(s)

graph <- temp_hour %>% ggplot(aes(x = date_hour)) + 
  geom_line(aes(y = avgs_hour_spline_10df), size = 2.5, col = "#007ba7") +
  geom_hline(yintercept = mu, linetype = 2, size = 2.5, col = "#50c878") +
  ggtitle("Smoothing Spline on Hourly Averages of edx_train Ratings") +
  xlab("Hourly Periodicity") + ylab("Hourly Aver. of edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Let's add to temp the results from the smoothing spline that has been 
# applied to hourly averages of ratings from temp (edx_train).

buffer <- temp_hour %>% select(date_hour, avgs_hour_spline_10df) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_hour") %>% as.data.frame()
rm(buffer)

# After looking for VISUAL evidence of time effect (with 4 periodicities), let's 
# look for STATISTICALLY SIGNIFICANT evidence. Is there a time trend in ratings 
# that can be considered as statistically significant? Let's have a first check 
# with a linear regression on weekly dates, which is an intermediate periodicity... 

ols_1 <- lm(rating~date_week, data = temp)

# Let's save fitted values for further graphical use. 

# Here, there is a small numerical challenge. In the first regression, there are 
# many more y values (edx_train ratings) than DISTINCT x values (weekly dates). 
# For each x value, we should get one fitted value on the y-axis; actually, 
# there are minuscule numerical differences between some fitted values corresponding 
# to the same x value (to the same date). The "biggest" difference has the first 
# significant figure in the 7th decimal place; all other differences are smaller than 1E-15.
# Nevertheless, I had to introduce a rule to be able to create a graph: I have picked up 
# the means of the fitted values corresponding to each x value. There is 
# neither visual nor analytical impact.

buffer <- temp %>% select(date_week) %>% mutate(fit = ols_1$fitted.values) %>% 
  group_by(date_week) %>% summarize(fit_ols_1 = mean(fit)) %>% as.data.frame()
temp_week <- temp_week %>% left_join(buffer, by = "date_week")
rm(buffer)

# Results from regressing ratings (from temp or edx_train) on dates with weekly periodicity.

summary(ols_1)

# The coefficient of the time trend is statistically significant according to the p-value. 
# But the R-squared (R2) is minute. Analytical comments can be found in 
# Recommendation_Report.Rmd and in Recommendation_Report.pdf.

# Since R2 was so low, let's try another representation of time effect: e.g. the fitted 
# values from a smoothing spline applied to weekly averages. 

s <- smooth.spline(x = temp_week$date_week, y = temp_week$avgs_week, df = 10)
temp_week <- temp_week %>% mutate(avgs_week_spline_10df = fitted(s))

buffer <- temp_week %>% select(date_week, avgs_week_spline_10df) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_week") %>% as.data.frame()
rm(s, buffer)

# With the results from the smoothing spline applied to weekly averages, let's run 
# another linear regression: this time we will regress ratings on the fitted values 
# from the smoothing applied to weekly averages. The ts() function preserves the status 
# of time series of both series.

# Just as in the case of the first regression, there is a small numerical challenge. 
# There are many more y values (edx_train ratings) than DISTINCT x values (splined  
# weekly averages of edx_train ratings). That is normal. For each x value, 
# we should get one fitted value on the y-axis. Actually, there are Lilliputian differences 
# between some fitted values corresponding to the same x value. I have decided to take 
# the means of fitted values for each x value. There is neither visual nor analytical impact.

ols_2 <- lm(ts(rating)~ts(avgs_week_spline_10df), data = temp)
buffer <- temp %>% select(date_week) %>% mutate(fit = ols_2$fitted.values) %>% 
  group_by(date_week) %>% summarize(fit_ols_2 = mean(fit)) %>% as.data.frame()
temp_week <- temp_week %>% left_join(buffer, by = "date_week")
rm(buffer)

# To better analyze R2 results from both regressions above, let's decompose the value of R2. 
# To do this, let's remember that R2 equals 1 - SSE / SST where SSE is 
# the sum of squared errors (sum of the squared differences between dependent variable 
# and fitted values) and that SST is the total sum of squares (sum of the squared differences 
# between dependent variable and the mean of the dependent variable). 

# RMSE is simply the square root of the division of SSE by the number of observations.

R2_1 = as.numeric(glance(ols_1)[1])
SSE_1 = sum((ols_1$fitted.values - temp$rating) ^ 2)
SST = sum((temp$rating - mu) ^ 2)
Checking_R2_1 = 1 - (SSE_1 / SST) 
RMSE_1 <- sqrt(SSE_1 / nrow(temp))
decomposing_R2_ols_1 <- data.frame(Regression = as.character("Regression 1"), 
  x_data = as.character("Dates with Weekly Periodicity"), R2 = R2_1, SSE = SSE_1, 
  SST = SST, Checking_R2 = Checking_R2_1, RMSE_on_edx_train = RMSE_1, 
  stringsAsFactors = FALSE) 

R2_2 = as.numeric(glance(ols_2)[1])
SSE_2 = sum((ols_2$fitted.values - temp$rating) ^ 2)
Checking_R2_2 = 1 - (SSE_2 / SST) 
RMSE_2 <- sqrt(SSE_2 / nrow(temp))
decomposing_R2_ols_2 <- data.frame(Regression = as.character("Regression 2"),
  x_data = as.character("Splined Weekly Averages of Ratings"), R2 = R2_2, SSE = SSE_2, 
  SST = SST, Checking_R2 = Checking_R2_2, RMSE_on_edx_train = RMSE_2, 
  stringsAsFactors = FALSE)

decomposing_R2_ols <- rbind(decomposing_R2_ols_1, decomposing_R2_ols_2)
decomposing_R2_ols %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                                 full_width = F, font_size = 12) 

rm(ols_1, ols_2, R2_1, R2_2, SSE_1, SSE_2, SST)
rm(Checking_R2_1, Checking_R2_2, RMSE_1, RMSE_2)
rm(decomposing_R2_ols, decomposing_R2_ols_1, decomposing_R2_ols_2)

# The R2 from the second regression above (= regressing ratings on splined weekly averages)
# is slightly better but still remains minute. Why is it any better? Let's visualize it 
# on graphs. 

# First a graph about the 1st regression (= ratings regressed on dates with weekly 
# periodicity):  the x values are simply dates (date_week), i.e. a "flat" time series.
# In blue on the graph figure: the fitted values from the 1st regression. For illustrative 
# purposes, weekly averages (coerced between whiskers) have been added as points.  

graph <- temp_week %>% ggplot(aes(x = date_week)) + 
  geom_point(aes(y = avgs_week_coerced), alpha = 0.25) + 
  geom_line(aes(y = fit_ols_1), size = 2.5, col = "#007ba7") +
  geom_hline(yintercept = mu, linetype = 2, size = 2.5, col = "#50c878") +
  ggtitle("Regression 1 with Weekly Dates as Independent Variable") +
  xlab("Dates with Weekly Periodicity") + ylab("edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)
temp_week$fit_ols_1 <- NULL

# Let's do the same for the second regression (ratings regressed on results from
# a spline applied to weekly averages). The crimson line represents the results 
# from the spline used as x data in the regression. The blue line represents 
# the fitted values from the regression. 

# In each series, one value has been decreased at the beginning of the period 
# to improve visual comparability with the former graph. 

maxi <- max(temp_week$avgs_week_coerced)
temp_week$borned_spline <- temp_week$avgs_week_spline_10df
temp_week$borned_spline[temp_week$borned_spline > maxi] <- maxi
temp_week$fit_ols_2[temp_week$fit_ols_2 > maxi] <- maxi

graph <- temp_week %>% ggplot(aes(x = date_week)) + 
  geom_point(aes(y = avgs_week_coerced), alpha = 0.25) +
  geom_line(aes(y = borned_spline), linetype = 2, size = 1, col = "#c90016") +
  geom_line(aes(y = fit_ols_2), size = 1, col = "blue") +
  geom_hline(yintercept = mu, linetype = 2, size = 2, col = "#50c878") +
  ggtitle("Regression 2: Ratings on Splined Weekly Aver.") +
  xlab("Splined Weekly Aver. of Ratings") + ylab("edx_train Ratings") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph

rm(maxi, graph)
temp_week$avgs_week_coerced <- NULL
temp_week$fit_ols_2 <- NULL

# The blue line is curved and is in a better position to capture rating movements than the 
# straight line from the previous graph. Discussion is available in 
# Recommendation_Report.Rmd and Recommendation_Report.pdf.

# Let's investigate more broadly in search of other possible representations
# of time. We already have 11 possible time trends: 4 lists of dates (each corresponding 
# to one periodicity), 4 lists of averages (one for each periodicity) and 
# 3 smoothing splines (splines on respectively weekly, daily and hourly averages). 
# Let's add a few ones.

# First a smoothing spline with monthly periodicity. The option df = 6 has been chosen 
# for monthly periodicity instead of df = 10 for weekly, daily or hourly periodicity. Why? 
# Because df = 10 would remove part of the smoothness degree of the monthly periodicity.

s <- smooth.spline(x = temp_month$date_month, y = temp_month$avgs_month, df = 6)
temp_month <- temp_month %>% mutate(avgs_month_spline_6df = fitted(s))
rm(s)

buffer <- temp_month %>% select(date_month, avgs_month_spline_6df) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_month") %>% as.data.frame()
rm(buffer)

# Let's also add 4 smoothing splines with built-in cross-validation, one for each 
# periodicity. I've opted for cv = TRUE, which triggers a "leave-one-out" cross-validation 
# process (more information in Recommendation_Report.Rmd and Recommendation_Report.pdf).

s <- smooth.spline(x = temp_month$date_month, y = temp_month$avgs_month, cv = TRUE)
temp_month <- temp_month %>% mutate(avgs_month_spline_looCV = fitted(s))
rm(s)
buffer <- temp_month %>% select(date_month, avgs_month_spline_looCV) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_month") %>% as.data.frame()
rm(buffer)

s <- smooth.spline(x = temp_week$date_week, y = temp_week$avgs_week, cv = TRUE)
temp_week <- temp_week %>% mutate(avgs_week_spline_looCV = fitted(s))
rm(s)
buffer <- temp_week %>% select(date_week, avgs_week_spline_looCV) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_week") %>% as.data.frame()
rm(buffer)

s <- smooth.spline(x = temp_day$date_day, y = temp_day$avgs_day, cv = TRUE)
temp_day <- temp_day %>% mutate(avgs_day_spline_looCV = fitted(s))
rm(s)
buffer <- temp_day %>% select(date_day, avgs_day_spline_looCV) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_day") %>% as.data.frame()
rm(buffer)

s <- smooth.spline(x = temp_hour$date_hour, y = temp_hour$avgs_hour, cv = TRUE)
temp_hour <- temp_hour %>% mutate(avgs_hour_spline_looCV = fitted(s))
rm(s)
buffer <- temp_hour %>% select(date_hour, avgs_hour_spline_looCV) %>% as.data.frame()
temp <- temp %>% left_join(buffer, by = "date_hour") %>% as.data.frame()
rm(buffer)

# Before running the lm() function on the 16 series, let's organize a results table 
# that will receive R2 results after lm() has been run. 

list_colnames <- c("date_month", "date_week", "date_day", "date_hour",
                   "avgs_month", "avgs_week", "avgs_day", "avgs_hour",
                   "avgs_month_spline_looCV", "avgs_week_spline_looCV", 
                   "avgs_day_spline_looCV", "avgs_hour_spline_looCV",
                   "avgs_month_spline_6df", "avgs_week_spline_10df", 
                   "avgs_day_spline_10df", "avgs_hour_spline_10df")

list_descriptions <- c("Monthly dates", "Weekly dates", "Daily dates", "Hourly dates",
                       "Monthly av. of ratings", 
                       "Weekly av. of ratings",
                       "Daily av. of ratings",
                       "Hourly av. of ratings", 
                       "Monthly av. splined with leave-one-out CV",
                       "Weekly av. splined with leave-one-out CV",
                       "Daily av. splined with leave-one-out CV",
                       "Hourly av. splined with leave-one-out CV",
                       "Monthly av. splined with 6 DF",
                       "Weekly av. splined with 10 DF",
                       "Daily av. splined with 10 DF",
                       "Hourly av. splined with 10 DF")

len <- length(list_colnames)
results <- data.frame(methodId = as.character(list_colnames), 
                      Method = as.character(list_descriptions), R2 = 1:len)
rm(list_descriptions)

# To further prepare running linear regressions on the 16 models, let's collect 
# the independent variables for all models and the common dependent variable (rating). 

buffer <- temp %>% select(list_colnames, rating) %>% as.data.frame()
rm(temp)

# Now we can run the linear regressions in a for loop.

for (i in 1:len) {
  ols <- lm(rating~buffer[, i], data = buffer)
  results[i, 3] <- as.numeric(glance(ols)[1])
}
rm(i, len, ols)

results_table <- results %>% mutate(methodId = as.character(methodId), 
                                    Method = as.character(Method)) %>% arrange(desc(R2))
selection <- results_table %>% select(Method, R2) %>% 
  rename(Time_Trend_on_edx_train = Method)
selection %>% kable() %>% kable_styling(bootstrap_options =
                          c("bordered", "condensed"), full_width = F, font_size = 12) %>%
                          column_spec(2, width = "1.5in")
rm(results)

# The highest R2 is delivered by hourly averages of ratings! Which is not surprising: 
# hourly averages are very flexible, with a number of occurrences approaching 100,000: 
# the graph hereunder illustrates it; the table hereunder quantifies it more precisely.

count_month <- nrow(temp_month)
count_week <- nrow(temp_week)
count_day <- nrow(temp_day)
count_hour <- nrow(temp_hour)

periodicity <- c("Monthly", "Weekly", "Daily", "Hourly")
number <- c(count_month, count_week, count_day, count_hour)
dummy <- data.frame(periodicity = as.character(periodicity), number = number)
rm(count_month, count_week, count_day, count_hour, periodicity, number)

graph <- dummy %>% mutate(periodicity = reorder(periodicity, number)) %>% 
  ggplot(aes(periodicity, number)) + 
  geom_bar(stat = "identity", width = 0.40, color = "#08457e", fill = "#9bc4e2") + 
  scale_y_log10() +
  ggtitle("Number of Time Periods in edx_train") +
  xlab("Periodicity") + ylab("Number of Time Periods") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(angle = 20, hjust = 1, size = 12), 
        axis.text.y =element_text(size = 12))
graph
rm(graph)

# Let's get more precise information from a table.

dummy %>% rename(Periodicity = periodicity, Number_of_Time_Periods = number) %>%
  kable() %>% kable_styling(bootstrap_options = "bordered", 
              full_width = F, font_size = 12) %>% column_spec(1:2, width = "2in")
rm(dummy)

# Comments in Recommendation_Report.Rmd and Recommendation_Report.pdf.

# Let's add a column with the standard deviations of the time trends, used as 
# a very rough estimate of the fitting degree of the corresponding time trend (correlation 
# with ratings could also be used but would seem tautological with respect to R2...).

buffer <- buffer[, results_table$methodId]
sds <- sapply(buffer, sd)
rm(buffer)

# For illustrative purposes, the standard deviation will be zeroed for the 4 series 
# of dates since they are just monotonously increasing functions of time without inflexion. 

sds <- as.data.frame(sds)
sds$sds[13:16] <- 0

# Let's visualize the results table.

results_table <- results_table %>% mutate(Fitting_Degree = sds$sds) %>% as.data.frame()
selection <- results_table %>% select(Method, R2, Fitting_Degree) %>%
  rename(Time_Trend_on_edx_train = Method)
selection %>% kable() %>% kable_styling(bootstrap_options =
                          c("bordered", "condensed"), full_width = F, font_size = 12) %>%
                          column_spec(2:3, width = "1.5in")
rm(sds, selection)

# There is a general link between R2 level and estimated fitting degree: see comments 
# in Recommendation_Report.Rmd or Recommendation_Report.pdf. 

# The main question now is: will the hourly averages of ratings not show overfitting 
# when run on edx_test? 

# Among the 16 methods, there are 2 groups: 4 lists of dates and 12 transformations 
# of edx_train ratings. The 4 lists of dates cannot be applied as such to edx_test. 
# We'll stick to the 12 transformations. Let's reduce list_colnames accordingly.

list_colnames <- list_colnames[5:16] 

# In temp_month, temp_week, temp_day and temp_hour, we already have time effects 
# on edx_train. Let's transfer them to edx_test.

edx_test <- read.csv("edx_test.csv") %>% select(timestamp, rating) %>% 
  mutate(date_month = round_date(as_datetime(timestamp), unit = "month")) %>%
  mutate(date_week = round_date(as_datetime(timestamp), unit = "week")) %>%
  mutate(date_day = round_date(as_datetime(timestamp), unit = "day")) %>%
  mutate(date_hour = round_date(as_datetime(timestamp), unit = "hour")) %>%
  select(- timestamp) %>% as.data.frame() 

edx_test <- edx_test %>% 
  left_join(temp_month, by ="date_month") %>% 
  left_join(temp_week, by ="date_week") %>% 
  left_join(temp_day, by ="date_day") %>% 
  left_join(temp_hour, by ="date_hour") %>% as.data.frame()

rm(temp_month, temp_week, temp_day, temp_hour)

# Replacing NAs by the mean of ratings in edx_train.

edx_test[is.na(edx_test)] <- mu

# Running the remaining 12 time models on edx_test in a for loop.

df <- edx_test %>% select(list_colnames)

rmses <- 1:length(list_colnames)
for (i in 1:length(rmses)) {
  rmses[i] <- RMSE(df[,i], edx_test$rating)
}
rm(i, df) 

# Building up reporting table. Comments available in Recommendation_Report.Rmd and 
# Recommendation_Report.pdf.

df <- data.frame(methodId = as.character(list_colnames), 
                 RMSE = rmses, stringsAsFactors = FALSE)
rm(list_colnames, rmses)

time_results_on_edx_test <- results_table %>% right_join(df, by ="methodId") %>% 
  arrange(RMSE) %>% as.data.frame() %>% select(Method, R2, Fitting_Degree, RMSE) %>%
  rename(Time_Trend_on_edx_train = Method, R2_on_edx_train = R2, 
         Fitting_Degree_on_edx_train = Fitting_Degree, RMSE_on_edx_test = RMSE)
time_results_on_edx_test %>% kable() %>% kable_styling(bootstrap_options = 
                             c("bordered", "condensed"), full_width = F, font_size = 12)
rm(results_table, time_results_on_edx_test, df)

# These results can be compared with the baseline model (no use of independent variables,
# just the mean of the dependent variable from edx_train, i.e. rating). In such a case, 
# the RMSE on edx_test is:

RMSE <- RMSE(edx_test$rating, mu)
rm(edx_test)

tab <- data.frame(Method_for_RMSE_on_edx_test = 
       as.character("Baseline Model (mean of edx_train ratings applied to edx_test)"), 
       RMSE_on_edx_test = RMSE) 
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                  full_width = F, font_size = 12)
rm(RMSE, tab)

# Comments available in Recommendation_Report.Rmd and Recommendation_Report.pdf.

#####################################################

# EXPLORATORY ANALYSIS OF GENRES

# Let's retrieve edx_train and edx_test.

edx_train <- as.data.frame(read.csv("edx_train.csv"))
edx_train <- edx_train %>% select(- X, - userId, - timestamp) %>% 
  mutate(title = as.character(title), genres = as.character(genres))

edx_test <- as.data.frame(read.csv("edx_test.csv"))
edx_test <- edx_test %>% select(- X, - userId, - title, - timestamp) %>% 
  mutate(genres = as.character(genres))

# How many genres in edx_train?

tab <- data.frame(Number_of_Genres_in_edx_train = n_distinct(edx_train$genres))
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                    full_width = F, font_size = 12) %>% column_spec(1, width = "3in")
rm(tab)

# LET'S VISUALIZE AVERAGES OF RATINGS PER GENRE TO ANSWER ONE QUESTION: 
# IS THE GENRES VARIABLE A GOOD CANDIDATE AS A PREDICTOR (INDEPENDENT VARIABLE)?

# For visual clarity, let's limit the graph to genres with more than 100,000 ratings.

# Means per genre will be represented as well as estimates of confidence intervals (means 
# plus or minus standard errors multiplied by 2, standard errors being the 
# standard deviations divided by the number of observations; hypothesis: means are 
# normally distributed, which is probably true except for genres with very few observations). 

graph <- edx_train %>% group_by(genres) %>% filter(n() > 100000) %>% 
  summarize(avgs = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  mutate(genres = reorder(genres, avgs)) %>%
  ggplot(aes(x = genres, y = avgs, ymin = avgs - 2*se, ymax = avgs + 2*se)) + 
  geom_point(size = 3, col = "#007ba7") +
  geom_errorbar() + 
  ggtitle("Genres in edx_train: Rating Means") +
  xlab("Genres with > 100,000 ratings") + 
  ylab("Means/Conf. Intervals") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(angle = 20, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))
graph
rm(graph)

# Averages being somewhat different, the variable genres appears as a possible candidate
# for predicting ratings on edx_test. Let's check this by calculating the RMSE on edx_test. 

avgs_genres <- edx_train %>% group_by(genres) %>% 
  summarize(genres_effect = mean(rating)) %>% as.data.frame()
temp <- edx_test %>% left_join(avgs_genres, by = "genres") 
RMSE <- RMSE(temp$genres_effect, temp$rating)
rm(avgs_genres, temp)
tab_1 <- data.frame(Model_Trained_on_edx_train_and_Tested_on_edx_test =  
                    as.character("Genres Effect Model"), RMSE_on_edx_test = RMSE)

# Results can be compared with the baseline model (no use of independent variables, just 
# the mean of ratings on edx_train). 

RMSE <- RMSE(edx_test$rating, mu)

tab_2 <- data.frame(Model_Trained_on_edx_train_and_Tested_on_edx_test = 
  as.character("Baseline Model (no independent variable, just mean(edx_train$rating)"),      RMSE_on_edx_test = RMSE)
tab <- rbind(tab_2, tab_1) %>% as.data.frame() 
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                  full_width = F, font_size = 12)
rm(RMSE)

# LET'S EXPLORE ANOTHER CONCEPT: INSTEAD OF THE GENRES VARIABLE WITH OFTEN 
# COMPOSITE GENRES ("COMEDY|ROMANCE", ETC.), LET'S SPLIT ALL COMPOSITE GENRES INTO 
# BASIC GENRES ("COMEDY", "ROMANCE").

df_basic_genres <- edx_train %>% separate_rows(genres, sep = "\\|") %>% 
  rename(basic_genres = genres)

# Which are the basic genres? How many ratings per basic genre? But, let's express a caveat: 
# each rating figures several times if the original genre is a 
# composite genre ("Comedy|Romance", etc.)! Consequently, partial overrepresentation!

tab <- df_basic_genres %>% group_by(basic_genres) %>% 
       summarize(n = n()) %>% arrange(desc(n)) 
tab %>% rename(Number_of_Ratings_in_edx_train = n) %>%
        kable() %>% kable_styling(bootstrap_options = 
        c("bordered", "condensed"), full_width = F, font_size = 12) %>%
        column_spec(1, width = "2in")
rm(tab)
                                                                                                                 
# There's a genre defined as "(no genres listed)", with 3 occurrences. Let's have a look. 

no_genres <- df_basic_genres %>% filter(basic_genres == "(no genres listed)")
no_genres %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                        full_width = F, font_size = 12) %>%
                                        column_spec(1:4, width = "1.5in")
rm(no_genres)

# Let's do some data cleaning, adding these 3 occurrences to another basic genre, 
# i.e. "Documentary". Please see Recommendation_Report.Rmd and 
# Recommendation_Report.pdf for motivation.

df_basic_genres$basic_genres[df_basic_genres$basic_genres == 
                             "(no genres listed)"] <- "Documentary"

# The last but one basic genre, i.e. IMAX, is in a different situation with more 
# than 4,000 ratings as indicated in the last but one table and more movies as mentioned 
# in the table hereunder. 

IMAX <- df_basic_genres %>% filter(basic_genres == "IMAX") %>% 
  summarize(Number_of_Movies_in_IMAX = n_distinct(movieId)) %>% as.data.frame()
IMAX %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                     full_width = F, font_size = 12) %>% column_spec(1, width = "3in")
rm(IMAX)
df_basic_genres$title <- NULL

# ARE BASIC GENRES A POTENTIAL CANDIDATE AS AN INDEPENDENT VARIABLE?

graph <- df_basic_genres %>% 
  group_by(basic_genres) %>% summarize(avgs = mean(rating), se = sd(rating)/sqrt(n())) %>%
  mutate(basic_genres = reorder(basic_genres, avgs)) %>%
  ggplot(aes(x = basic_genres, y = avgs, ymin = avgs - 2*se, ymax = avgs + 2*se)) +
  geom_point(size = 3, col = "#007ba7") +
  geom_errorbar() + 
  ggtitle("Basic Genres in edx_train: Rating Means") +
  xlab("Basic Genres") + 
  ylab("Means/Conf. Intervals") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y =element_text(size = 12))
graph
rm(graph)

# Because of some overlapping, I can't use basic genres in a direct way as the 
# variable genres. I could try regressions with numerous independent variables but
# this is beyond resources. We will just try and combine the basic genres into a smaller
# number of clusters. 

# CLUSTERS OF BASIC GENRES

# Let's first establish a link between each movie and the basic genres it belongs to. 

# From df_basic_genres, we will only keep one row for each combination of one movieId 
# with one basic genre. Let's build up a unique identifier of these combinations and
# using this identifier, let's eliminate all duplicates. 

df_basic_genres$id <- paste(df_basic_genres$movieId, df_basic_genres$basic_genres, sep="_")
df_basic_genres <- distinct(df_basic_genres, id, .keep_all = TRUE)
df_basic_genres$id <- NULL

# Let's do some wrangling with 2 objectives:
# 1. having each movie just on one row,
# 2. having each basic genre as a separate column. 

df_basic_genres <- df_basic_genres %>% spread(basic_genres, rating)

# We now have one row per movieId. We have 20 columns, one for movieId and 19 for the 
# 19 basic genres. In the 19 columns related to basic genres, we have NA each time a movie 
# does not belong to a basic genre! Let's change the NAs into 0.

df_basic_genres[is.na(df_basic_genres)] <- 0

# For a movieId, on one row, we have as many non-zero digits as the number of basic genres 
# the movie belongs to. Actually, we are only interested in having 1s instead of a rating
# simply to indicate the basic genres the movie belongs to. Before transforming 
# non-zero digits into 1s in the 19 basic genre columns, we exclude the movieId column! 
# After putting 1s, we'll include movieId again. 

temp <- df_basic_genres %>% select(- movieId)
temp[temp > 0] <- 1 

df_basic_genres <- data.frame(movieId = df_basic_genres$movieId, temp)
rm(temp)

# Now we have an additional taxonomy for movies: for each movie we can see which 
# basic genres it belongs to. Let's build up a limited number of clusters (10) 
# on the basis of basic genres. We make results reproducible with set.seed(1). 
# We run 30 iterations because of variability in the results from the kmeans() function. 

set.seed(1)
km <- kmeans(df_basic_genres[, 2:20], 10, nstart = 30)

# Analyzing the composition of clusters is beyond resources. Let's check for some evidence 
# about a cluster effect being a potential candidate for contributing to rating predictions 
# on edx_test. I am going to compute a cluster effect on edx_train, produce a graph 
# with rating means per cluster and calculate the RMSE on edx_test.  

# Let's first add the clusters identification to df_basic_genres. 

df_basic_genres <- data.frame(df_basic_genres, clusters = km$cluster)
rm(km)

# Let's just keep 2 columns: movieId and clusters.

df_clusters <- df_basic_genres %>% select(movieId, clusters)
rm(df_basic_genres)

# Let's add the variable clusters to edx_train and to edx_test.

edx_train_with_clusters <- edx_train %>% left_join(df_clusters, by = "movieId") %>%
  select(- title, - genres)

edx_test_with_clusters <- edx_test %>% left_join(df_clusters, by = "movieId") %>%
  select(- genres)
rm(edx_train, edx_test, df_clusters)

#  Let's draw a graph with rating means per cluster on edx_train. 

graph <- edx_train_with_clusters %>% group_by(clusters) %>% 
  summarize(avgs = mean(rating), se = sd(rating)/sqrt(n())) %>%
  mutate(clusters = reorder(clusters, avgs)) %>%
  ggplot(aes(x = clusters, y = avgs, ymin = avgs - 2*se, ymax = avgs + 2*se)) + 
  geom_point(size = 3, col = "#007ba7") +
  geom_errorbar() + 
  ggtitle("Clusters on edx_train: Rating Means") +
  xlab("Clusters") + 
  ylab("Means/Conf. Intervals") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Comments in Recommendation_Report.Rmd and Recommendation_Report.pdf

# Let's calculate a cluster effect on edx_train and the RMSE on edx_test.

avgs_cluster <- edx_train_with_clusters %>% group_by(clusters) %>% 
  summarize(cluster_effect = mean(rating)) 
temp <- edx_test_with_clusters %>% left_join(avgs_cluster, by = "clusters")
RMSE <- RMSE(temp$cluster_effect, temp$rating)
rm(edx_train_with_clusters, edx_test_with_clusters, temp, avgs_cluster)

tab_3 <- data.frame(Model_Trained_on_edx_train_and_Tested_on_edx_test = 
                    as.character("Cluster Effect Model"), RMSE_on_edx_test = RMSE)
rm(RMSE)

# Let's compare, in terms of RMSE, the genres effect model and the cluster effect model 
# with the baseline model.

tab <- rbind(tab_2, tab_1, tab_3)
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                  full_width = F, font_size = 12) 
rm(tab, tab_1, tab_2, tab_3)

# Conclusion of exploratory analysis of genres: see Recommendation_Report.Rmd and 
# Recommendation_Report.pdf. 

#################################################################################

# EXPLORATORY ANALYSIS OF MOVIE EFFECT

edx_train <- as.data.frame(read.csv("edx_train.csv"))
edx_train <- edx_train %>% select(userId, movieId, rating, title) %>% 
  mutate(title = as.character(title))

edx_test <- as.data.frame(read.csv("edx_test.csv"))
edx_test <- edx_test %>% select(userId, movieId, rating)

# Number of movies

tab <- data.frame(Number_of_Movies_in_edx_train = n_distinct(edx_train$movieId)) 
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                    full_width = F, font_size = 12) %>% column_spec(1, width = "3in")
rm(tab)

# Concentration of ratings per movie: some movies get many more ratings than others.

graph <- edx_train %>% count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "#007ba7", fill = "#9bc4e2") + 
  scale_x_log10() + 
  ggtitle("Concentration of Ratings per Movie") +
  xlab("Count of Ratings per Movie") +
  ylab("Count of Movies") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Table of movies with more than 10,000 ratings

tab <- edx_train %>% group_by(title) %>% filter(n() > 10000) %>% 
  summarize(Number_of_Ratings_in_edx_train = n()) %>% rename(Title = title) %>% 
  arrange(desc(Number_of_Ratings_in_edx_train)) %>% as.data.frame() 
tab %>% kable() %>% kable_styling(bootstrap_options = 
                    c("bordered", "condensed"), full_width = F, font_size = 12)
rm(tab)

# Means of ratings and confidence intervals for movies with more than 10,000 ratings

graph <- edx_train %>% group_by(movieId) %>% filter(n() > 10000) %>% 
  summarize(avgs = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  mutate(movieId = reorder(movieId, avgs)) %>%
  ggplot(aes(x = movieId, y = avgs, ymin = avgs - 2*se, ymax = avgs + 2*se)) + 
  geom_point(size = 2, col = "#007ba7") +
  geom_errorbar() +
  ggtitle("Rating Means on edx_train for Movies with > 10,000 Ratings") +
  xlab("Movies Ranked by Rating Mean") +
  ylab("Rating Means/Conf. Intervals") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12))
graph
rm(graph)

# Comments on results available in Recommendation_Report.Rmd and 
# Recommendation_Report.pdf.

######################################################################################

# EXPLORATORY ANALYSIS OF USER EFFECT

# Number of users

tab <- data.frame(Number_of_Users = n_distinct(edx_train$userId))
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                    full_width = F, font_size = 12) %>% column_spec(1, width = "3in")
rm(tab)

# Concentration of ratings per user: some users have issued many more ratings than others.

graph <- edx_train %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "#007ba7", fill = "#9bc4e2") + 
  scale_x_log10() + 
  ggtitle("Concentration of edx_train Ratings per User") +
  xlab("Count of Ratings per User") +
  ylab("Count of Users") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Table of users with more than 1,500 ratings

tab <- edx_train %>% group_by(userId) %>% filter(n() > 1500) %>% 
  summarize(Number_of_Ratings_per_User_in_edx_train = n()) %>%  
  arrange(desc(Number_of_Ratings_per_User_in_edx_train)) %>% as.data.frame() 
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                    full_width = F, font_size = 12) %>% column_spec(1, width = "2in")
rm(tab)

# Rating means and confidence intervals for users with more than 1,000 ratings

graph <- edx_train %>% group_by(userId) %>% filter(n() > 1000) %>% 
  summarize(avgs = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  mutate(userId = reorder(userId, avgs)) %>%
  ggplot(aes(x = userId, y = avgs, ymin = avgs - 2*se, ymax = avgs + 2*se)) + 
  geom_point(size = 2, col = "#007ba7") +
  geom_errorbar() +
  ggtitle("Rating Means for Users with > 1,000 Ratings") +
  xlab("Users Ranked by Rating Mean") +
  ylab("Rating Means/Conf. Intervals") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12))
graph
rm(graph, edx_train, edx_test)

# Comments available in Recommendation_Report.Rmd and Recommendation_Report.pdf

## END OF EXPLORATORY ANALYSIS, VISUALIZATION, STATISTICS, TIME SERIES ANALYSIS, 
## WRANGLING, CLUSTERING AND EFFECT PER EFFECT PRE-ANALYSIS

#################################################################################

## CLEANING USER INTERFACE FOR RAM MANAGEMENT

# Clearing plots
invisible(if(!is.null(dev.list())) dev.off())

# Cleaning workspace
rm(list=ls())

# Cleaning console
cat("\014")

#################################################################################

# GLOBAL ANALYSIS AND MODELING ON edx_train AND edx_test

# As extensively explicated and commented upon in Recommendation_Report.Rmd and 
# Recommendation_Report.pdf, three global models have been trained on edx_train and 
# tested on edx_test. All of them are 4-effect models combining time, genres, 
# movie and user effects. The rationale of these models can be found in 
# Recommendation_Report.Rmd or Recommendation_Report.pdf, as well as an analysis 
# of their results. 

# Results from these models will be provided here. Code will be given only for the third 
# model, to avoid redundancies. 


# 1st GLOBAL MODEL: TIME EFFECT (HOURLY AVERAGES), GENRES EFFECT, MOVIE EFFECT AND 
# USER EFFECT  

# The first model encompasses in this order a time effect represented by hourly averages, 
# a genres effect, a movie effect and a user effect. 

# Here are the results from the first global model, retrieved from a piece of code 
# having run separately.

myfile <- 
  "https://raw.githubusercontent.com/Dev-P-L/Recommendation-System/master/time_genres_movie_user_results.csv"
tab <- read.csv(myfile)
tab$X <- NULL
write.csv(tab, "time_genres_movie_user_results.csv")
tab <- as.data.frame(tab) %>% mutate(Method = as.character(Method)) %>% 
  rename(REJECTED_Global_Model = Method, RMSE_on_edx_test = RMSE)
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                    full_width = F, font_size = 12) %>% 
                    row_spec(c(2, 5), bold = T) %>% column_spec(2, width = "2in")
rm(myfile, tab)

# 2nd GLOBAL MODEL:  MOVIE EFFECT, USER EFFECT, GENRES EFFECT AND 
# TIME EFFECT (HOURLY AVERAGES) 

myfile <- 
  "https://raw.githubusercontent.com/Dev-P-L/Recommendation-System/master/movie_user_genres_time_results.csv"
tab <- read.csv(myfile)
tab$X <- NULL
write.csv(tab, "movie_user_genres_time_results.csv")
tab <- as.data.frame(tab) %>% select(- X) %>% mutate(Method = as.character(Method)) %>% 
  rename(REJECTED_Global_Model = Method, RMSE_on_edx_test = RMSE)
tab %>% kable() %>% kable_styling(bootstrap_options = "bordered", 
                                  full_width = F, font_size = 12) %>% 
  row_spec(c(2, 5), bold = T) %>% column_spec(2, width = "2in")
rm(myfile, tab)

# 3rd GLOBAL MODEL:  TIME EFFECT (DAILY AVERAGES), GENRES EFFECT, MOVIE EFFECT AND 
# USER EFFECT

edx_train <- read.csv("edx_train.csv") %>%
  mutate(date_day = round_date(as_datetime(timestamp), unit = "day")) %>%
  mutate(genres = as.character(genres)) %>% select(- X, - timestamp, - title) %>% 
  as.data.frame()
mu <- mean(edx_train$rating)

edx_test <- read.csv("edx_test.csv") %>%
  mutate(date_day = round_date(as_datetime(timestamp), unit = "day")) %>%
  mutate(genres = as.character(genres)) %>% select(- X, - timestamp, - title) %>% 
  as.data.frame()

# The first step is the baseline model, with no independent variable and just 
# the rating mean from edx_train to predict edx_test ratings.

model_0_rmse <- RMSE(edx_test$rating, mu)
rmse_results_0 <- data_frame(Method = 
  as.character("Baseline Model (no independent variable, just mean(edx_train$rating))"), 
  RMSE = model_0_rmse)
rm(model_0_rmse)

# Time Effect is added.

time_avgs <- edx_train %>% group_by(date_day) %>% 
  summarize(time_effect = mean(rating - mu))

predicted_ratings <- edx_test %>% left_join(time_avgs, by = "date_day") %>%  
  mutate(pred = mu + time_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results_1 <- bind_rows(rmse_results_0, data_frame(Method = 
  as.character("Time Effect Model (daily averages)"), RMSE = model_1_rmse))
rm(rmse_results_0, model_1_rmse, predicted_ratings)

# Genres Effect is added as well. 

genres_avgs <- edx_train %>% left_join(time_avgs, by = "date_day") %>% 
  group_by(genres) %>% summarize(genres_effect = mean(rating - mu - time_effect))
predicted_ratings <- edx_test %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  mutate(pred = mu + time_effect + genres_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu

model_2_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results_2 <- bind_rows(rmse_results_1, data_frame(Method = 
  as.character("Time and Genres Effects Model"), RMSE = model_2_rmse ))
rm(predicted_ratings, model_2_rmse, rmse_results_1)

# Movie Effect

movie_avgs <- edx_train %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  group_by(movieId) %>% 
  summarize(movie_effect = mean(rating - mu - time_effect - genres_effect))

predicted_ratings <- edx_test %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred = mu + time_effect + genres_effect + movie_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_3_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results_3 <- bind_rows(rmse_results_2, data_frame(Method = 
  as.character("Time Genres and Movie Effects Model"), RMSE = model_3_rmse ))
rm(predicted_ratings, model_3_rmse, rmse_results_2)

# User Effect

user_avgs <- edx_train %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>% 
  summarize(user_effect = mean(rating - mu - time_effect - genres_effect - movie_effect))

predicted_ratings <- edx_test %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + time_effect + genres_effect + movie_effect + user_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_4_rmse <- RMSE(predicted_ratings, edx_test$rating)

rmse_results_4 <- bind_rows(rmse_results_3, data_frame(Method = 
  as.character("Time Genres Movie and User Effects Model"), RMSE = model_4_rmse ))
rmse_results_4 %>% rename(Method_Run_on_edx_train_and_Tested_on_edx_test = Method,
                          RMSE_on_edx_test = RMSE) %>%
                   kable() %>% kable_styling(bootstrap_options = 
                   "bordered", full_width = F, font_size = 12) %>%
                   row_spec(5, bold = T) %>% column_spec(2, width = "2.5in")

rm(time_avgs, genres_avgs, movie_avgs, user_avgs)
rm(predicted_ratings, model_4_rmse, rmse_results_3)

# REGULARIZING OR PENALIZING WITH LAMBDA

# Comments available in Recommendation_Report.Rmd and Recommendation_Report.pdf

# Let's compute lambda by cross-validating on a sequence of values. A range between 
# 0 and 20 had first been tested (on edx_test). After getting an optimal lambda of 5, 
# a sharp shrinkage has been operated in the range (from 0-20 to 3-7) as well
# as in the increment (0.25 instead of 1). 

lambdas <- seq(3, 7, 0.25)
rmses <- sapply(lambdas, function(l){
  
  time_avgs_reg <- edx_train %>% 
    group_by(date_day) %>%
    summarize(time_effect_reg = sum(rating - mu)/(n() + l))
  
  genres_avgs_reg <- edx_train %>% 
    left_join(time_avgs_reg, by = "date_day") %>%
    group_by(genres) %>%
    summarize(genres_effect_reg = sum(rating - mu - time_effect_reg)/(n() + l))
  
  movie_avgs_reg <- edx_train %>% 
    left_join(time_avgs_reg, by = "date_day") %>%
    left_join(genres_avgs_reg, by = "genres") %>%  
    group_by(movieId) %>%
    summarize(movie_effect_reg = 
              sum(rating - mu - time_effect_reg - genres_effect_reg)/(n() + l))
  
  user_avgs_reg <- edx_train %>% 
    left_join(time_avgs_reg, by = "date_day") %>%
    left_join(genres_avgs_reg, by = "genres") %>%  
    left_join(movie_avgs_reg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(user_effect_reg = 
      sum(rating - mu - time_effect_reg - genres_effect_reg - movie_effect_reg)/(n()+l))
  
  predicted_ratings <- edx_test %>%
    left_join(time_avgs_reg, by = "date_day") %>%
    left_join(genres_avgs_reg, by = "genres") %>%  
    left_join(movie_avgs_reg, by = "movieId") %>%    
    left_join(user_avgs_reg, by = "userId") %>% 
    mutate(pred = mu + time_effect_reg + genres_effect_reg + movie_effect_reg + 
             user_effect_reg) %>% .$pred
  
  predicted_ratings[is.na(predicted_ratings)] <- mu
  RMSE(predicted_ratings, edx_test$rating)
})

df <- data.frame(lambdas = lambdas, rmses = rmses)

# Let's represent on a scatter plot the RMSEs and the corresponding lambdas. We see
# that the optimal lambda equals 5. 

graph <- df %>% 
  ggplot(aes(x = lambdas, y = rmses)) + 
  geom_point(size = 3, col = "#007ba7") +
  ggtitle("Regularization on edx_train and edx_test") +
  xlab("Lambdas") + ylab("RMSE on edx_test") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
graph

# We confirm this by retrieving the value of lambda corresponding to the minimum RMSE.

l <- lambdas[which.min(rmses)]
rm(lambdas, df, graph)

# Producing new results table 

model_5_rmse <- min(rmses)
rmse_results_5 <- bind_rows(rmse_results_4, data_frame(Method = 
  as.character("Time Genres Movie and User Effects + Regularization Model"), 
  RMSE = model_5_rmse ))
rmse_results_5 %>% rename(Accepted_Model_on_edx_train_and_edx_test = Method,
                          RMSE_on_edx_test = RMSE) %>%
                   kable() %>% kable_styling(bootstrap_options = 
                              "bordered", full_width = F, font_size = 12) %>%
                               row_spec(6, bold = T) %>% column_spec(2, width = "2.5in")
rm(model_5_rmse, rmse_results_4, rmses)

# Saving the optimal lambda for further use.

optimal_lambda_on_edx_test <- data.frame(l = l)
write.csv(optimal_lambda_on_edx_test, "optimal_lambda_on_edx_test.csv")
rm(optimal_lambda_on_edx_test)

# COERCING OUT-OF-RANGE PREDICTIONS.

# We have to run the whole regularization process with the optimal value of lambda 
# in order to access predicted_ratings and to coerce out-of-range predicted ratings. 

time_avgs_reg <- edx_train %>% 
  group_by(date_day) %>%
  summarize(time_effect_reg = sum(rating - mu)/(n() + l))

genres_avgs_reg <- edx_train %>% 
  left_join(time_avgs_reg, by = "date_day") %>%
  group_by(genres) %>%
  summarize(genres_effect_reg = sum(rating - mu - time_effect_reg)/(n() + l))

movie_avgs_reg <- edx_train %>% 
  left_join(time_avgs_reg, by = "date_day") %>%
  left_join(genres_avgs_reg, by = "genres") %>%  
  group_by(movieId) %>%
  summarize(movie_effect_reg = 
              sum(rating - mu - time_effect_reg - genres_effect_reg)/(n() + l))

user_avgs_reg <- edx_train %>% 
  left_join(time_avgs_reg, by = "date_day") %>%
  left_join(genres_avgs_reg, by = "genres") %>%  
  left_join(movie_avgs_reg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(user_effect_reg = 
    sum(rating - mu - time_effect_reg - genres_effect_reg - movie_effect_reg)/(n()+l))

predicted_ratings <- edx_test %>%
  left_join(time_avgs_reg, by = "date_day") %>%
  left_join(genres_avgs_reg, by = "genres") %>%  
  left_join(movie_avgs_reg, by = "movieId") %>%    
  left_join(user_avgs_reg, by = "userId") %>% 
  mutate(pred = mu + time_effect_reg + genres_effect_reg + movie_effect_reg + 
           user_effect_reg) %>% .$pred

predicted_ratings[is.na(predicted_ratings)] <- mu

# Coercing predicted ratings to the 0.5-5 range. 

predicted_ratings[predicted_ratings > 5] <- 5
predicted_ratings[predicted_ratings < 0.5] <- 0.5

# Producing new results table.

model_6_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results_6 <- bind_rows(rmse_results_5,
  data_frame(Method="Time Genres Movie and User Effects + Regularization + Coercion Model", 
  RMSE = model_6_rmse ))
rmse_results_6 %>% rename(Accepted_Model_on_edx_train_and_edx_test = Method,
                          RMSE_on_edx_test = RMSE) %>%
                   kable() %>% kable_styling(bootstrap_options = 
                              "bordered", full_width = F, font_size = 12) %>%
                               row_spec(7, bold = T) %>% column_spec(2, width = "2.5in")
rm(time_avgs_reg, genres_avgs_reg, movie_avgs_reg, user_avgs_reg)
rm(l, model_6_rmse, rmse_results_6, rmse_results_5, predicted_ratings)
rm(edx_train, edx_test, mu)

# END OF GLOBAL ANALYSIS AND MODELING ON edx_train AND edx_test

#################################################################################

## CLEANING USER INTERFACE FOR RAM MANAGEMENT REASONS BEFORE RUNNING FINAL MODEL ON edx 
## AND TESTING IT ON THE VALIDATION TEST

# Clearing plots
invisible(if(!is.null(dev.list())) dev.off())

# Cleaning workspace
rm(list=ls())

# Cleaning console
cat("\014")

#################################################################################

## FINAL MODEL: RUNNING ON edx AND VALIDATING ON THE ... VALIDATION SET

# Comments are available in Recommendation_Report.Rmd and Recommendation_Report.pdf.

edx <- read.csv("edx.csv")
edx <- edx %>% 
  mutate(genres = as.character(genres)) %>% 
  mutate(date_day = round_date(as_datetime(timestamp), unit = "day")) %>%
  select(- X, - title, - timestamp)  
mu <- mean(edx$rating)

val <- read.csv("validation.csv")  
val <- val %>% 
  mutate(genres = as.character(genres)) %>% 
  mutate(date_day = round_date(as_datetime(timestamp), unit = "day")) %>% 
  select(- X, - title, - timestamp)

# Model 0 : Baseline Model (no independent variable, just mean(edx$rating))
# The rating mean has been computed on edx as shown a few lines hereinabove. 

model_0_rmse <- RMSE(val$rating, mu)
rmse_results_0 <- data_frame(Method = 
  as.character("Baseline Model (no independent variable, just mean(edx$rating)"), 
  RMSE = model_0_rmse)

rm(model_0_rmse)

# Time Effect computed on edx

time_avgs <- edx %>% group_by(date_day) %>% summarize(time_effect = mean(rating - mu))

predicted_ratings <- val %>% 
  left_join(time_avgs, by = "date_day") %>%  
  mutate(pred = mu + time_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_1_rmse <- RMSE(predicted_ratings, val$rating)
rmse_results_1 <- bind_rows(rmse_results_0, data_frame(Method = 
  as.character("Time Effect (daily averages) Model"), RMSE = model_1_rmse))

rm(rmse_results_0, model_1_rmse, predicted_ratings)

# Genres Effect computed on edx

genres_avgs <- edx %>% 
  left_join(time_avgs, by = "date_day") %>% 
  group_by(genres) %>% summarize(genres_effect = mean(rating - mu - time_effect))

predicted_ratings <- val %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  mutate(pred = mu + time_effect + genres_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_2_rmse <- RMSE(predicted_ratings, val$rating)
rmse_results_2 <- bind_rows(rmse_results_1,
  data_frame(Method = as.character("Time and Genres Effects Model"), RMSE = model_2_rmse ))

rm(predicted_ratings, model_2_rmse, rmse_results_1)

# Movie Effect computed on edx

movie_avgs <- edx %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  group_by(movieId) %>% 
  summarize(movie_effect = mean(rating - mu - time_effect - genres_effect))

predicted_ratings <- val %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred = mu + time_effect + genres_effect + movie_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_3_rmse <- RMSE(predicted_ratings, val$rating)
rmse_results_3 <- bind_rows(rmse_results_2,
  data_frame(Method = as.character("Time Genres and Movie Effects Model"), 
  RMSE = model_3_rmse ))

rm(predicted_ratings, model_3_rmse, rmse_results_2)

# User Effect computed on edx

user_avgs <- edx %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>% 
  summarize(user_effect = mean(rating - mu - time_effect - genres_effect - movie_effect))

predicted_ratings <- val %>% 
  left_join(time_avgs, by = "date_day") %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + time_effect + genres_effect + movie_effect + user_effect) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu
model_4_rmse <- RMSE(predicted_ratings, val$rating)
rmse_results_4 <- bind_rows(rmse_results_3,
  data_frame(Method = as.character("Time Genres Movie and User Effects Model"), 
  RMSE = model_4_rmse ))

rm(time_avgs, genres_avgs, movie_avgs, user_avgs)
rm(predicted_ratings, model_4_rmse, rmse_results_3)

# Regularizing with the optimal lambda obtained by training the third global model 
# on edx_train and testing it on edx_test.

l <- read.csv("optimal_lambda_on_edx_test.csv")[1, 2]

time_avgs_reg <- edx %>% 
  group_by(date_day) %>%
  summarize(time_effect_reg = sum(rating - mu)/(n() + l))

genres_avgs_reg <- edx %>% 
  left_join(time_avgs_reg, by = "date_day") %>%
  group_by(genres) %>%
  summarize(genres_effect_reg = sum(rating - mu - time_effect_reg)/(n() + l))

movie_avgs_reg <- edx %>% 
  left_join(time_avgs_reg, by = "date_day") %>%
  left_join(genres_avgs_reg, by = "genres") %>%  
  group_by(movieId) %>%
  summarize(movie_effect_reg = 
            sum(rating - mu - time_effect_reg - genres_effect_reg)/(n() + l))

user_avgs_reg <- edx %>% 
  left_join(time_avgs_reg, by = "date_day") %>%
  left_join(genres_avgs_reg, by = "genres") %>%  
  left_join(movie_avgs_reg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(user_effect_reg = 
    sum(rating - mu - time_effect_reg - genres_effect_reg - movie_effect_reg)/(n()+l))

predicted_ratings <- val %>%
  left_join(time_avgs_reg, by = "date_day") %>%
  left_join(genres_avgs_reg, by = "genres") %>%  
  left_join(movie_avgs_reg, by = "movieId") %>%    
  left_join(user_avgs_reg, by = "userId") %>% 
  mutate(pred = mu + time_effect_reg + genres_effect_reg + movie_effect_reg + 
         user_effect_reg) %>% .$pred
predicted_ratings[is.na(predicted_ratings)] <- mu

model_5_rmse <- RMSE(predicted_ratings, val$rating)
rmse_results_5 <- bind_rows(rmse_results_4, data_frame(Method = 
  as.character("Time Genres Movie and User Effects + Regularization Model"), 
  RMSE = model_5_rmse ))

rm(l, model_5_rmse, rmse_results_4)

# Coercing out-of-range predicted ratings to 0.5-5 range.

predicted_ratings[predicted_ratings > 5] <- 5
predicted_ratings[predicted_ratings < 0.5] <- 0.5

##############################################################################

# FINAL RESULTS AT READERS' DISPOSAL IN WORKSPACE

model_6_rmse <- RMSE(predicted_ratings, val$rating)
rmse_results_6 <- bind_rows(rmse_results_5, data_frame(Method = 
  as.character("Time Genres Movie and User Effects + Regularization + Coercion Model"), 
  RMSE = model_6_rmse ))
rmse_results_6 %>% rename(Final_Model_on_edx_and_the_Validation_Set = Method,
                          RMSE_on_Validation_Set = RMSE) %>%
                   kable() %>% kable_styling(bootstrap_options = 
                              "bordered", full_width = F, font_size = 12) %>%
                              row_spec(7, bold = T) %>% column_spec(2, width = "2.5in")

# FOR READERS' CONVENIENCE RESULTS WILL BE STORED IN THREE OBJECTS 
# THAT WILL NOT BE REMOVED BY THIS PROGRAM.

# The final predicted ratings are in this vector:

final_predicted_ratings <- predicted_ratings

# They have also been added to the validation set. 

validation_set_with_predicted_ratings <- val %>% 
  mutate (final_predicted_ratings = final_predicted_ratings)

# Here is the final RMSE on the validation set

final_RMSE_on_validation_set <- model_6_rmse

################################################################################

# ## CLEANING USER INTERFACE EXCEPT FOR FINAL RESULTS

rm(time_avgs_reg, genres_avgs_reg, movie_avgs_reg, user_avgs_reg)
rm(model_6_rmse, rmse_results_6, rmse_results_5, predicted_ratings)
rm(edx, val, mu)

# Clearing plots
invisible(if(!is.null(dev.list())) dev.off())

# Cleaning console
cat("\014")

################################################################################

