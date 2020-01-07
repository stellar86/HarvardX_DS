################################################################################################################################
#Author: E Rossouw
#Date: 2020-01-02
#Description: Movielens Recommendation System
################################################################################################################################

################################################################################################################################
# WARNING - THIS SCRIPT REQUIRES INTERNET ACCESS AND WILL TAKE ABOUT 45 MINS TO EXECUTE
################################################################################################################################

################################################################################################################################
# Install Required Libraries
################################################################################################################################
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggplot2)) install.packages("plotly")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(Metrics)) install.packages("Metrics")
if(!require(lubridate)) install.packages("lubridate")
if(!require(recosystem)) install.packages("recosystem")
if(!require(caret)) install.packages("caret")
if(!require(forcats)) install.packages("forcats")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(formatR)) install.packages("formatR")

################################################################################################################################
# Load Required Libraries
################################################################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)
library(Metrics)
library(lubridate)
library(recommenderlab)
library(recosystem)
library(caret)
library(forcats)
library(plotly)
library(knitr)
library(kableExtra)
library(formatR)

################################################################################################################################
# Create edx set, validation set
################################################################################################################################
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#remove some items no longer required to reduce memory footprint
rm( dl,ratings, movies, test_index, temp, movielens, removed)

################################################################################################################################
# Section 2: Data Exploration
################################################################################################################################

#Check Dataset Composition
head(edx,5) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                font_size = 10,
                full_width = FALSE) %>% footnote(general = "edx Dataset Columns Overview", footnote_as_chunk = T)

#Column Overview
col_overview <- data.frame(ColName = c("userId","movieId","rating","timestamp","title","genres"), Datatype = c("integer","numeric", "numeric","integer","character","character"), Description = c("Unique ID for the user","Unique ID for the movie","A rating between 0 and 5 for the movie"," Date and time the rating was given","Movie title (not unique)","Pipe-Seperated Genres associated with the movie"))

col_overview %>% kable() %>%
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                font_size = 10,
                full_width = FALSE) %>%   
  footnote(general = "edx Column and Datatype Overview", footnote_as_chunk = T)

#Get Dataset Compostion
edx_summary <- data.frame(Rows =nrow(edx), Cols = ncol(edx), Movies = n_distinct(edx$movieId), Users = n_distinct(edx$userId), Genres = 19)

kable(edx_summary) %>%
  kable_styling(bootstrap_options = "striped", full_width = F , position ="center") %>%
  footnote(general = "edx Dataset Overview",
           footnote_as_chunk = T)

val_summary <- data.frame(Rows =nrow(validation), Cols = ncol(validation), Movies = n_distinct(validation$movieId), Users = n_distinct(validation$userId), Genres = 19)

kable(val_summary) %>%
  kable_styling(bootstrap_options = "striped", full_width = F , position ="center") %>%
  footnote(general = "validation Dataset Overview",
           footnote_as_chunk = T)

#Check if NA's exist in data sets
edx_checkna <- sapply(edx, function(x) sum(is.na(x)))
kable(edx_checkna) %>%
  kable_styling(bootstrap_options = "striped", full_width = F , position ="center") %>%
  footnote(general = "Check NA's edx Dataset",
           footnote_as_chunk = T)

val_checkna <- sapply(validation, function(x) sum(is.na(x))) 

kable(val_checkna) %>%
  kable_styling(bootstrap_options = "striped", full_width = F , position ="center") %>%
  footnote(general = "Check NA's validation Dataset",
           footnote_as_chunk = T)
################################################################################################################################
# Section 3: Feature Engineering/Pre-Processing
################################################################################################################################
#Get Timestamp Overview
glimpse(edx)

#Convert UNIX Timestamp to Human Readbale Date
edx <- mutate(edx, date = as_datetime(timestamp))
validation <- mutate(validation, date = as_datetime(timestamp))

glimpse(edx)
glimpse(validation)

summary(edx)
summary(validation)

# Extract the year and month of rate in both dataset

edx$yearOfRate <- format(edx$date,"%Y")
edx$monthOfRate <- format(edx$date,"%m")

validation$yearOfRate <- format(validation$date,"%Y")
validation$monthOfRate <- format(validation$date,"%m")
glimpse(edx)

# Extract the year of release for each movie in both dataset

# edx dataset
edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title,c("titleTemp", "release"),regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F) %>%
  mutate(release = if_else(str_length(release) > 4, as.integer(str_split(release, "-",simplify = T)[1]),as.integer(release))
  ) %>% mutate(title = if_else(is.na(titleTemp),title,titleTemp)) %>% select(-titleTemp)

# validation dataset
validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(title,c("titleTemp", "release"),regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F) %>%
  mutate(release = if_else(str_length(release) > 4, as.integer(str_split(release, "-",simplify = T)[1]),as.integer(release))
  ) %>% mutate(title = if_else(is.na(titleTemp),title,titleTemp)) %>% select(-titleTemp)
glimpse(edx)

# Convert the columns into the desidered data type

edx$yearOfRate <- as.numeric(edx$yearOfRate)
edx$monthOfRate <- as.numeric(edx$monthOfRate)
edx$release <- as.numeric(edx$release)
validation$yearOfRate <- as.numeric(validation$yearOfRate)
validation$monthOfRate <- as.numeric(validation$monthOfRate)
validation$release <- as.numeric(validation$release)

# Remove unnecessary columns on edx and validation dataset

edx <- edx %>% select(userId, movieId, rating, title, genres, release, yearOfRate, monthOfRate)
validation <- validation %>% select(userId, movieId, rating, title, genres, release, yearOfRate, monthOfRate)

#Pre-processing results
glimpse(edx)
glimpse(validation)

################################################################################################################################
# Section 4: Data Analysis
################################################################################################################################
#Get Rating Distribution for Users
edx %>% group_by(rating) %>% summarize(n = n())  %>% ggplot(aes(rating,n)) +  geom_col() +
  ylab("Nr of Ratings") + xlab("Rating Level") + 
  theme_economist_white() +
  ggtitle("Frequency Distribution of User Ratings")

# Ratings Distribution
ratings_distribution <- edx %>%
  group_by(rating) %>% 
  summarise(ratings_distribution_sum = n()) %>%
  arrange(desc(ratings_distribution_sum))

kable(ratings_distribution) %>%
  kable_styling(bootstrap_options = "striped", full_width = F , position ="center") %>%
  footnote(general = "Nr of Ratings per Rating Category",
           footnote_as_chunk = T)

#Distribution of Movie Ratings
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "grey40", color = "white", bins = 10) +
  scale_x_log10() +  theme_economist_white() +
  ggtitle("Distribution of Movie Rating Frequency")

#Ratings of Movies per Year
edx %>% group_by(yearOfRate)  %>% summarize(n = n())   %>% ggplot(aes(yearOfRate,n)) +  geom_col() +
  ylab("Nr of Ratings") + xlab("Year") + 
  theme_economist_white() +
  ggtitle("Ratings of Movies per Year")

#Get Ratings of Movies per Month
edx %>% group_by(monthOfRate)  %>% summarize(n = n())   %>% ggplot(aes(monthOfRate,n)) +  geom_col() +
  ylab("Nr of Ratings") + xlab("Month") + 
  theme_economist_white() +
  ggtitle("Ratings of Movies per Month")

#Top 20 Rated Movies
edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=20) %>%
  ggplot(aes(title, count)) +
  theme_economist_white() +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  labs(title = "Ratings Per Title - TOP 20 Movies",
       x = "Movie",
       y = "Frequency")

#Relationship between year of release and movie rating
year_avgs <- edx%>% group_by(yearOfRate) %>% summarize(avg_rating_by_year = mean(rating)) #year the movie was rated

year_avgs %>%
  ggplot(aes(yearOfRate, avg_rating_by_year)) +
  geom_point() + theme_economist_white() + geom_smooth() +
  ggtitle("Year of Movie Rating vs Average Movie Rating")

#Calculate Additional Movie Rating Metrics
edx_movies_metrics <- edx %>%
  separate_rows(genres,
                sep = "\\|") %>%
  group_by(genres) %>%
  summarize(Ratings_Total = n(),
            Ratings_Mean = mean(rating),
            Movies_Total = n_distinct(movieId),
            Users_Total = n_distinct(userId));

#Display the genres with the most rated movies (not distinct movies)
edx_movies_metrics$Ratings_Mean <- round(edx_movies_metrics$Ratings_Mean, digits = 2)

#Ratings Distribution per Genre
edx_movies_metrics %>%
  group_by(genres) %>%
  summarise(Ratings_Total) %>%
  ggplot(aes(genres, Ratings_Total)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
labs(title = "Ratings Distribution Per Genre",
     x = "Genre",
     y = "Ratings_Total") + theme_economist_white()

#Display Summary Table
kable(edx_movies_metrics) %>%
  kable_styling(bootstrap_options = "striped", full_width = F , position ="center") %>%
  footnote(general = "Genre rating summary",
           footnote_as_chunk = T)

#Genres Mean rating
temp <- edx_movies_metrics %>%
  group_by(genres) %>%
  summarize(Ratings_Mean) %>%
  arrange(- Ratings_Mean)

temp %>%
  ggplot(aes(reorder(genres,  Ratings_Mean),  Ratings_Mean, fill=  Ratings_Mean)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Spectral") + labs(y = "Mean Rating", x = "Genre") + theme_economist_white() +
  ggtitle("Average Rating of Genres")

################################################################################################################################
# Section 5: Create Training and Testing Data Partitions for Models
################################################################################################################################
set.seed(1, sample.kind="Rounding")

#Create smaller dataset for training and testing models
edx2 <-  head(edx,1000000)
test_index <- createDataPartition(y = edx2$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx2[-test_index,]
test_set <- edx2[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

rm(test_index)
################################################################################################################################
# Section 5: Create RMSE Function for Analysis
################################################################################################################################

#RMSE Function for Analysis

RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Plot function for predicted vs real ratings
Pred_Plot <- function(true_ratings = NULL, predicted_ratings = NULL, titlex = NULL)
{
  pred_ratings <- round(predicted_ratings/0.5) *0.5
  MF_first50_pred <- data.frame(true_ratings[1:50],predicted_ratings[1:50],correct_predicted = 0);
  names(MF_first50_pred) <- c("real_ratings","predicted_ratings");
  MF_first50_pred$correct_predicted <- ifelse(MF_first50_pred$real_ratings == MF_first50_pred$predicted_ratings, 1, 0)
  
  
 pmf <- ggplot(data=MF_first50_pred, aes(x=real_ratings, y = predicted_ratings,colour=correct_predicted)) + 
    xlab("Real Ratings") + ylab("Predicted Ratings") +
    ggtitle(paste(titlex, " (Real vs Pred Ratings [50])")) +
    theme(plot.title = element_text(size = 12, color = "black", hjust = 0.5)) + 
    geom_jitter()
  
  plot(pmf)
}

################################################################################################################################
# Section 5: Base Modelling
################################################################################################################################

##################################Naive Bayse Model##################################
# Calculate the average rating of all movies in train_set dataset

mu_hat <- mean(train_set$rating)

paste("The mean is:", as.character(mu_hat))

# Predict the basic RMSE on the test_set set

naive_rmse <- RMSE(test_set$rating, mu_hat)

predictions <- rep(4.5, nrow(test_set))

#Plot Predictions

Pred_Plot(test_set$rating, mu_hat,"Mean-Baseline Model")

# Creating a results table for all RMSE results

rmse_results <- data.frame(Dataset = "Train/Test", Model="Naive Mean-Baseline Model", RMSE=naive_rmse,Accuracy = mean(predictions==test_set$rating), Lambda ="NA" )

##################################Movie Effects Model##################################

# Modelling the movie effect bias

mu <- mean(train_set$rating) 

#Determine b_i
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Compute the predicted ratings on test set

pred_movie_model <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

movie_model_rmse <- RMSE(pred_movie_model, test_set$rating)

#Plot Predictions

Pred_Plot(round(pred_movie_model), test_set$rating,"Movie Effect Model")

# Adding the results to the results dataset

rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="Movie Effect Model",
                                         RMSE = movie_model_rmse,
                                         Accuracy = mean(round(pred_movie_model/0.5)*0.5==test_set$rating)
                                         , Lambda ="NA")

##################################Movie + User Effects Model##################################

# Modelling the movie and user effect bias

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Compute the predicted ratings on test dataset

pred_user_movie_model <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


user_movie_model_rmse <- RMSE(pred_user_movie_model, test_set$rating)

#Plot Predictions

Pred_Plot(round(pred_user_movie_model), test_set$rating,"Movie+User Effect Model")

# Adding the results to the results dataset

rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Move+User Effect Model",
                                         RMSE = user_movie_model_rmse,
                                         Accuracy = mean(round(pred_user_movie_model/0.5)*0.5==test_set$rating) , Lambda ="NA")
##################################Movie + User + Year Effects Model##################################
# Modelling the movie and user + year released effect bias
year_pop <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(yearOfRate) %>%
  summarize(b_u_r = mean(rating - mu_hat - b_i - b_u))

# Compute the predicted ratings on test dataset

pred_m_u_r_model <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_pop, by='yearOfRate') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_r) %>%
  pull(pred)

m_u_r_model_rmse <- RMSE(pred_m_u_r_model, test_set$rating)

#Plot Predictions

Pred_Plot(round(pred_m_u_r_model), test_set$rating,"Mov/Usr/Year Effect Model")

# Adding the results to the results dataset

rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Movie+User+Year Effect Model",
                                         RMSE = m_u_r_model_rmse,
                                         Accuracy = mean(round(pred_m_u_r_model/0.5)*0.5==test_set$rating) , Lambda ="NA")
##################################Movie + User + Genre Effects Model##################################
# Modelling the movie and user + year + genre released effect bias
genre_pop <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_u_g = mean(rating - mu_hat - b_i - b_u ))


# Compute the predicted ratings on test dataset

pred_m_u_g_model <- test_set %>%
  
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_pop, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
  pull(pred)


m_u_g_model_rmse <- RMSE(pred_m_u_g_model, test_set$rating)

#Plot Predictions

Pred_Plot(floor(pred_m_u_g_model), test_set$rating,"Mov/Usr/Genr Effect Model")

# Adding the results to the results dataset

rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Movie+User+Genre Effect Model",
                                         RMSE = m_u_g_model_rmse,
                                         Accuracy = mean(round(pred_m_u_g_model/0.5)*0.5==test_set$rating) , Lambda ="NA")
##################################Movie + User + Year + Genre Effects Model##################################

genre_pop <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_pop, by='yearOfRate') %>%
  group_by(genres) %>%
  summarize(b_u_g = mean(rating - mu_hat - b_i - b_u - b_u_r))


# Compute the predicted ratings on test dataset

pred_m_u_g_y_model <- test_set %>%
  
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_pop, by='yearOfRate') %>%
  left_join(genre_pop, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_r + b_u_g) %>%
  pull(pred)


m_u_g_y_model_rmse <- RMSE(pred_m_u_g_y_model, test_set$rating)

#Plot Predictions

Pred_Plot(floor(pred_m_u_g_y_model), test_set$rating,"Movie+User+Genre+Year Effect Model")

# Adding the results to the results dataset

rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Movie+User+Year+Genre Effect Model",
                                         RMSE = m_u_g_y_model_rmse,
                                         Accuracy = mean(round(pred_m_u_g_y_model/0.5)*0.5==test_set$rating) , Lambda ="NA")
################################################################################################################################
# Section 5: Regularization
################################################################################################################################

##################################Movie Effects Regularization Model##################################
# Regularization logic added to model grouped by movie id. 
# Cross test_set is used to determine the minimum lambda to use for the model.

lambda_mov <- seq(0, 15, 0.1)

# Compute the predicted ratings on test_set dataset using different values of lambda

just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses_movie <- sapply(lambda_mov, function(l){
  mu <- mean(train_set$rating)
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

#Plot the lambda tuning parameter vs rmse
qplot(lambda_mov, rmses_movie) + ggtitle("Lambda VS RMSE") + theme_economist_white()

# Get the lambda value that minimize the RMSE
min_lambda_movie <- lambda_mov[which.min(rmses_movie)]

#min_lambda_movie = 3.6

# Predict the RMSE on the test_set set
regularized_movie_model_rmse <- min(rmses_movie)

predicted_ratings <- test_set %>% 
  left_join(just_the_sum, by='movieId') %>% 
  mutate(b_i = s/(n_i+min_lambda_movie)) %>%
  mutate(pred = mu + b_i) %>%
  .$pred

#Plot Predictions
Pred_Plot(round(predicted_ratings), test_set$rating,"Reg Movie Effect Model")

# Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Regularized Movie Effect Model", RMSE=regularized_movie_model_rmse,
                                         Accuracy = mean(round(predicted_ratings/0.5)*0.5==test_set$rating) , Lambda = as.character(min_lambda_movie))

##################################Movie + User Effects Regularization Model##################################                                     
# Regularization logic added to model grouped by movie id and user id. 
# Cross test_set is used to determine the minimum lambda to use for the model

lambda_movuser<- seq(0, 15, 0.1)

rmses_user_movie <- sapply(lambda_movuser, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

#Plot the lambda tuning parameter vs rmse
qplot(lambda_movuser, rmses_user_movie) + ggtitle("Lambda VS RMSE") + theme_economist_white()

# Get the lambda value that minimize the RMSE
min_lambda_movie_user <- lambda_movuser[which.min(rmses_user_movie)]

#min_lambda_movie_user = 7

# Predict the RMSE on the test_set set
regularized_user_movie_model_rmse <- min(rmses_user_movie)

#Calculate Predicted Ratings from Min Lambda
mu <- mean(train_set$rating)
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+min_lambda_movie_user))
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+min_lambda_movie_user))
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#Plot Predictions
Pred_Plot(round(predicted_ratings), test_set$rating,"Reg Movie+User Effect Model")

# Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Regularized Movie+User Effect Model", RMSE=regularized_user_movie_model_rmse,
                                         Accuracy = mean(round(predicted_ratings/0.5)*0.5==test_set$rating), Lambda = as.character(min_lambda_movie_user))

##################################Movie + User + Year Effects Regularization Model##################################
# Regularization logic added to model grouped by movie id and user id and year rated. 
# Cross test_set is used to determine the minimum lambda to use for the model

lambda_movuseryear <- seq(0, 15, 0.1)

# Compute the predicted ratings on test_set dataset using different values of lambda

rmses_user_movie_year <- sapply(lambda_movuseryear, function(l) {
  mu <- mean(train_set$rating)
  # Calculate the average by user
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # Calculate the average by user
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_u_r <- train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(yearOfRate) %>%
    summarize(b_u_r = sum(rating - b_i - mu_hat - b_u) / (n() + l))
  
  # Compute the predicted ratings on test_set dataset
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_u_r, by='yearOfRate') %>%
    mutate(pred = mu_hat + b_i + b_u + b_u_r) %>%
    .$pred
  
  # Predict the RMSE on the test set
  return(RMSE(predicted_ratings, test_set$rating))
  
  
})

#Plot the lambda tuning parameter vs rmse
qplot(lambda_movuseryear, rmses_user_movie_year) + ggtitle("Lambda VS RMSE") + theme_economist_white()


# Get the lambda value that minimize the RMSE

min_lambda_movie_user_year <- lambda_movuseryear[which.min(rmses_user_movie_year)]

#min_lambda_movie_user_genre = 4.9

# Predict the RMSE on the test_set set

regularized_user_movie_year_model_rmse <- min(rmses_user_movie_year)

#Calculate Predicted Ratings from Min Lambda
mu <- mean(train_set$rating)
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+min_lambda_movie_user_year))

# Calculate the average by user
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+min_lambda_movie_user_year))

b_u_r <- train_set %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(yearOfRate) %>%
  summarize(b_u_r = sum(rating - b_i - mu_hat - b_u) / (n() + min_lambda_movie_user_year))

# Compute the predicted ratings on test_set dataset
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_u_r, by='yearOfRate') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_r) %>%
  .$pred

#Plot Predictions
Pred_Plot(round(predicted_ratings), test_set$rating,"Reg Movie+User+Year Effect Model")

# Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Regularized Movie+User+Year Effect Model", RMSE=regularized_user_movie_year_model_rmse,
                                         Accuracy = mean(round(predicted_ratings/0.5)*0.5==test_set$rating), Lambda = as.character(min_lambda_movie_user_year))

##################################Movie + User + Year + Genre Effects Regularization Model##################################
# Regularization logic added to model grouped by movie id and user id and and year rated and genre. 
# Cross test_set is used to determine the minimum lambda to use for the model

lambda_movusergenre <- seq(0, 15, 0.1)

# Compute the predicted ratings on test_set dataset using different values of lambda

rmses_user_movie_genre <- sapply(lambda_movusergenre, function(l) {
  mu <- mean(train_set$rating)
  # Calculate the average by user
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  # Calculate the average by user
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_u_r <- train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(yearOfRate) %>%
    summarize(b_u_r = sum(rating - b_i - mu_hat - b_u) / (n() + l))
  
  b_u_g <- train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_u_r, by='yearOfRate') %>%
    group_by(genres) %>%
    summarize(b_u_g = sum(rating - b_i - mu_hat - b_u - b_u_r) / (n() + l))
  
  # Compute the predicted ratings on test_set dataset
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_u_r, by='yearOfRate') %>%
    left_join(b_u_g, by='genres') %>%
    mutate(pred = mu_hat + b_i + b_u + b_u_r + b_u_g) %>%
    .$pred
  
  # Predict the RMSE on the test set
  return(RMSE(predicted_ratings, test_set$rating))
  
  
})

#Plot the lambda tuning parameter vs rmse
qplot(lambda_movusergenre, rmses_user_movie_genre) + ggtitle("Lambda VS RMSE") + theme_economist_white()

# Get the lambda value that minimize the RMSE

min_lambda_movie_user_genre <- lambda_movusergenre[which.min(rmses_user_movie_genre)]

#min_lambda_movie_user_genre = 4.9

# Predict the RMSE on the test_set set

regularized_user_movie_genre_model_rmse <- min(rmses_user_movie_genre)

#Calculate Predicted Ratings from Min Lambda
mu <- mean(train_set$rating)
# Calculate the average by user
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+min_lambda_movie_user_genre))

# Calculate the average by user
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+min_lambda_movie_user_genre))

b_u_r <- train_set %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(yearOfRate) %>%
  summarize(b_u_r = sum(rating - b_i - mu_hat - b_u) / (n() + min_lambda_movie_user_genre))

b_u_g <- train_set %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_u_r, by='yearOfRate') %>%
  group_by(genres) %>%
  summarize(b_u_g = sum(rating - b_i - mu_hat - b_u - b_u_r) / (n() + min_lambda_movie_user_genre))

# Compute the predicted ratings on test_set dataset
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_u_r, by='yearOfRate') %>%
  left_join(b_u_g, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_r + b_u_g) %>%
  .$pred

#Plot Predictions
Pred_Plot(round(predicted_ratings), test_set$rating,"Reg Movie+User+Year+Genre Effect Model")

# Adding the results to the results dataset
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test",Model="Regularized Movie+User+Year+Genre Effect Model", RMSE=regularized_user_movie_genre_model_rmse,
                                         Accuracy = mean(round(predicted_ratings/0.5)*0.5==test_set$rating),Lambda = as.character(min_lambda_movie_user_genre))

################################################################################################################################
# Section 5: Results on Validation Set
################################################################################################################################

min_lambda_val_set <- 3.5

#Calculate Predicted Ratings from Min Lambda
mu <- mean(edx$rating)
# Calculate the average by user
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+min_lambda_val_set))

# Calculate the average by user
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+min_lambda_val_set))

b_u_r <- edx %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(yearOfRate) %>%
  summarize(b_u_r = sum(rating - b_i - mu_hat - b_u) / (n() + min_lambda_val_set))

b_u_g <- edx %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_u_r, by='yearOfRate') %>%
  group_by(genres) %>%
  summarize(b_u_g = sum(rating - b_i - mu_hat - b_u - b_u_r) / (n() + min_lambda_val_set))

# Compute the predicted ratings on test_set dataset
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_u_r, by='yearOfRate') %>%
  left_join(b_u_g, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_r + b_u_g) %>%
  .$pred

rmse_val_res <- RMSE(predicted_ratings, validation$rating)

#Plot Predictions
Pred_Plot(round(predicted_ratings), validation$rating,"Regularized Model 1")

rmse_results <- rmse_results %>% add_row(Dataset = "edx/validation", Model="Regularized Movie+User+Year+Genre Effect Model", RMSE=rmse_val_res,
                                         Accuracy = mean(round(predicted_ratings/0.5)*0.5==validation$rating), Lambda = as.character(min_lambda_val_set))
rmse_results %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                font_size = 10,
                full_width = FALSE) %>%
  footnote(general = "RMSE's for Model 1",
           footnote_as_chunk = T)

##############################################################################################
#Section 5: Matrix Factorization with parallel stochastic gradient descent
##############################################################################################

#Create algorithm datasets
edx_fact <- edx %>% select(movieId,userId,rating) 
validation_fact <- validation %>% select(movieId,userId,rating) 
#Convert to Matrix
edx_fact <- as.matrix(edx_fact)
validation_fact <- as.matrix(validation_fact)

#write to disk
write.table(edx_fact , file = "trainingset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_fact, file = "validationset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

#Initialize train and test sets
set.seed(1, sample.kind="Rounding")
training_dataset <- data_file( "trainingset.txt")
validation_dataset <- data_file( "validationset.txt")

#Create Reco Object
r = Reco()

#Create Tuning Parameters
opts = r$tune(training_dataset, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                            costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 10))

#Train Model
r$train(training_dataset, opts = c(opts$min, nthread = 1, niter = 20))

# write predictions to a tempfile on HDisk

stored_prediction = tempfile()

#Run Prediction
r$predict(validation_dataset, out_file(stored_prediction))  
real_ratings <- read.table("validationset.txt", header = FALSE, sep = " ")$V3
pred_ratings <- scan(stored_prediction)

#Determine RMSE
rmse_of_model_mf <- RMSE(pred_ratings,real_ratings)
rmse_results <- rmse_results %>% add_row(Dataset = "edx/validation", Model="Matrix Factorization with Parallel Stochastic Gradient Descent", RMSE=rmse_of_model_mf,
                                         Accuracy = mean(round(pred_ratings/0.5)*0.5==validation$rating), Lambda = "NA")
rmse_results %>% filter (RMSE <= 0.8) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                font_size = 10,
                full_width = FALSE) %>%
  footnote(general = "RMSE for Model 2",
           footnote_as_chunk = T)

#Compare Predictions
pred_ratings_rounded <-  pred_ratings
pred_ratings_rounded <- round(pred_ratings_rounded/0.5) *0.5

#Plot Predictions
Pred_Plot(pred_ratings_rounded, real_ratings,"Matrix Fact with SGD")

##############################################################################################
#Section 6: Results
##############################################################################################
#Print Final RMSE Results
rmse_results %>% filter (RMSE <= 0.8649) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"),
                position = "center",
                font_size = 10,
                full_width = FALSE) %>%
  footnote(general = "Final Results",
           footnote_as_chunk = T)
##############################################################################################
#Section 9: Environmental Variables
##############################################################################################
#Print OS
print("Operating System:")
version

#Print Installed Packages
print("All installed packages")
installed.packages()


