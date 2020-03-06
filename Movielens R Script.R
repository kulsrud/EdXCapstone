if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") #For general data cleaning
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") #For creating test and train set
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org") #For displaying graphs side by side

file<- "input_data.rda"  #Raw input data previously stored as image
load(file) #To load raw input data

#The below code will work as long as the edx and validation data frames are defined in the RStudio environment. 

##-------------------------------------------------Data exploration---------------------------------------

knitr::kable(head(edx, 4), "markdown") #To show top 4 rows of sample data

head(edx) #The dataset is tidy, with each row representing one rating for movie m by user u. 
colSums((is.na(edx))) #There are no missing values in the dataset

#Transformed datestamp to date format and then to rating year and month, and weekday.
ml_df<- edx %>% mutate(rating_date = (as.Date.POSIXct(timestamp))) %>% #To transform timestamp to data format
  select(-timestamp) #removes timestamp column as the information is contained in the rating_date column

##------------------------------------------------Data Analysis------------------------------------

#look at best movies
ml_df %>%
  group_by(title) %>% #To see individual movies by name
  summarize(rating = mean(rating), n = n()) %>% #to get mean rating per movie and number of ratings
  select(title, rating, n) %>% #To select columns for displaying in table
  arrange(desc(rating)) %>% #Arranging observations so we get highest scores first
  slice(1:10) %>% #To show top 10 best movies
  knitr::kable() #To present in table

#Look at worst movies, repeats code from above but arranges with low scores first
ml_df %>%
  group_by(title) %>% #to view observations by movie title
  summarize(rating = mean(rating), n = n()) %>% #summarizing average rating per movie, and number of ratings
  select(title, rating, n) %>% #selecting columns for table visual
  arrange(desc(-rating)) %>% #arranging observations to get lowest scores first
  slice(1:10) %>%#to show bottom 10 worst movies
  knitr::kable() #to present in table

#Look at relationship between number of ratings and different variables
ml_df %>% 
  group_by(userId) %>% #want to show mean rating by user
  summarize(num_rating = n(), rating = mean(rating)) %>% #To show relationship between number of ratings and mean rating
  ggplot(aes(num_rating, rating))+ #define x and y variables for ggplot
  geom_point()+ #Create scatterplot
  labs(title = "Average rating by number of ratings, per user",
       y = "Average rating",
       x = "Number of ratings")+
  geom_smooth(method = "auto", se = TRUE)

##------------------------------------------------Estimating Model Variabes------------------------------------

mu<- mean(ml_df$rating)

#Genre effect - identify genre effect for movie u in genre g. Creating separate dataframe grouped by movies to facilitate data processing
ml_df_movies<- ml_df %>%
  group_by(movieId) %>% #group by movieId
  summarize(genres = genres[1], #all observations of a movie have the same genre, taking the first observation for each movie
            rating = mean(rating), #averaging ratings for each movie
            n = n()) #Keep only unique movieIds and retain their genre description

genres<- ml_df_movies %>%
  mutate(split_genres = str_split(genres, "\\|")) %>% #Separating each movie's different genres
  unnest(cols = split_genres) %>% #Turning genre obsrvations from list to tidy format
  group_by(split_genres) %>% #aggregating by genre
  summarize(genre_rating = sum((rating*n)/sum(n)) - mu, n = sum(n)) #estimating genre effect by subtracting each genres average rating from the dataset mean. 

genres %>%
  ggplot(aes(reorder(split_genres, genre_rating), genre_rating))+ #assigning x and y variables
  geom_bar(stat = "identity")+ #to display genre effect value, not count
  geom_hline(aes(yintercept = mean(genre_rating)))+ #to add averae guideline
  geom_label(aes(y = mean(genre_rating)+0.05, x = 7), label = "Mean genre effect")+ #to add guideline for mean genre effect
  coord_flip()+ #to turn graph on its side
  labs(title = "Genre rating by genre", #adding labels
       y = "Genre Rating",
       x = "Genre")


lambda<- 4.9 #Result from tuning (at end of script)

#Calcualte genre effect for individual movies. The for-loop follows the definition of genre effect from the course materials - it summarizes the total movie effect based on what genres are assigned to each movie. 
genre_rating<- matrix()
for (k in c(1:nrow(ml_df_movies))){
  genre_rating[k] <- matrix(sum(str_detect(ml_df_movies$genres[k], genres$split_genres)*genres$genre_rating))
}

b_g<- ml_df_movies %>% 
  cbind(genre_rating) %>% #attach genre scores back to a movieId 
  rename(b_g = genre_rating)%>% #rename to have consistent variable naming
  select(movieId, b_g) #selecting only relevant columns

#Regularized movie effect - identify b_i for each movie ID
b_i <- ml_df %>%
  left_join(b_g, by = "movieId") %>% #joining in genre effect to help estimate movie effect
  group_by(movieId) %>% #grouping by movieId to estimate movie effect
  summarize(b_i = sum(rating - mu - b_g)/(n()+lambda)) #estimating movie effect

#Regularized user effect - identify b_u for each user ID. 
b_u <- ml_df %>% 
  left_join(b_i, by="movieId") %>% #joining in genre effect to help estimate user effect
  left_join(b_g, by = "movieId") %>% #joining in movie effect to help estimate user effect
  group_by(userId) %>% #group by user ID to estimate uder effect
  summarize(b_u = sum(rating - b_i - b_g - mu)/(n()+lambda)) #estimating user effect

#Defining RMSE function
RMSE <- function(true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##---------------------------------------------------Calculating final RMSE--------------------------------------

#Validate results - joining in variables estimated based on the edx dataset, and applying them to movies in the validation set to test their prediction
predicted_ratings_validation <- 
  validation %>%
  #mutate(release_year = as.integer(str_sub(title, start = -5, end = -2))) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "movieId") %>%
  mutate(pred = mu + b_g + b_i + b_u) %>%
  .$pred

#Calculating and reporting on RMSE
validation_set_rmse <- RMSE(predicted_ratings_validation, validation$rating)
names(validation_set_rmse)<- "Regularized movie and user effects + genre effects: "
knitr::kable(validation_set_rmse, col.names = NULL)


##---------------------------------------------------Parameter Tuning----------------------------------------------

#This code was used for tuning the lambda parameter that is defined in line 80
test_index <- createDataPartition(y = ml_df$rating, times = 1, #Create index to split in test and training sets
                                  p = 0.2, list = FALSE)
train_set <- ml_df[-test_index,] #defining train set
test_set <- ml_df[test_index,] #defining test set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>% #Ensuring that all movies are present in both train and test set to avoid NA values
  semi_join(train_set, by = "userId") #Ensuring that all users are present in both train and test set to avoid NA values


mu <- mean(train_set$rating) 

#Regularized movie and user effect. individual steps explained above. - replicating model presented above for each lambda option to identify best lambda value. 
lambdas <- seq(4.5, 5.5, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    left_join(b_g, by="movieId") %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu - b_g)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_g, by="movieId") %>%
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_g - b_i)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_g, by = "movieId") %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses) #Graph showing different lambda options

min(rmses) #returning best train/ test set rmse
lambda <- lambdas[which.min(rmses)] #saving best lambda
