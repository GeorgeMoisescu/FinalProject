if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# exploring the structure of data

str(edx)
# the edx set has 9000055 observations and 6 variables
# the variables are userId (int type), movieId(int type),rating (numeric type), timestamp (int type), title (character type), genres (character type)
# the genres variable contains multiples values in the same observation so I make one variable for each genre. 

head(edx)

max(str_count(edx$genres, "\\|"))
edx_tidy <- edx %>% separate(genres,c("1_genre","2_genre","3_genre","4_genre","5_genre","6_genre","7_genre","8_genre"),"\\|")

individual_genre <- unique(c(unique(edx_tidy$`1_genre`),unique(edx_tidy$`2_genre`),unique(edx_tidy$`3_genre`),unique(edx_tidy$`4_genre`),unique(edx_tidy$`5_genre`),unique(edx_tidy$`6_genre`),unique(edx_tidy$`7_genre`),unique(edx_tidy$`8_genre`)))
individual_genre <-individual_genre[order(individual_genre)]
individual_genre

# there are 20 individual genres in the genre variable

edx_tidy <- edx_tidy %>% mutate(Action = 0,Adventure = 0,Animation = 0,Children = 0,Comedy = 0,Crime = 0,Documentary = 0,Drama = 0,Fantasy = 0,Film_Noir = 0,Horror = 0,IMAX = 0,Musical = 0,Mystery = 0,Romance = 0,Sci_Fi = 0,Thriller = 0,War = 0, Western = 0,No_Genre = 0)
head(edx_tidy)


# populating the genre variables with the corresponding values (1 if the movie belong to that genre, 0 if not)

temp <- edx_tidy$`1_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`1_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`1_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`1_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`1_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`1_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`1_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`1_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`1_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`1_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`1_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`1_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`1_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`1_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`1_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`1_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`1_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`1_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`1_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`1_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1




temp <- edx_tidy$`2_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`2_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`2_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`2_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`2_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`2_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`2_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`2_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`2_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`2_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`2_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`2_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`2_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`2_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`2_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`2_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`2_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`2_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`2_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`2_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1




temp <- edx_tidy$`3_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`3_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`3_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`3_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`3_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`3_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`3_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`3_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`3_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`3_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`3_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`3_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`3_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`3_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`3_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`3_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`3_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`3_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`3_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`3_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1



temp <- edx_tidy$`4_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`4_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`4_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`4_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`4_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`4_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`4_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`4_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`4_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`4_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`4_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`4_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`4_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`4_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`4_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`4_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`4_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`4_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`4_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`4_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1



temp <- edx_tidy$`5_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`5_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`5_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`5_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`5_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`5_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`5_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`5_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`5_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`5_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`5_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`5_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`5_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`5_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`5_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`5_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`5_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`5_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`5_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`5_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1



temp <- edx_tidy$`6_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`6_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`6_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`6_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`6_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`6_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`6_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`6_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`6_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`6_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`6_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`6_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`6_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`6_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`6_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`6_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`6_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`6_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`6_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`6_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1


temp <- edx_tidy$`7_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`7_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`7_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`7_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`7_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`7_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`7_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`7_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`7_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`7_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`7_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`7_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`7_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`7_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`7_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`7_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`7_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`7_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`7_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`7_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1



temp <- edx_tidy$`8_genre` == "Action"
edx_tidy$Action[temp] <- 1

temp <- edx_tidy$`8_genre` == "Adventure"
edx_tidy$Adventure[temp] <- 1

temp <- edx_tidy$`8_genre` == "Animation"
edx_tidy$Animation[temp] <- 1

temp <- edx_tidy$`8_genre` == "Children"
edx_tidy$Children[temp] <- 1

temp <- edx_tidy$`8_genre` == "Comedy"
edx_tidy$Comedy[temp] <- 1

temp <- edx_tidy$`8_genre` == "Crime"
edx_tidy$Crime[temp] <- 1

temp <- edx_tidy$`8_genre` == "Documentary"
edx_tidy$Documentary[temp] <- 1

temp <- edx_tidy$`8_genre` == "Drama"
edx_tidy$Drama[temp] <- 1

temp <- edx_tidy$`8_genre` == "Fantasy"
edx_tidy$Fantasy[temp] <- 1

temp <- edx_tidy$`8_genre` == "Film-Noir"
edx_tidy$Film_Noir[temp] <- 1

temp <- edx_tidy$`8_genre` == "Horror"
edx_tidy$Horror[temp] <- 1

temp <- edx_tidy$`8_genre` == "IMAX"
edx_tidy$IMAX[temp] <- 1

temp <- edx_tidy$`8_genre` == "Musical"
edx_tidy$Musical[temp] <- 1

temp <- edx_tidy$`8_genre` == "Mystery"
edx_tidy$Mystery[temp] <- 1

temp <- edx_tidy$`8_genre` == "Romance"
edx_tidy$Romance[temp] <- 1

temp <- edx_tidy$`8_genre` == "Sci-Fi"
edx_tidy$Sci_Fi[temp] <- 1

temp <- edx_tidy$`8_genre` == "Thriller"
edx_tidy$Thriller[temp] <- 1

temp <- edx_tidy$`8_genre` == "War"
edx_tidy$War[temp] <- 1

temp <- edx_tidy$`8_genre` == "Western"
edx_tidy$Western[temp] <- 1

temp <- edx_tidy$`8_genre` == "(no genres listed)"
edx_tidy$No_Genre[temp] <- 1

edx_tidy <- edx_tidy[,-c(6:13)]

# here i extract the year of the rating from the timestamp variable.

edx_tidy <- edx_tidy%>% mutate(rated_year = year(as_datetime(edx_tidy$timestamp)))
edx_tidy <- edx_tidy[,-4]
edx_tidy <- edx_tidy[,c(1:3,25,4:24)]
edx_tidy


# exploratory data analysis

genre_sum <- rep(0,20)
genre_rating <- rep(0,20)


  genre_sum[1] <- sum(edx_tidy$Action)
  filtered_data <- edx_tidy %>% filter(Action == 1)
  genre_rating[1] <- round(mean(filtered_data$rating),digits = 2)

  genre_sum[2] <- sum(edx_tidy$Adventure)
  filtered_data <- edx_tidy %>% filter(Adventure == 1)
  genre_rating[2] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[3] <- sum(edx_tidy$Animation)
  filtered_data <- edx_tidy %>% filter(Animation == 1)
  genre_rating[3] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[4] <- sum(edx_tidy$Children)
  filtered_data <- edx_tidy %>% filter(Children == 1)
  genre_rating[4] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[5] <- sum(edx_tidy$Comedy)
  filtered_data <- edx_tidy %>% filter(Comedy == 1)
  genre_rating[5] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[6] <- sum(edx_tidy$Crime)
  filtered_data <- edx_tidy %>% filter(Crime == 1)
  genre_rating[6] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[7] <- sum(edx_tidy$Documentary)
  filtered_data <- edx_tidy %>% filter(Documentary == 1)
  genre_rating[7] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[8] <- sum(edx_tidy$Drama)
  filtered_data <- edx_tidy %>% filter(Drama == 1)
  genre_rating[8] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[9] <- sum(edx_tidy$Fantasy)
  filtered_data <- edx_tidy %>% filter(Fantasy == 1)
  genre_rating[9] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[10] <- sum(edx_tidy$Film_Noir)
  filtered_data <- edx_tidy %>% filter(Film_Noir == 1)
  genre_rating[10] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[11] <- sum(edx_tidy$Horror)
  filtered_data <- edx_tidy %>% filter(Horror == 1)
  genre_rating[11] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[12] <- sum(edx_tidy$IMAX)
  filtered_data <- edx_tidy %>% filter(IMAX == 1)
  genre_rating[12] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[13] <- sum(edx_tidy$Musical)
  filtered_data <- edx_tidy %>% filter(Musical == 1)
  genre_rating[13] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[14] <- sum(edx_tidy$Mystery)
  filtered_data <- edx_tidy %>% filter(Mystery == 1)
  genre_rating[14] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[15] <- sum(edx_tidy$Romance)
  filtered_data <- edx_tidy %>% filter(Romance == 1)
  genre_rating[15] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[16] <- sum(edx_tidy$Sci_Fi)
  filtered_data <- edx_tidy %>% filter(Sci_Fi == 1)
  genre_rating[16] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[17] <- sum(edx_tidy$Thriller)
  filtered_data <- edx_tidy %>% filter(Thriller == 1)
  genre_rating[17] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[18] <- sum(edx_tidy$War)
  filtered_data <- edx_tidy %>% filter(War == 1)
  genre_rating[18] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[19] <- sum(edx_tidy$Western)
  filtered_data <- edx_tidy %>% filter(Western == 1)
  genre_rating[19] <- round(mean(filtered_data$rating),digits = 2)
  
  genre_sum[20] <- sum(edx_tidy$No_Genre)
  filtered_data <- edx_tidy %>% filter(No_Genre == 1)
  genre_rating[20] <- round(mean(filtered_data$rating),digits = 2)
  
  
genre_sum
genre_rating

genre_data <- data.frame(genre = colnames(edx_tidy[,-c(1:5)]),no_of_ratings = genre_sum,rating_mean = genre_rating)

genre_data <- genre_data[order(genre_data$no_of_ratings,decreasing = TRUE),]
genre_data
genre_data %>% ggplot(aes(genre,no_of_ratings)) + geom_point(size = 3, color = "blue") +
  theme(axis.text.x = element_text(angle = 90, face = "bold",vjust = 0.5,hjust = 1)) +
  ggtitle("Variation of number of ratings with genre category")

# we see that Drama and Comedy are the most rated movies fallowed by Action and Thriller

genre_data[order(genre_data$rating_mean,decreasing = TRUE),]
genre_data %>% ggplot(aes(genre,rating_mean)) + geom_point(size = 3, color = "blue") +
  theme(axis.text.x = element_text(angle = 90, face = "bold",vjust = 0.5,hjust = 1)) +
  ggtitle("Variation of average ratings with genre category")

# the average rating for Film_Noir is 4.01 the most hight and the rest genres have an average between 3.78 and 3.27
# the Film_Noir genre has an average of 4.01 but is ranked 17th out of 20 in term of numbers of ratings

year_data <- edx_tidy %>% group_by(rated_year) %>% summarize(no_of_ratings = n(),average_rating = mean(rating))
year_data[order(year_data$no_of_ratings,decreasing = TRUE),]
year_data %>% ggplot(aes(factor(rated_year),no_of_ratings)) + geom_point(size = 3, color = "blue") +
  theme(axis.text.x = element_text(angle = 90, face = "bold",vjust = 0.5,hjust = 1)) +
  xlab("rated_year") +
  ggtitle("Variation of number of ratings with year of ratings")

#we can see that the year 2000 and 2005 have the most number of ratings.


movieId_data <- edx_tidy %>% group_by(movieId) %>% summarize(no_of_ratings = n(),average_rating = mean(rating))
movieId_data <- left_join(movieId_data,unique(edx_tidy %>% select(movieId,title)),by = "movieId")

movieId_data <- movieId_data[order(movieId_data$no_of_ratings,decreasing = TRUE),]
movieId_data

# the most 10 rated movies in the data frame with their average rating.

movieId_data %>% ggplot(aes(no_of_ratings)) +
  geom_histogram(bins = 30) + 
  scale_x_log10() +
  ggtitle("The distribution of number of ratings for all movies")

# the distribution of numbers of ratings of all the movies. We can see that there are movies that are more rated than other movies.

movieId_data %>% ggplot(aes(no_of_ratings,average_rating)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method='lm') +
  ggtitle("Variation of average rating with number of ratings of movies")

# we can see from this plot there is a positive influence of numbers of ratings of movies on average rating of movies
# the slope of linear regression line is very small


userId_data <- edx_tidy %>% group_by(userId) %>% summarize(no_of_ratings = n(),average_rating = mean(rating))
userId_data <- userId_data[order(userId_data$no_of_ratings,decreasing = TRUE),]
userId_data

userId_data %>% ggplot(aes(no_of_ratings)) +
  geom_histogram(bins = 30) +
  scale_x_log10() +
  ggtitle("Distribution of number of ratings for all users")

# the distribution of numbers of ratings of all the users. we can see that there are user more active than other users.

userId_data %>% ggplot(aes(no_of_ratings,average_rating)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = 'lm') +
  ggtitle("Variation of average rating with number of ratings given by users")

# we can see from this plot the influence of numbers of ratings of users on average rating of users
# the slope of linear regression line is very small


# Creating the algorithm using edx dataset for predicting ratings. 

# spliting the data in train and test partitions

index <- createDataPartition(edx_tidy$rating,p = 0.8,list = FALSE)
edx_tidy_train <- edx_tidy[index,]
edx_tidy_test <- edx_tidy[-index,]

edx_tidy_test <- edx_tidy_test %>%
  semi_join(edx_tidy_train, by = "movieId") %>%
  semi_join(edx_tidy_train, by = "userId")



RMSE <- function(data_rating,predicted_rating)
{
  sqrt(mean((data_rating - predicted_rating)^2))
}

# predicted rating is the sum of average rating and movies biases,users biases, year biases and genre biases

avg_rating <- mean(edx_tidy_train$rating)
avg_rating

movie_effect <- edx_tidy_train %>% group_by(movieId) %>% summarize(b_i = sum(rating-avg_rating)/(n()+5))
movie_effect

edx_tidy_train <- edx_tidy_train %>% left_join(movie_effect, by = 'movieId')
edx_tidy_train

edx_tidy_test <- left_join(edx_tidy_test,movie_effect, by = 'movieId') 
edx_tidy_test

user_effect <- edx_tidy_train %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - avg_rating)/(n()+5))
user_effect

edx_tidy_train <- edx_tidy_train %>% left_join(user_effect, by = 'userId')
edx_tidy_train

edx_tidy_test <- left_join(edx_tidy_test,user_effect,by = 'userId')
edx_tidy_test

year_effect <- edx_tidy_train %>% group_by(rated_year) %>% summarize(b_y = sum(rating - b_i - b_u - avg_rating)/(n()+5))
year_effect

edx_tidy_train <- edx_tidy_train %>% left_join(year_effect, by = 'rated_year')
edx_tidy_train

edx_tidy_test <- left_join(edx_tidy_test,year_effect,by = 'rated_year')
edx_tidy_test



# calculating the genre effect 

edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = 0)
edx_tidy_test <- edx_tidy_test %>% mutate(b_genre = 0)



  
  filtered_data <- edx_tidy_train %>% filter(Action == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Action == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Action == 1,genre_bias_value,0) + b_genre)
  

  filtered_data <- edx_tidy_train %>% filter(Adventure == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Adventure == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Adventure == 1,genre_bias_value,0) + b_genre)


  filtered_data <- edx_tidy_train %>% filter(Animation == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Animation == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Animation == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Children == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Children == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Children == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Comedy == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Comedy == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Comedy == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Crime == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Crime == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Crime == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Documentary == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Documentary == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Documentary == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Drama == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Drama == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Drama == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Fantasy == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Fantasy == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Fantasy == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Film_Noir == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Film_Noir == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Film_Noir == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Horror == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Horror == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Horror == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(IMAX == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(IMAX == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(IMAX == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Musical == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Musical == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Musical == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Mystery == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Mystery == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Mystery == 1,genre_bias_value,0) + b_genre)
  
  
  
  filtered_data <- edx_tidy_train %>% filter(Romance == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Romance == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Romance == 1,genre_bias_value,0) + b_genre)
  
 
  filtered_data <- edx_tidy_train %>% filter(Sci_Fi == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Sci_Fi == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Sci_Fi == 1,genre_bias_value,0) + b_genre)
  
  
  
  filtered_data <- edx_tidy_train %>% filter(Thriller == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Thriller == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Thriller == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(War == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(War == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(War == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(Western == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(Western == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(Western == 1,genre_bias_value,0) + b_genre)
  
  
  filtered_data <- edx_tidy_train %>% filter(No_Genre == 1)
  
  effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating - filtered_data$b_genre
  
  genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)
  
  edx_tidy_train <- edx_tidy_train %>% mutate(b_genre = ifelse(No_Genre == 1,genre_bias_value,0)+b_genre)
  
  edx_tidy_test <-edx_tidy_test %>% mutate(b_genre = ifelse(No_Genre == 1,genre_bias_value,0) + b_genre)
  
  
  

pred_rating <- avg_rating + edx_tidy_test$b_i + edx_tidy_test$b_u +edx_tidy_test$b_y + edx_tidy_test$b_genre



RMSE(edx_tidy_test$rating,pred_rating) 


# making the validation data set tidy

max(str_count(validation$genres, "\\|"))
validation_tidy <- validation %>% separate(genres,c("1_genre","2_genre","3_genre","4_genre","5_genre","6_genre","7_genre","8_genre"),"\\|")
head(validation_tidy)
individual_genre <- unique(c(unique(validation_tidy$`1_genre`),unique(validation_tidy$`2_genre`),unique(validation_tidy$`3_genre`),unique(validation_tidy$`4_genre`),unique(validation_tidy$`5_genre`),unique(validation_tidy$`6_genre`),unique(validation_tidy$`7_genre`),unique(validation_tidy$`8_genre`)))
individual_genre <-individual_genre[order(individual_genre)]
individual_genre
validation_tidy <- validation_tidy %>% mutate(Action = 0,Adventure = 0,Animation = 0,Children = 0,Comedy = 0,Crime = 0,Documentary = 0,Drama = 0,Fantasy = 0,Film_Noir = 0,Horror = 0,IMAX = 0,Musical = 0,Mystery = 0,Romance = 0,Sci_Fi = 0,Thriller = 0,War = 0, Western = 0,No_Genre = 0)
head(validation_tidy)

# populating the genre variables with the corresponding values (1 if the movie belong to that genre, 0 if not)

temp <- validation_tidy$`1_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`1_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`1_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`1_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`1_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`1_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`1_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`1_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`1_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`1_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`1_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`1_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`1_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`1_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`1_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`1_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`1_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`1_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`1_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`1_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1




temp <- validation_tidy$`2_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`2_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`2_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`2_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`2_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`2_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`2_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`2_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`2_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`2_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`2_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`2_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`2_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`2_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`2_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`2_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`2_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`2_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`2_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`2_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1




temp <- validation_tidy$`3_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`3_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`3_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`3_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`3_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`3_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`3_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`3_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`3_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`3_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`3_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`3_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`3_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`3_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`3_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`3_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`3_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`3_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`3_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`3_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1



temp <- validation_tidy$`4_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`4_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`4_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`4_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`4_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`4_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`4_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`4_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`4_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`4_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`4_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`4_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`4_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`4_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`4_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`4_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`4_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`4_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`4_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`4_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1



temp <- validation_tidy$`5_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`5_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`5_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`5_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`5_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`5_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`5_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`5_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`5_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`5_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`5_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`5_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`5_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`5_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`5_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`5_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`5_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`5_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`5_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`5_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1



temp <- validation_tidy$`6_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`6_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`6_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`6_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`6_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`6_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`6_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`6_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`6_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`6_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`6_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`6_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`6_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`6_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`6_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`6_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`6_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`6_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`6_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`6_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1


temp <- validation_tidy$`7_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`7_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`7_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`7_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`7_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`7_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`7_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`7_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`7_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`7_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`7_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`7_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`7_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`7_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`7_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`7_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`7_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`7_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`7_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`7_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1



temp <- validation_tidy$`8_genre` == "Action"
validation_tidy$Action[temp] <- 1

temp <- validation_tidy$`8_genre` == "Adventure"
validation_tidy$Adventure[temp] <- 1

temp <- validation_tidy$`8_genre` == "Animation"
validation_tidy$Animation[temp] <- 1

temp <- validation_tidy$`8_genre` == "Children"
validation_tidy$Children[temp] <- 1

temp <- validation_tidy$`8_genre` == "Comedy"
validation_tidy$Comedy[temp] <- 1

temp <- validation_tidy$`8_genre` == "Crime"
validation_tidy$Crime[temp] <- 1

temp <- validation_tidy$`8_genre` == "Documentary"
validation_tidy$Documentary[temp] <- 1

temp <- validation_tidy$`8_genre` == "Drama"
validation_tidy$Drama[temp] <- 1

temp <- validation_tidy$`8_genre` == "Fantasy"
validation_tidy$Fantasy[temp] <- 1

temp <- validation_tidy$`8_genre` == "Film-Noir"
validation_tidy$Film_Noir[temp] <- 1

temp <- validation_tidy$`8_genre` == "Horror"
validation_tidy$Horror[temp] <- 1

temp <- validation_tidy$`8_genre` == "IMAX"
validation_tidy$IMAX[temp] <- 1

temp <- validation_tidy$`8_genre` == "Musical"
validation_tidy$Musical[temp] <- 1

temp <- validation_tidy$`8_genre` == "Mystery"
validation_tidy$Mystery[temp] <- 1

temp <- validation_tidy$`8_genre` == "Romance"
validation_tidy$Romance[temp] <- 1

temp <- validation_tidy$`8_genre` == "Sci-Fi"
validation_tidy$Sci_Fi[temp] <- 1

temp <- validation_tidy$`8_genre` == "Thriller"
validation_tidy$Thriller[temp] <- 1

temp <- validation_tidy$`8_genre` == "War"
validation_tidy$War[temp] <- 1

temp <- validation_tidy$`8_genre` == "Western"
validation_tidy$Western[temp] <- 1

temp <- validation_tidy$`8_genre` == "(no genres listed)"
validation_tidy$No_Genre[temp] <- 1


validation_tidy <- validation_tidy[,-c(6:13)]
validation_tidy <- validation_tidy%>% mutate(rated_year = year(as_datetime(validation_tidy$timestamp)))
validation_tidy <- validation_tidy[,-4]
validation_tidy <- validation_tidy[,c(1:3,25,4:24)]
head(validation_tidy)





# testing the algorithm on the validation data set


avg_rating_edx <- mean(edx_tidy$rating)
avg_rating_edx

movie_effect <- edx_tidy %>% group_by(movieId) %>% summarize(b_i = sum(rating-avg_rating_edx)/(n()+5))
movie_effect

edx_tidy <- edx_tidy %>% left_join(movie_effect, by = 'movieId')
edx_tidy

validation_tidy <- left_join(validation_tidy,movie_effect, by = 'movieId') 
validation_tidy

user_effect <- edx_tidy %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - avg_rating_edx)/(n()+5))
user_effect

edx_tidy <- edx_tidy %>% left_join(user_effect, by = 'userId')
edx_tidy

validation_tidy <- left_join(validation_tidy,user_effect,by = 'userId')
validation_tidy

year_effect <- edx_tidy %>% group_by(rated_year) %>% summarize(b_y = sum(rating - b_i - b_u - avg_rating_edx)/(n()+5))
year_effect

edx_tidy <- edx_tidy %>% left_join(year_effect, by = 'rated_year')
edx_tidy

validation_tidy <- left_join(validation_tidy,year_effect,by = 'rated_year')
validation_tidy



# calculating the genre effect


edx_tidy <- edx_tidy %>% mutate(b_genre = 0)
validation_tidy <- validation_tidy %>% mutate(b_genre = 0)


filtered_data <- edx_tidy %>% filter(Action == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Action == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Action == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Adventure == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Adventure == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Adventure == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Animation == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Animation == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Animation == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Children == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Children == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Children == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Comedy == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Comedy == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Comedy == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Crime == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Crime == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Crime == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Documentary == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Documentary == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Documentary == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Drama == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Drama == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Drama == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Fantasy == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Fantasy == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Fantasy == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Film_Noir == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Film_Noir == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Film_Noir == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Horror == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Horror == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Horror == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(IMAX == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(IMAX == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(IMAX == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Musical == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Musical == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Musical == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Mystery == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Mystery == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Mystery == 1,genre_bias_value,0) + b_genre)



filtered_data <- edx_tidy %>% filter(Romance == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Romance == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Romance == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Sci_Fi == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Sci_Fi == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Sci_Fi == 1,genre_bias_value,0) + b_genre)



filtered_data <- edx_tidy %>% filter(Thriller == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Thriller == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Thriller == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(War == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(War == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(War == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(Western == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(Western == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(Western == 1,genre_bias_value,0) + b_genre)


filtered_data <- edx_tidy %>% filter(No_Genre == 1)

effects_value <- filtered_data$rating - filtered_data$b_i - filtered_data$b_u - filtered_data$b_y -  avg_rating_edx - filtered_data$b_genre

genre_bias_value <- sum(effects_value)/(nrow(filtered_data)+5)

edx_tidy <- edx_tidy %>% mutate(b_genre = ifelse(No_Genre == 1,genre_bias_value,0)+b_genre)

validation_tidy <-validation_tidy %>% mutate(b_genre = ifelse(No_Genre == 1,genre_bias_value,0) + b_genre)





pred_rating <- avg_rating_edx + validation_tidy$b_i + validation_tidy$b_u +validation_tidy$b_y + validation_tidy$b_genre


# residual mean squared error

RMSE(validation_tidy$rating,pred_rating)

