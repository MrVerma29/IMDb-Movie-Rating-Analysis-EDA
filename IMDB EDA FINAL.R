#Importing relevant libraries
library(dplyr)
library(rvest)
library(plotly)
library(tidyr)
library(ggplot2)
library(stringr)
library(corrplot)

# Read the CSV file containing IMDb movie metadata into a data frame named IMDB
IMDB <- read.csv("C://Users//Issac.Abraham//Desktop//movie_metadata.csv")
# Generate summary statistics for the IMDB data frame
summary(IMDB)

# View the top 5 rows of the dataset
head(IMDB, 5)

#View dimensions of the dataset
dim(IMDB)

# Get the data types of each column in the dataset
str(IMDB)

# Count the number of duplicated rows in the dataset
sum(duplicated(IMDB))

# Remove duplicate rows from the dataset
IMDB <- unique(IMDB)

# Print the first 5 movie titles
movie_titles <- IMDB$movie_title
head(movie_titles, 5)

# Remove special character "Â" from movie titles in the IMDB$movie_title column
IMDB$movie_title <- str_replace_all(IMDB$movie_title, "Â", "")  # Replace special character with ""
IMDB$movie_title <- str_trim(IMDB$movie_title, side = "right")  # Trim spaces from the right end
head(IMDB$movie_title)

# Separate rows in the 'genres' column based on the '|' separator,
IMDB <- IMDB %>%
  separate_rows(genres, sep = "\\|") %>%
# Create a new column 'genre_indicator' with value 1
  mutate(genre_indicator = 1) %>%
  spread(genres, genre_indicator, fill = 0)

# Print the updated dataset
head(IMDB)

# Count the number of NULL values in each column
na_count <- colSums(is.na(IMDB))

# Print the result
print(na_count)
# Remove rows with any NA values from the IMDB dataset
IMDB <- na.omit(IMDB)

# Identify columns with numerical data
num_vars <- IMDB %>%
  select_if(is.numeric) %>%
  colnames()

# Print the list of columns with numerical data
print(num_vars)

# Identify columns with categorical data
cat_vars <- setdiff(names(IMDB), num_vars)
print(cat_vars)


#UNIVARIATE ANALYSIS OF THE NUMERICAL COLUMNS

# Create a histogram of the 'num_critic_for_reviews' variable in the IMDB dataset
hist_num_critic_for_reviews <- ggplot(IMDB, aes(x = num_critic_for_reviews)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Num Critic for Reviews") +
  theme_minimal()
# Print the histogram
print(hist_num_critic_for_reviews)


# Create a histogram of the 'duration' variable in the IMDB dataset
hist_duration <- ggplot(IMDB, aes(x = duration)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Duration") +
  theme_minimal()
# Print the histogram
print(hist_duration)


# Create a histogram of the 'director_facebook_likes' variable in the IMDB dataset
hist_director_facebook_likes <- ggplot(IMDB, aes(x = director_facebook_likes)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Director Facebook Likes") +
  theme_minimal()
# Print the histogram
print(hist_director_facebook_likes)


# Create a histogram of the 'actor_3_facebook_likes' variable in the IMDB dataset
hist_actor_3_facebook_likes <- ggplot(IMDB, aes(x = actor_3_facebook_likes)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Actor 3 Facebook Likes") +
  theme_minimal()
# Print the histogram
print(hist_actor_3_facebook_likes)


# Create a histogram of the 'actor_1_facebook_likes' variable in the IMDB dataset
hist_actor_1_facebook_likes <- ggplot(IMDB, aes(x = actor_1_facebook_likes)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Actor 1 Facebook Likes") +
  theme_minimal()
# Print the histogram
print(hist_actor_1_facebook_likes)


# Create a histogram of the 'gross' variable in the IMDB dataset
hist_gross <- ggplot(IMDB, aes(x = gross)) +
  geom_histogram(binwidth = 10000000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Gross") +
  theme_minimal()
# Print the histogram
print(hist_gross)


# Create a histogram of the 'num_voted_users' variable in the IMDB dataset
hist_num_voted_users <- ggplot(IMDB, aes(x = num_voted_users)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Num Voted Users") +
  theme_minimal()
# Print the histogram
print(hist_num_voted_users)


# Create a histogram of the 'cast_total_facebook_likes' variable in the IMDB dataset
hist_cast_total_facebook_likes <- ggplot(IMDB, aes(x = cast_total_facebook_likes)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Cast Total Facebook Likes") +
  theme_minimal()
# Print the histogram
print(hist_cast_total_facebook_likes)

# Create a histogram of the 'facenumber_in_poster' variable in the IMDB dataset
hist_facenumber_in_poster <- ggplot(IMDB, aes(x = facenumber_in_poster)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Face Number in Poster") +
  theme_minimal()
# Print the histogram
print(hist_facenumber_in_poster)


# Create a histogram of the 'num_user_for_reviews' variable in the IMDB dataset
hist_num_user_for_reviews <- ggplot(IMDB, aes(x = num_user_for_reviews)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Number of User Reviews") +
  theme_minimal()
# Print the histogram
print(hist_num_user_for_reviews)


# Create a histogram of the 'budget' variable in the IMDB dataset
hist_budget <- ggplot(IMDB, aes(x = budget)) +
  geom_histogram(binwidth = 10000000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Budget") +
  theme_minimal()
# Print the histogram
print(hist_budget)


# Create a histogram of the 'title_year' variable in the IMDB dataset
hist_title_year <- ggplot(IMDB, aes(x = title_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Title Year") +
  theme_minimal()
# Print the histogram
print(hist_title_year)


# Create histogram for 'actor_2_facebook_likes'
hist_actor_2_facebook_likes <- ggplot(IMDB, aes(x = actor_2_facebook_likes)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Actor 2 Facebook Likes") +
  theme_minimal()
# Print the histogram for 'actor_2_facebook_likes'
print(hist_actor_2_facebook_likes)


# Create histogram for 'imdb_score'
hist_imdb_score <- ggplot(IMDB, aes(x = imdb_score)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of IMDB Score") +
  theme_minimal()
# Print the histogram for 'imdb_score'
print(hist_imdb_score)


# Create histogram for 'aspect_ratio'
hist_aspect_ratio <- ggplot(IMDB, aes(x = aspect_ratio)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "orange", alpha = 0.7) +
  labs(title = "Histogram of Aspect Ratio") +
  theme_minimal()
# Print the histogram for 'aspect_ratio'
print(hist_aspect_ratio)


# Create histogram for 'movie_facebook_likes'
hist_movie_facebook_likes <- ggplot(IMDB, aes(x = movie_facebook_likes)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "purple", alpha = 0.7) +
  labs(title = "Histogram of Movie Facebook Likes") +
  theme_minimal()
# Print the histogram for 'movie_facebook_likes'
print(hist_movie_facebook_likes)


#UNIVARIATE ANALYSIS OF THE CATEGORICAL COLUMNS

# Create histogram for 'COLOR'
bar_color <- ggplot(IMDB, aes(x = color)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Color") +
  theme_minimal()
# Print the bar chart for 'color'
print(bar_color)

# Create histogram for 'director_name'
bar_director_name <- ggplot(IMDB, aes(x = director_name)) +
  geom_bar(fill = "skyblue", color = "white") +
  labs(title = "Bar Chart of Director Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# Print the bar chart for 'director_name'
print(bar_director_name)

# Create histogram for 'content_rating'
bar_content_rating <- ggplot(IMDB, aes(x = content_rating)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Content Rating") +
  theme_minimal()
# Print the bar chart for 'content_rating'
print(bar_content_rating)


# Create histogram for 'actor_2_name'
bar_actor_2_name <- ggplot(IMDB, aes(x = actor_2_name)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Actor 2 Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
# Print the bar chart for 'actor_2_name'
print(bar_actor_2_name)


# Create bar chart for 'movie_title'
bar_movie_title <- ggplot(IMDB, aes(x = movie_title)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Movie Title") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# Print the bar chart for 'movie_title'
print(bar_movie_title)


# Create histogram for 'genres'
bar_genres <- ggplot(IMDB, aes(x = genres)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Genres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
# Print the bar chart for 'genres'
print(bar_genres)


# Create histogram for 'language'
bar_language <- ggplot(IMDB, aes(x = language)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Language") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# Print the bar chart for 'language'
print(bar_language)


# Create histogram for 'country'
bar_country <- ggplot(IMDB, aes(x = country)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print the bar chart for 'country'
print(bar_country)


#BIVARIATE ANALYSIS CATEGORICAL VS NUMERICAL COLUMNS

# Box plot for 'color' vs 'num_critic_for_reviews'
boxplot_color_num_critic <- ggplot(IMDB, aes(x = color, y = num_critic_for_reviews, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Num Critic for Reviews") +
  theme_minimal()

# Print the box plot
print(boxplot_color_num_critic)

# Box plot for 'content_rating' vs 'num_critic_for_reviews'
boxplot_content_rating_num <- ggplot(IMDB, aes(x = content_rating, y = num_critic_for_reviews, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Num Critic for Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Print the box plot
print(boxplot_content_rating_num)


# Box plot for 'color' vs 'duration'
boxplot_color_duration <- ggplot(IMDB, aes(x = color, y = duration, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Duration") +
  theme_minimal()

# Print the box plot
print(boxplot_color_duration)


# Box plot for 'content_rating' vs 'duration'
boxplot_content_rating_duration <- ggplot(IMDB, aes(x = content_rating, y = duration, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Duration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Print the box plot
print(boxplot_content_rating_duration)


# Box plot for 'color' vs 'director_facebook_likes'
boxplot_color_director_facebook <- ggplot(IMDB, aes(x = color, y = director_facebook_likes, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Director Facebook Likes") +
  theme_minimal()

# Print the box plot
print(boxplot_color_director_facebook)



# Box plot for 'content_rating' vs 'director_facebook_likes'
boxplot_content_rating_director_facebook <- ggplot(IMDB, aes(x = content_rating, y = director_facebook_likes, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Director Facebook Likes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Print the box plot
print(boxplot_content_rating_director_facebook)


# Box plot for 'color' vs 'actor_3_facebook_likes'
boxplot_color_actor3_facebook_likes <- ggplot(IMDB, aes(x = color, y = actor_3_facebook_likes, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Actor 3 Facebook Likes") +
  theme_minimal()

# Print the box plot
print(boxplot_color_actor3_facebook_likes)


# Box plot for 'content_rating' vs 'actor_3_facebook_likes'
boxplot_content_rating_actor3_facebook_likes <- ggplot(IMDB, aes(x = content_rating, y = actor_3_facebook_likes, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Actor 3 Facebook Likes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Print the box plot
print(boxplot_content_rating_actor3_facebook_likes)


# Box plot for 'color' vs 'actor_1_facebook_likes'
boxplot_color_actor1_facebook_likes <- ggplot(IMDB, aes(x = color, y = actor_1_facebook_likes, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Actor 1 Facebook Likes") +
  theme_minimal()

# Print the box plot
print(boxplot_color_actor1_facebook_likes)


# Box plot for 'color' vs 'num_voted_users'
boxplot_color_num_voted_users <- ggplot(IMDB, aes(x = color, y = num_voted_users, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Num Voted Users") +
  theme_minimal()

# Print the box plot
print(boxplot_color_num_voted_users)

# Box plot for 'content_rating' vs 'num_voted_users'
boxplot_content_rating_num_voted_users <- ggplot(IMDB, aes(x = content_rating, y = num_voted_users, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Num Voted Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_num_voted_users)

# Box plot for 'color' vs 'facenumber_in_poster'
boxplot_color_facenumber_in_poster <- ggplot(IMDB, aes(x = color, y = facenumber_in_poster, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Face Number in Poster") +
  theme_minimal()

# Print the box plot
print(boxplot_color_facenumber_in_poster)

# Box plot for 'content_rating' vs 'facenumber_in_poster'
boxplot_content_rating_facenumber_in_poster <- ggplot(IMDB, aes(x = content_rating, y = facenumber_in_poster, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Face Number in Poster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_facenumber_in_poster)

# Box plot for 'color' vs 'num_user_for_reviews'
boxplot_color_num_user_for_reviews <- ggplot(IMDB, aes(x = color, y = num_user_for_reviews, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Num User for Reviews") +
  theme_minimal()

# Print the box plot
print(boxplot_color_num_user_for_reviews)

# Box plot for 'content_rating' vs 'num_user_for_reviews'
boxplot_content_rating_num_user_for_reviews <- ggplot(IMDB, aes(x = content_rating, y = num_user_for_reviews, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Num User for Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_num_user_for_reviews)

# Box plot for 'color' vs 'budget'
boxplot_color_budget <- ggplot(IMDB, aes(x = color, y = budget, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Budget") +
  theme_minimal()

# Print the box plot
print(boxplot_color_budget)

# Box plot for 'content_rating' vs 'budget'
boxplot_content_rating_budget <- ggplot(IMDB, aes(x = content_rating, y = budget, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Budget") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_budget)

# Box plot for 'color' vs 'title_year'
boxplot_color_title_year <- ggplot(IMDB, aes(x = color, y = title_year, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Title Year") +
  theme_minimal()

# Print the box plot
print(boxplot_color_title_year)

# Box plot for 'content_rating' vs 'title_year'
boxplot_content_rating_title_year <- ggplot(IMDB, aes(x = content_rating, y = title_year, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Title Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_title_year)

# Box plot for 'color' vs 'imdb_score'
boxplot_color_imdb_score <- ggplot(IMDB, aes(x = color, y = imdb_score, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs IMDB Score") +
  theme_minimal()

# Print the box plot
print(boxplot_color_imdb_score)

# Box plot for 'content_rating' vs 'imdb_score'
boxplot_content_rating_imdb_score <- ggplot(IMDB, aes(x = content_rating, y = imdb_score, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs IMDB Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_imdb_score)

# Box plot for 'color' vs 'aspect_ratio'
boxplot_color_aspect_ratio <- ggplot(IMDB, aes(x = color, y = aspect_ratio, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Aspect Ratio") +
  theme_minimal()

# Print the box plot
print(boxplot_color_aspect_ratio)


# Box plot for 'content_rating' vs 'aspect_ratio'
boxplot_content_rating_aspect_ratio <- ggplot(IMDB, aes(x = content_rating, y = aspect_ratio, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Aspect Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_aspect_ratio)


# Box plot for 'color' vs 'movie_facebook_likes'
boxplot_color_movie_facebook_likes <- ggplot(IMDB, aes(x = color, y = movie_facebook_likes, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Movie Facebook Likes") +
  theme_minimal()

# Print the box plot
print(boxplot_color_movie_facebook_likes)


# Box plot for 'content_rating' vs 'movie_facebook_likes'
boxplot_content_rating_movie_facebook_likes <- ggplot(IMDB, aes(x = content_rating, y = movie_facebook_likes, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Movie Facebook Likes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_movie_facebook_likes)


# Box plot for 'color' vs 'gross'
boxplot_color_gross <- ggplot(IMDB, aes(x = color, y = gross, fill = color)) +
  geom_boxplot() +
  labs(title = "Box Plot: Color vs Gross") +
  theme_minimal()

# Print the box plot
print(boxplot_color_gross)


# Box plot for 'content_rating' vs 'gross'
boxplot_content_rating_gross <- ggplot(IMDB, aes(x = content_rating, y = gross, fill = content_rating)) +
  geom_boxplot() +
  labs(title = "Box Plot: Content Rating vs Gross") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Print the box plot
print(boxplot_content_rating_gross)

#summary of statistics of the dataframe
summary(IMDB)

#The Given data set of IMDb rating has varied data,and outlier treatment cannot be undertaken as outliers in budget and IMDb score can be a key focus area like high bugdget movies.

# Creating 'profit' column
IMDB$profit <- IMDB$gross - IMDB$budget
print(IMDB$profit,5)

# Create 'return_on_investment' column
IMDB$return_on_investment <- (IMDB$profit / IMDB$budget) * 100
head(IMDB$return_on_investment, 5)

# Create a new dataframe df1 by selecting specific columns from the IMDB dataset
df1 <- IMDB %>%
  select(director_name, num_critic_for_reviews, actor_1_name, movie_title, 
         num_voted_users, cast_total_facebook_likes, facenumber_in_poster, movie_facebook_likes, 
         num_user_for_reviews, content_rating, title_year, imdb_score, budget, profit, 
         return_on_investment)

# Display the first few rows of the new dataframe df1
head(df1)


#MULTIVARIATE ANALYSIS

# Create a histogram of movie releases over the years
ggplot(df1, aes(x = title_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie Releases") +
  theme_minimal()
#From the graph, it can be infered that there aren't many records of movies released before 1980.
#Thus to make the data represent records of before 1980 will be removed.

#Filter df1 to include only movies released from 1980 onwards
df1 <- df1 %>%
  filter(title_year >= 1980)


#TOP 20 MOST PROFITABLE MOVIES
# Sort df1 by profit in descending order
sorted_IMDB <- df1[order(df1$profit, decreasing = TRUE), ]
# Select the top 20 most profitable movies
top_20 <- head(sorted_IMDB, 20)

# Create scatter plot with regression line and text labels
ggplot(top_20, aes(x = budget / 1e6, y = profit / 1e6, label = movie_title)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(hjust = -0.1, vjust = -0.5, size = 3) +
  labs(x = "Budget $million", y = "Profit $million", title = "Top 20 Profitable Movies") +
  theme_minimal()
#It can be inferred from this plot that high budget movies tend to earn more profit. 
#The trend is almost linear, with profit increasing with the increase in budget.



#THE 20 MOST PROFITABLE MOVIES BASED ON INVESTMENT
# Sort the DataFrame by profit in descending order
sorted_IMDB1 <- df1[order(df1$profit, decreasing = TRUE), ]

# Select the top 20 most profitable movies
top_20_1 <- head(sorted_IMDB1, 20)

# Create scatter plot with regression line and text labels
ggplot(top_20_1, aes(x = budget / 1e6, y = return_on_investment, label = movie_title)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(hjust = -0.1, vjust = -0.5, size = 3) +
  labs(x = "Budget $million", y = "Percent Return on Investment", title = "20 Most Profitable Movies based on Return on Investment")
#These are the top 20 movies based on its Percentage Return on Investment ((profit/budget)*100).
#Since profit earned by a movie does not give a clear picture about its monetary success over the years, this analysis, over the absolute value of the Return on Investment(ROI) across its Budget, would provide better results.
#As hypothesized, the ROI is high for Low Budget Films and decreases as the budget of the movie increases.



#TOP 20 DIRECTORS WITH AVERAGE HIGHEST IMBD RATING
#Creating a dataframe with diarectors with average imbd rating
director_avg_imdb <- aggregate(imdb_score ~ director_name, data = df1, FUN = mean)

# Sort the directors by average IMDb score in descending order
director_avg_imdb <- director_avg_imdb[order(director_avg_imdb$imdb_score, decreasing = TRUE), ]

# Select the top 20 directors with the highest average IMDb scores
top_20_directors <- head(director_avg_imdb, 20)

# Create a horizontal bar plot for the top directors
ggplot(top_20_directors, aes(x = imdb_score, y = reorder(director_name, imdb_score))) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(x = "Average IMDb Score", y = "Director Name", title = "Top 20 Directors with Highest Average IMDb Scores") +
  theme_minimal()



#TOP DIRECTORS BY TOTAL PROFIT
# Calculate the total profit for each director
director_profit <- aggregate(profit ~ director_name, data = df1, FUN = sum)

# Sort the data by total profit in descending order
director_profit <- director_profit[order(director_profit$profit, decreasing = TRUE), ]

# Select the top N directors to display in the plot (e.g., top 10)
top_directors <- head(director_profit, 10)

# Create a bar plot
ggplot(top_directors, aes(x = director_name, y = profit)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(x = "Director Name", y = "Total Profit", title = "Top Directors by Total Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
#Thus it can be infered that top directors can raise prospect of profits but may not promise high ratings


#SCATTER PLOT OF MOVIE FACEBOOK LIKES VS IMDB SCORE
# Create a scatter plot using ggplot2
ggplot(IMDB, aes(x = movie_facebook_likes, y = imdb_score, color = content_rating)) +
  geom_point(alpha = 0.7) +
  labs(x = "Movie Facebook Likes", y = "IMDb Score", title = "Scatter Plot of IMDb Score vs. Movie Facebook Likes") +
  theme_minimal()
#Movie with extremely high Facebook likes tend to have higher imdb score.
#But the score for movie with low Facebook likes vary in a very wide range.


#COMPARISON BETWEEN NUM_CRITICS AND MOVIES_FACEBOOK_LIKES
# Create a scatter plot using ggplot2
ggplot(IMDB, aes(x = num_critic_for_reviews, y = movie_facebook_likes)) +
  geom_point(alpha = 0.5) +
  labs(x = "Number of Critic Reviews", y = "Movie Facebook Likes", title = "Comparison of Number of Critic Reviews vs. Movie Facebook Likes") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal()
#It can be infered that the number of critics are instrumental in forming a unbiased opinion that effects the public opinion.



#HEATMAP GIVING CORELATION BETWEEN DIFFERNT COLUMNS IN THE IMDB DATA
#creating a dataframe with the columns from df1
IMDB2 <- df1[, c('num_critic_for_reviews', 'cast_total_facebook_likes', 'num_user_for_reviews', 
                 'title_year', 'movie_facebook_likes', 'num_voted_users', 'facenumber_in_poster', 'budget')]

# Calculate the correlation matrix
corr <- cor(IMDB2)

# Create the heatmap
corrplot(corr, method = "color", type = "full", tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("navy", "white", "firebrick3"))(100), 
         title = "Correlation Heatmap")
#Based on the heatmap, we can see some high correlations (greater than 0.7) between predictors.
#According to the highest correlation value 0.95, we find actor_1_facebook_likes is highly correlated with the cast_total_facebook_likes 
#There are high correlations among num_voted_users, num_user_for_reviews and num_critic_for_reviews. We want to keep num_voted_users and take the ratio of num_user_for_reviews and num_critic_for_reviews.


#PROFIT VS IMDB SCORE
# Create the scatter plot
ggplot(df1, aes(x = profit, y = imdb_score)) +
  geom_point(alpha = 0.5) +
  labs(x = "Profit", y = "IMDb Score", title = "Scatter Plot of Profit vs. IMDb Score") +
  theme_minimal()
#It can be infered that highly rated movies have a higher chance of encountering losses. Loss prospect is higher in high budget movies with IMDb Score >6.
#From the heatmap it can be seen that num_critic_for_reviews and movie_facebook_likes have high degree of co relattion thus we'll further analyse them


#CRITIC REVIEW VS IMDB SCORE
# Create the scatter plot
ggplot(IMDB, aes(x = num_critic_for_reviews, y = imdb_score)) +
  geom_point(alpha = 0.5) +
  labs(x = "Number of Critic Reviews", y = "IMDb Score", title = "Scatter Plot of Number of Critic Reviews vs. IMDb Score") +
  theme_minimal()
#It can be infered that there is high correlation as the number of critics rises the IMDb Score is high.