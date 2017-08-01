# Assignment:     R Project (STAT 6430)
# Names:          Sally Gao, Stephen Mortensen, Kennan Grant
# Computing IDs:  sg2zv, sam8sp, khg3je

# 1. What seems to be associated with a high rating?
# 2. What groups are most likely to provide higher ratings?

# Read in and load -----------------------------------------------------

install.packages("zipcode")
install.packages("lubridate")
library(modes)
library(lubridate)
library(zipcode)
library(tidyr)
source("Rproj-main.R")

setwd("C:/Users/smort/OneDrive/Documents/Grad School/STAT6430/Assignments/R Final Project/STAT-6430-R-Project-master/STAT-6430-R-Project-master")

# Join all available data to reviews --------------------------------------

normalized_base_table <- reviews %>% 
  left_join(movies, by = "movie_id") %>% 
  left_join(reviewers, by = "reviewer_id") %>% 
  left_join(zip_codes, by = "zip_code")


# The Fundamental Question ------------------------------------------------

# 
# Given an explanatory variable value/level is an attribute of an observation, 
# what is the probability that the response variable of the observation is
# a particular value/level? If the probability is higher than the probability
# of encountering that response value/level in the overall dataset, then that
# explanatory variable value/level is predictive and can be said to be
# "associated" with the target response value/level.
#
# In our case, we want to know 'what seems to be associated with a high rating?'
# In other words: What variable values/levels, when present, predict a high
# rating with a higher probability than the baseline probability in the overall
# dataset?
#

# Rating Distribution Overview (Graph 1) --------------------------------------

# rating distribution bar chart
normalized_base_table %>% 
  ggplot(aes(x = rating)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab(label = "Percent")

# bar chart data, in table
normalized_base_table %>% 
  group_by(rating) %>% 
  summarise(n = n()
            , percent = n/100000*100)

# conclusion: 
34.174 + 21.201 # 55.375% of reviews are 'high' ratings of 4 or 5. This is the 
# 'baseline probability'.  For a variable value/level to be 
# 'associated' with a high-rating (4 or 5), then, given that particular
# variable value/level the probability of a high-rating
# must be higher than 55.375%. 



# Reviewer Breakdown By Mode Rating (Graph 2) ---------------------------------

# find mode rating of reviewer
reviewer_id_ratings <- normalized_base_table %>% 
  group_by(reviewer_id) %>% 
  summarise(mode_rating = modes::modes(rating)[1]) %>% 
  arrange(reviewer_id)

# append reviewer mode col to a dataframe
mode_appended <- normalized_base_table %>% 
  select(reviewer_id, rating) %>% 
  left_join(reviewer_id_ratings, by = "reviewer_id")

# # calculate probability of high rating, given high mode rating
high_mode_ratings <- mode_appended %>% 
  group_by(mode_rating == 4 | mode_rating == 5) %>% 
  summarise(n = n()
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # 64% -- implies association

high_mode_ratings

mode_ratings <- as.data.frame(matrix(NA, nrow=1,ncol=4))
mode_ratings[1,] <- c("Mode","High Mode", 8.956, "indianred")
names(mode_ratings) <- c("Analysis","Category", "Prop", "ColorChoice")

ggplot(data=mode_ratings, aes(x=Category, y=Prop, fill=ColorChoice)) +
  geom_bar(stat="identity") + guides(fill=FALSE)


# Note that we checked to make sure that it was *NOT* the case that many
# reviewers were 1-review reviewers, which would have made our prediction
# technique useless going forward.  In fact, the minimum number of reviews
# of any reviewer in this dataset is 20.


# Timestamp (Graph 3) ---------------------------------------------------------

# calculate probability of high rating, given an hour of the timestamp
time_ratings1 <- normalized_base_table %>% 
  group_by(hour = lubridate::hour(timestamp)) %>% 
  summarise(n = n()
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # hours 16, 15, and 12 are most likely to 
# yield high rating (61.7%, 61.6%, and 60% -- respectively)

time_ratings1

time_ratings <- as.data.frame(matrix(NA, nrow=24,ncol=4))
time_ratings[,1] <- "Time"
time_ratings[,2] <- time_ratings1[,1]
time_ratings[,3] <- time_ratings1[,3]-55.375
time_ratings[,4] <- "steelblue"
names(time_ratings) <- c("Analysis","Category", "Prop", "ColorChoice")

ggplot(data=time_ratings, aes(x=Category, y=Prop, fill=ColorChoice)) + geom_bar(stat="identity") 


# Release Date ------------------------------------------------------------

normalized_base_table$release_date <- as.Date(
  x=normalized_base_table$release_date,format='%d-%b-%Y') # convert to date

date_ratings1 <- normalized_base_table %>% 
  group_by(lubridate::month(release_date)) %>% 
  summarise(n = n()
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # only January is even mildly associated 
# with high rating, at 58.3%

date_ratings1

date_ratings <- as.data.frame(matrix(NA, nrow=13,ncol=4))
date_ratings[,1] <- "Date"
date_ratings[,2] <- date_ratings1[,1]
date_ratings[,3] <- date_ratings1[,3]-55.375
date_ratings[,4] <- "seagreen"
names(date_ratings) <- c("Analysis","Category", "Prop", "ColorChoice")

ggplot(data=date_ratings, aes(x=Category, y=Prop)) + geom_bar(stat="identity", fill="seagreen") 


# Genres -------------------------------------------------------------

genre_rating <- as.data.frame(matrix(NA,nrow=2,ncol=4))
names(genre_rating) <- c("genre","totalrev","highrev","perc_high_rating")

# Film-Noir   
genre_rating[1,2:4] <- normalized_base_table %>% 
  filter(`Film-Noir` == 1) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # 70.6%

genre_rating[1,1] <- "FilmNoir"

# War    
genre_rating[2,2:4] <- normalized_base_table %>% 
  filter(War == 1) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # 66.5%

genre_rating[2,1] <- "War"
genre_rating[,4] <- genre_rating[,4]-55.375

genre_rating

genre_ratings <- as.data.frame(matrix(NA, nrow=2,ncol=4))
genre_ratings[,1] <- "Genre"
genre_ratings[,2] <- genre_rating[,1]
genre_ratings[,3] <- genre_rating[,4]
genre_ratings[,4] <- "peachpuff"
names(genre_ratings) <- c("Analysis","Category", "Prop", "ColorChoice")

genre_ratings

ggplot(data=genre_ratings, aes(x=Category, y=Prop)) + geom_bar(stat="identity", fill="peachpuff") 


# State -------------------------------------------------------------------

# State is *very* predictive.
state_ratings1 <- normalized_base_table %>% 
  group_by(State) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating))

state_ratings1

state_ratings <- as.data.frame(matrix(NA, nrow=54,ncol=4))
state_ratings[,1] <- "State"
state_ratings[,2] <- state_ratings1[,1]
state_ratings[,3] <- state_ratings1[,4]-55.375
state_ratings[,4] <- "mediumpurple"
names(state_ratings) <- c("Analysis","Category", "Prop", "ColorChoice")

state_ratings <- state_ratings[1:10,]

ggplot(data=state_ratings, aes(x=reorder(Category, -Prop), y=Prop)) + geom_bar(stat="identity", fill="mediumpurple") 


# City --------------------------------------------------------------------

# City is very predictive, but a closer look reveals it is single reviewers
normalized_base_table %>% 
  group_by(City) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  #as.data.frame() %>% 
  arrange(desc(perc_high_rating))


mastergraph <- rbind(mode_ratings, time_ratings,date_ratings,genre_ratings,state_ratings)
mastergraph

ggplot(data=mastergraph, aes(x=Category, y=Prop))+geom_bar(fill='steelblue', stat="identity")+facet_wrap( ~ Analysis, scales="free_x")+guides(fill=FALSE)+scale_y_continuous(labels=scales::percent)
