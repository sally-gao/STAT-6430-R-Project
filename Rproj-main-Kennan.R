# Assignment:     R Project (STAT 6430)
# Names:          Sally Gao, Stephen Mortensen, Kennan Grant
# Computing IDs:  sg2zv, sam8sp, khg3je

# 1. What seems to be associated with a high rating?
# 2. What groups are most likely to provide higher ratings?

# Read in and load -----------------------------------------------------

library(modes)
library(lubridate)
library(zipcode)
source("Rproj-main.R")



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

# create pie chart of 64% !!!


# Note that we checked to make sure that it was *NOT* the case that many
# reviewers were 1-review reviewers, which would have made our prediction
# technique useless going forward.  In fact, the minimum number of reviews
# of any reviewer in this dataset is 20.


# Timestamp (Graph 3) ---------------------------------------------------------

# calculate probability of high rating, given an hour of the timestamp
normalized_base_table %>% 
  group_by(hour = lubridate::hour(timestamp)) %>% 
  summarise(n = n()
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # hours 16, 15, and 12 are most likely to 
# yield high rating (61.7%, 61.6%, and 60% -- respectively)


# Release Date ------------------------------------------------------------

normalized_base_table$release_date <- as.Date(
  x=normalized_base_table$release_date,format='%d-%b-%Y') # convert to date

normalized_base_table %>% 
  group_by(lubridate::month(release_date)) %>% 
  summarise(n = n()
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # only January is even mildly associated 
# with high rating, at 58.3%


# Genres -------------------------------------------------------------

# Film-Noir   
normalized_base_table %>% 
  filter(`Film-Noir` == 1) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # 70.6%


# War    
normalized_base_table %>% 
  filter(War == 1) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating)) # 66.5%



# State -------------------------------------------------------------------

# State is *very* predictive.
normalized_base_table %>% 
  group_by(State) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  as.data.frame() %>% 
  arrange(desc(perc_high_rating))


# City --------------------------------------------------------------------

# City is very predictive, but a closer look reveals it is single reviewers
normalized_base_table %>% 
  group_by(City) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  #as.data.frame() %>% 
  arrange(desc(perc_high_rating))



