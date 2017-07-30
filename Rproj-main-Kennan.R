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


modes::modes(normalized_base_table$rating)[1]

# Univariate Analysis -----------------------------------------------------

# reviwer_id
#
# mode rating of reviewer
reviewer_id_ratings <- normalized_base_table %>% 
  group_by(reviewer_id) %>% 
  summarise(mode_rating = modes::modes(rating)[1]) %>% 
  arrange(reviewer_id)

mode_appended <- normalized_base_table %>% # append reviewer mode to ratings
  select(reviewer_id, rating) %>% 
  left_join(reviewer_id_ratings, by = "reviewer_id")


modes::modes(normalized_base_table$rating)[1] # mode overall is '4'
sum(normalized_base_table$rating == 4) / nrow(normalized_base_table) * 100
# 34.1% of reviews are overall mode of '4'
sum(mode_appended$rating == mode_appended$mode_rating) / 
  nrow(mode_appended) * 100 # 41% of time, mode of reviewer predicts review
# this is better than the 34.1% of reviews that are the overall mode of '4'.
#
# Note that we checked to make sure that it wasn't not the case that many
# reviewers were not 1-review reviewers, which would have made our prediction
# technique useless going forward.  In fact, the minimum number of reviews
# of any reviewer in this dataset is 20.

# ---

# timestamp
#
normalized_base_table %>% 
  group_by(lubridate::wday(timestamp)) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) # no signal for day of week

normalized_base_table %>% 
  group_by(lubridate::hour(timestamp)) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) %>% 
  as.data.frame() # there is signal for hour of day.  


# release_date
#
normalized_base_table$release_date <- as.Date(
  x=normalized_base_table$release_date,format='%d-%b-%Y') # convert to date

normalized_base_table %>% 
  group_by(lubridate::month(release_date)) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) # August and May slightly more likely to be 3 than other
                       # which are more likely to be 4


# Fantasy
normalized_base_table %>% 
  group_by(Fantasy) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) # 3.21 avg-- lower

# Film-Noir   
normalized_base_table %>% 
  group_by(`Film-Noir`) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) # 3.92 avg-- higher

# Horror
normalized_base_table %>% 
  group_by(Horror) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) # 3.29 avg-- lower

# War    
normalized_base_table %>% 
  group_by(War) %>% 
  summarise(mean(rating)
            , modes::modes(rating)[1]
            , n = n()) # 3.81 avg-- higher


# convert lat and long to double
normalized_base_table <- normalized_base_table %>% 
  mutate(Lat = as.double(Lat)
         ,Long = as.double(Long))


# gender
#
# shows distributions by rating by gender
normalized_base_table %>% 
  ggplot(aes(x=rating, fill = factor(gender))) +
  geom_bar() +
  facet_wrap(~ gender, scales = "free")

normalized_base_table %>% 
  group_by(gender, rating) %>% 
  summarise(n = n()) %>% 
  mutate(proportion = n / sum(n)) # only minor differences

# occupation
#
# 
normalized_base_table %>% 
  group_by(occupation) %>% 
  summarise(n = n(),
            mean = mean(rating)) %>% 
  arrange(desc(n)) %>% 
  as.data.frame() # healthcare gives low -- 2.90


# City
#
# 
normalized_base_table %>% 
  group_by(City) %>% 
  summarise(n = n(),
            mean = mean(rating)) %>% 
  arrange(desc(n)) %>% 
  as.data.frame() # NYC gives low -- 2.94




# variable list -----------------------------------------------------------



"Lat"           
"Long"          

