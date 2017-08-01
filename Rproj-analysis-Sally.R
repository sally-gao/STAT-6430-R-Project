# Assignment:     R Project (STAT 6430)
# Names:          Sally Gao, Stephen Mortensen, Kennan Grant
# Computing IDs:  sg2zv, sam8sp, khg3je

# 1. What seems to be associated with a high rating?
# 2. What groups are most likely to provide higher ratings?

# copying Kennan
big_table <- reviews %>% 
  left_join(movies, by = "movie_id") %>% 
  left_join(reviewers, by = "reviewer_id") %>% 
  left_join(zip_codes, by = "zip_code")

# fill in Location as CANADA for zip codes with letters in them
big_table$Location[grepl("[[:alpha:]]", big_table$zip_code)]<- "CANADA"

big_table$occupation <- factor(big_table$occupation)

mean(big_table$rating) # 3.52986

big_table %>% 
  group_by(occupation) %>% 
  summarise (mean_rating = mean(rating), median_rating = median(rating)) -> occupation_summary
# healthcare workers give really low ratings - mean of 2.896220!!
# lawyers and doctors tend to give high ratings.

ggplot(data = big_table, mapping = aes(x = occupation, y = rating)) +
  geom_boxplot() +  coord_flip()