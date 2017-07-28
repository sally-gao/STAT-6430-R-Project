
# 1. Which percentage of each rating was given?
# =============================================

# count number of ratings for each level (1,2, ..5 ) and divide by total number of reviews
sum(reviews$rating==1) / nrow(reviews) * 100 # 6.11% of all ratings are 1
sum(reviews$rating==2) / nrow(reviews) * 100 # 11.37% of all ratings are 2
sum(reviews$rating==3) / nrow(reviews) * 100 # 27.145% of all ratings are 3
sum(reviews$rating==4) / nrow(reviews) * 100 # 34.174% of all ratings are 4
sum(reviews$rating==5) / nrow(reviews) * 100 # 21.201% of all ratings are 5

# 2. Which reviewers were the top-10 in terms of number of movies reviewed?
# =========================================================================

# group by reviewer_id and count frequencies
reviewer.count <- summarise(group_by(reviews, reviewer_id), n())

# sort by frequency (highest to lowest)
reviewer.count.sorted <- reviewer.count[order(-reviewer.count$`n()`), ]

# get top 10
top10 <- reviewer.count.sorted[1:10,]

top10
#   reviewer_id   n()
# 1         405   737
# 2         655   685
# 3          13   636
# 4         450   540
# 5         276   518
# 6         416   493
# 7         537   490
# 8         303   484
# 9         234   480
# 10        393   448

# 3. Find a 95% confidence interval for the average rating among all reviewers,
# and a 95% confidence interval for the average rating among the top-10 reviewers.
# ===============================================================================

# find mean rating for each reviewer
reviewers.mean <- summarise(group_by(reviews, reviewer_id), mean(rating))

x <- mean(reviews$rating) # mean of all ratings
n <- nrow(reviews) # number of reviews
sd <- sd(reviews$rating) # sd of ratings

# find 95% confidence intervals
x - qt(.975, n-1) * (sd/sqrt(n))
x + qt(.975, n-1) * (sd/sqrt(n))
# Output: 3.522883 and 3.536837

# find movies rated by top 10 reviewers
reviews %>% 
  filter(reviewer_id %in% top10$reviewer_id) %>% 
  select(rating) -> top10reviews

x2 <- mean(top10reviews$rating) # mean
n2 <- nrow(top10reviews) # number of reviews
sd2 <- sd(top10reviews$rating) # standard deviation

# find 95% confidence interval
x2 - qt(.975, n2-1) * (sd2/sqrt(n2))
x2 + qt(.975, n2-1) * (sd2/sqrt(n2))
# Output: 3.073829 and 3.138837

# Top 10 reviewers seem to give lower reviews, on average, than the total reviewer population.

# 4. Which movies were the top-10 based on of number of times reviewed?
# =====================================================================

# group by movie_id and count frequencies
movies.count <- summarise(group_by(reviews, movie_id), times_reviewed=n())

# sort by frequency (highest to lowest)
movies.count.sorted <- movies.count[order(-movies.count$times_reviewed), ]

# get top 10
movies10 <- movies.count.sorted[1:10,]

# merge with movies to get movie titles
movies10.named <- merge(movies10, movies[, c("movie_id", "movie_title")], by="movie_id")
movies10.named[,c("movie_title", "times_reviewed")]
# Output:
# movie_title times_reviewed
# 1               Toy Story (1995)            452
# 2               Star Wars (1977)            583
# 3                   Fargo (1996)            508
# 4  Independence Day (ID4) (1996)            429
# 5      Return of the Jedi (1983)            507
# 6                 Contact (1997)            509
# 7    English Patient, The (1996)            481
# 8                  Scream (1996)            478
# 9               Liar Liar (1997)            485
# 10          Air Force One (1997)            431

# 5. Which genres occurred most/least often, based on the number of reviews? 
# ==========================================================================

# merge movie titles and genres into movies.count.sorted
movies.genre <- merge(movies.count.sorted,
                      movies[, c("movie_id", "movie_title", "Action","Adventure","Animation"
                                 ,"Children","Comedy","Crime","Documentary"
                                 ,"Drama","Fantasy","Film-Noir","Horror"
                                 ,"Musical","Mystery","Romance","Sci_Fi"
                                 ,"Thriller","War","Western")],
                      by="movie_id")

# multiply every genre column with times_reviewed column
movies.genre.multiplied <- sapply(movies.genre[c("Action","Adventure","Animation"
                      ,"Children","Comedy","Crime","Documentary"
                      ,"Drama","Fantasy","Film-Noir","Horror"
                      ,"Musical","Mystery","Romance","Sci_Fi"
                      ,"Thriller","War","Western")], function(x) movies.genre$times_reviewed * x )

# sum the columns to get the total number of times each genre was reviewed
genre.totals <- colSums(movies.genre.multiplied)
genre.totals
# Output:
# Action   Adventure   Animation    Children      Comedy       Crime Documentary       Drama     Fantasy   Film-Noir 
# 25589       13753        3605        7182       29832        8055         758       39895        1352        1733 
# Horror     Musical     Mystery     Romance      Sci_Fi    Thriller         War     Western 
# 5317        4954        5245       19461       12730       21872        9398        1854 

# Genre with most occurences: Drama
# Genre with fewest occurences: Documentary

