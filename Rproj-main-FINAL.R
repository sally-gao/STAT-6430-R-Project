# Assignment:     R Project (STAT 6430)
# Names:          Sally Gao, Stephen Mortensen, Kennan Grant
# Computing IDs:  sg2zv, sam8sp, khg3je

# load libraries
library(tidyverse)
library(stringr)


# Reviews -----------------------------------------------------------------

# read in reviews.  
reviews <- read_delim("reviews.txt"
                      , delim="\t"
                      , col_names=c("reviewer_id","movie_id","rating","timestamp")
)

# convert timestamp to POSIXct datetime format
reviews[["timestamp"]] <- as.POSIXct(reviews[["timestamp"]]
                                     , tz="GMT",origin="1970-01-01") 

# create primary key: review_id 
reviews <- rowid_to_column(reviews, "review_id")



# Movies ------------------------------------------------------------------

# reads in movies.
# primary key:  movie_id
movies <- read_delim("genres.txt"
                     , delim="|"
                     , col_names=c("movie_id","movie_title","release_date"
                                   ,"DROP_THIS_COL","IMDb_URL"
                                   ,"unknown","Action","Adventure","Animation"
                                   ,"Children","Comedy","Crime","Documentary"
                                   ,"Drama","Fantasy","Film-Noir","Horror"
                                   ,"Musical","Mystery","Romance","Sci_Fi"
                                   ,"Thriller","War","Western"))


# Reviewers ---------------------------------------------------------------

# reads in reviewers.  
# primary key:  reviewer_id
reviewers <- read_delim("reviewers.txt"
                        , delim="|"
                        , col_names=c("reviewer_id","age","gender"
                                      ,"occupation","zip_code"))



# Zip Codes ---------------------------------------------------------------

# reads in zip codes.
# primary key:  zip_code
zip_codes <- read_csv("zipcodes.csv"
                      , col_names=c("zip_code","zip_code_type","City","State"
                                    ,"location_type","Lat","Long"
                                    ,"Location","Decommisioned"))


library(modes)
library(lubridate)
library(zipcode)

# Join all available data to reviews --------------------------------------

normalized_base_table <- reviews %>% 
  left_join(movies, by = "movie_id") %>% 
  left_join(reviewers, by = "reviewer_id") %>% 
  left_join(zip_codes, by = "zip_code")


# Rating Distribution Overview --------------------------------------

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


# Note that we checked to make sure that it was *NOT* the case that many
# reviewers were 1-review reviewers, which would have made our prediction
# technique useless going forward.  In fact, the minimum number of reviews
# of any reviewer in this dataset is 20.


# Timestamp ---------------------------------------------------------

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
  arrange(desc(perc_high_rating))  ## But reviewer numbers small..


# City --------------------------------------------------------------------

# City is very predictive, but a closer look reveals it is single reviewers
normalized_base_table %>% 
  group_by(City) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  #as.data.frame() %>% 
  arrange(desc(perc_high_rating))



### APPENDIX


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

# join with movies to get movie titles (used join instead of merge to preserve movies10 order)
movies10.named <- plyr::join(movies10, movies[, c("movie_id", "movie_title")], by="movie_id")
movies10.named[,c("movie_title", "times_reviewed")]

# Output:
# movie_title times_reviewed
# 1               Star Wars (1977)            583
# 2                 Contact (1997)            509
# 3                   Fargo (1996)            508
# 4      Return of the Jedi (1983)            507
# 5               Liar Liar (1997)            485
# 6    English Patient, The (1996)            481
# 7                  Scream (1996)            478
# 8               Toy Story (1995)            452
# 9           Air Force One (1997)            431
# 10 Independence Day (ID4) (1996)            429

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


# Merging all files together - 
# Using left join to maintain movies as our base table, row count before and after matches.
master1 <- left_join(reviews, reviewers, by="reviewer_id")
master2 <- left_join(master1, movies, by="movie_id")
master <- left_join(master2, zip_codes, by="zip_code")

# 6. 
# Summing all genre columns by row, calculation proportion with sum > 1
master$totGenres <- rowSums(master[,c(14:32)])
genresPercent <- nrow(master[master$totGenres > 1,])/nrow(master) * 100
genresPercent
# 69.938%

# 7. 
# Subsetting by gender
fRvwrs <- master[master$gender == "F",]
fRvwrs$gender

mRvwrs <- master[master$gender =="M",]
mRvwrs$gender

# Function for confidence interval
t.int.conf.test <- function(set, count, conf.level) {
  return(c(mean(set) - abs(qt((1-conf.level)/2, count-1)) * (sd(set)/sqrt(count)), mean(set) + abs(qt((1-conf.level)/2, count-1)) * (sd(set)/sqrt(count))))
}

fRtngConf <- t.int.conf.test(fRvwrs$rating, nrow(fRvwrs), 0.95)
fRtngConf
# 3.517202 3.545813

mRtngConf <- t.int.conf.test(mRvwrs$rating, nrow(mRvwrs), 0.95)
mRtngConf
# 3.521309 3.537269

# 8. 
# Fill in Canada and unknown values for zip codes
master$State[grepl("[A-Z]",master$zip_code)] <- 'Canada'
master$State[is.na(master$State)] <- 'unknown'

# Group to count up number of reviews by state
by_region <- group_by(master, State)

countRegion <- summarise(by_region, totRvw = length(rating))
attach(countRegion)
countRegion <- countRegion[order(-totRvw),]
top5States <- head(countRegion[,c('State','totRvw')],5)
top5States
#   State totRvw
#   <chr>  <int>
# 1    CA  13842
# 2    MN   7635
# 3    NY   6882
# 4    IL   5740
# 5    TX   5042

# 9.
# Group by movie_id to find the number of movies and the number of ratings per rating
by_movie <- group_by(master, movie_id)
movieNumRvws <- summarise(by_movie, totRvw = length(rating))
by_numRvws <- group_by(movieNumRvws, totRvw)
movieNumNums <- summarise(by_numRvws, totNum = length(totRvw))
totMovies <- nrow(movieNumRvws)
movieNumNums$percent <- movieNumNums$totNum/totMovies*100
attach(movieNumNums)
movieNumNums <- movieNumNums[order(-totNum),]
movieNumNums
#    totRvw totNum  percent
#     <int>  <int>    <dbl>
#  1      1    141 8.382878
#  2      2     68 4.042806
#  3      4     64 3.804994
#  4      3     60 3.567182
#  5      5     51 3.032105
#  6      7     44 2.615933
#  7      6     39 2.318668
#  8      9     33 1.961950
#  9     10     33 1.961950
# 10      8     30 1.783591
# ... with 263 more rows

# 10.

# Calculate average review for each genre
meanGenre = data.frame(Genre = NA, MeanRating = NA)

j <- 0

for (i in 14:32) {
  print(i)
  meanGenre[j,] <- c(colnames(master[i]), mean(master$rating[master[,i]==1]))
  j <- j+1
}
meanGenre

# Sort by mean review, pull out top and bottom
attach(meanGenre)
meanGenre <- meanGenre[order(MeanRating),]
meanGenre
hiAvgRvw <- tail(meanGenre, 1)
hiAvgRvw
#     Genre       MeanRating
# Film-Noir 3.92152336987882
loAvgRvw <- head(meanGenre, 1)
loAvgRvw
#   Genre       MeanRating
# Fantasy 3.21523668639053

# 11.
# Segmenting older reviewers, calculating mean ratings for each genre
oldMeanGenre = data.frame(Genre = NA, MeanRating = NA)

oldMaster <- master[master$age > 30,]

j <- 0

for (i in 14:32) {
  print(i)
  oldMeanGenre[j,] <- c(colnames(oldMaster[i]), mean(oldMaster$rating[oldMaster[,i]==1]))
  j <- j+1
}
oldMeanGenre

# Sorting older reviewer means and pulling out top and bottom
attach(oldMeanGenre)
oldMeanGenre <- oldMeanGenre[order(MeanRating),]
oldMeanGenre
hiAvgRvw <- tail(oldMeanGenre, 1)
hiAvgRvw
#     Genre       MeanRating
# Film-Noir 3.99805447470817
loAvgRvw <- head(oldMeanGenre, 1)
loAvgRvw
#  Genre      MeanRating
# Horror 3.28520017993702

# Same as above for younger reviewers
yngMeanGenre = data.frame(Genre = NA, MeanRating = NA)

yngMaster <- master[master$age <= 30,]

j <- 0

for (i in 14:32) {
  print(i)
  yngMeanGenre[j,] <- c(colnames(yngMaster[i]), mean(yngMaster$rating[yngMaster[,i]==1]))
  j <- j+1
}
yngMeanGenre

attach(yngMeanGenre)
yngMeanGenre <- yngMeanGenre[order(MeanRating),]
yngMeanGenre
hiAvgRvw <- tail(yngMeanGenre, 1)
hiAvgRvw
#     Genre       MeanRating
# Film-Noir 3.80992907801418
loAvgRvw <- head(yngMeanGenre, 1)
loAvgRvw
#   Genre       MeanRating
# Fantasy 3.09078590785908










## GRAPHS --------------

# Define prop diff interval funtion for testing significance ------------------
  propdiffint <- function(df){
    propdifftable <- as.data.frame(matrix(NA, nrow=nrow(df), ncol=3))
    names(propdifftable) <- c("category", "lowerbound", "upperbound")
    for(i in (1:nrow(df))){
      p1 <- df$perc_high_rating[i]/100
      p2 <- 0.55375
      n1 <- df$n[i]
      n2 <- 100000
      zconf <- 0.95
      propdiff <- p1-p2
      error <- qnorm(zconf + (1-zconf)/2) * sqrt((p1*(1-p1))/n1+(p2*(1-p2))/n2)
      propdifftable[i,] <- c(df[i,1], propdiff-error, propdiff+error)
    }
    return(propdifftable)
  }

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

# Find the proportional difference 95% confidence interval
propdiffint(high_mode_ratings)
# 0.08481171 0.09431742 - Looks significantly higher

# Create dataframe for graphing
mode_ratings <- as.data.frame(matrix(NA, nrow=1,ncol=3))
mode_ratings[1,] <- c("Mode","High Mode", 8.956)
mode_ratings[2,] <- c("Mode","Low Mode", -18.43583)
names(mode_ratings) <- c("Analysis","Category", "Prop")

mode_ratings$Prop <- as.double(mode_ratings$Prop) # Ensure y-axis is continuous numeric

# Graph it!
ggplot(data=mode_ratings, aes(x=Category, y=Prop)) +
  geom_bar(stat="identity", fill="indianred") +
  geom_hline(yintercept = 0) +
  labs(x="Mode Rating of Reviewer", 
       y="Prob of High Rating Compared to Baseline", 
       title="Probability of High Rating by Mode Rating of Reviewer") +
  annotate("text", min(mode_ratings$Category), 0, label = "Baseline Probability
           of High Rating (55.375%)")


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

propdiffint(time_ratings1)

#    hour   lowerbound   upperbound
# 1    16  0.049797451  0.076392191
# 2    15  0.044577257  0.079335681
# 3    12  0.017586228  0.079934693
# 4     0  0.024346110  0.051835251
# 5    13  0.013873393  0.061538761
# 6    14  0.012117219  0.047102374
# 7    21  0.018460871  0.040685448
# 8     3  0.007940936  0.036423890
# 9     2  0.006204897  0.035304578
# 10   17  0.004187753  0.029439995
# 11   22  0.003088988  0.026736416
# 12   18 -0.014560548  0.009947298
# 13   20 -0.019288279  0.006121878
# 14    1 -0.022631314  0.005296845
# 15    8 -0.047206766  0.011198381
# 16   23 -0.041398925 -0.014710720
# 17   19 -0.044530089 -0.020506469
# 18    5 -0.048423290 -0.017549263
# 19   11 -0.079342762 -0.001469640
# 20    4 -0.067688514 -0.036985296
# 21    6 -0.076883243 -0.043188186
# 22    7 -0.094483974 -0.044184858
# 23   10 -0.145866736 -0.089059425
# 24    9 -0.144878581 -0.100499430
# Numerous examples without 0 in interval - solidly predictive

# Create graphing dataframe
time_ratings <- as.data.frame(matrix(NA, nrow=24,ncol=3))
time_ratings[,1] <- "Time"
time_ratings[,2] <- time_ratings1[,1]
time_ratings[,3] <- time_ratings1[,3]-55.375
names(time_ratings) <- c("Analysis","Category", "Prop")

time_ratings$Prop <- as.double(time_ratings$Prop) # Y-axis is continuous numeric

# Graph it!
ggplot(data=time_ratings, aes(x=reorder(Category, -Prop), y=Prop)) +
  geom_bar(stat="identity", fill="lightsteelblue") +
  geom_hline(yintercept = 0) +
  labs(x="Hour", y="Prob of High Rating Compared to Baseline", 
       title="Probability of High Rating by Hour of the Day") +
  annotate("text", x = 7, 0, label = "Baseline Probability
           of High Rating (55.375%)")


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

propdiffint(date_ratings1)

#    category  lowerbound   upperbound
# 1         1  0.02479773  0.034333488
# 2        NA -0.32284704  0.326458154
# 3        11 -0.02268387  0.020026368
# 4        10 -0.03671357  0.005850817
# 5         7 -0.04025561 -0.001689251
# 6         2 -0.05585311 -0.023466750
# 7        12 -0.07870244 -0.043498842
# 8         9 -0.09681865 -0.047990215
# 9         6 -0.09922526 -0.059780288
# 10        4 -0.10347669 -0.065368471
# 11        3 -0.10840282 -0.076300195
# 12        5 -0.17090514 -0.129255407
# 13        8 -0.17558169 -0.137922065
# January is significant for a slightly higher rating, but the difference is small.

# Dataframe for graphing
date_ratings <- as.data.frame(matrix(NA, nrow=13,ncol=3))
date_ratings[,1] <- "Date"
date_ratings[,2] <- date_ratings1[,1]
date_ratings[,3] <- date_ratings1[,3]-55.375
names(date_ratings) <- c("Analysis","Category", "Prop")

date_ratings$Prop <- as.double(date_ratings$Prop) #Y-axis numeric

#Graph it!
ggplot(data=date_ratings, aes(x=reorder(Category, -Prop), y=Prop)) + 
  geom_bar(stat="identity", fill="lightseagreen") +
  geom_hline(yintercept = 0) +
  labs(x="Month", y="Prob of High Rating Compared to Baseline", 
       title="Probability of High Rating by Month of Year") +
  annotate("text", x = 3.2, 0, label = "Baseline Probability
           of High Rating (55.375%)")


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
genre_rating

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

# Create dataframe for producing proportion difference confidence interval
genre_loop <- genre_rating[,-3,drop=FALSE]
genre_loop[,3] <- genre_loop[,3]+55.375
names(genre_loop) <- c("genre", "n", "perc_high_rating")

propdiffint(genre_loop)
#   category        lowerbound        upperbound
# 1 FilmNoir 0.130875756445984 0.174203585735204
# 2      War 0.101150638410004 0.121206778061585
# Significant

genre_rating

# Dataframe for graphing
genre_ratings <- as.data.frame(matrix(NA, nrow=2,ncol=3))
genre_ratings[,1] <- "Genre"
genre_ratings[,2] <- genre_rating[,1]
genre_ratings[,3] <- genre_rating[,4]
names(genre_ratings) <- c("Analysis","Category", "Prop")

genre_ratings$Prop <- as.double(genre_ratings$Prop) # y-axis numeric

# Graph it!
ggplot(data=genre_ratings, aes(x=Category, y=Prop)) + 
  geom_bar(stat="identity", fill="peachpuff") +
  geom_hline(yintercept = 0) +
  labs(x="Genre", y="Prob of High Rating Compared to Baseline", 
       title="Probability of High Rating by Genre (War or Film-Noir)") +
  annotate("text", x = .5, y = 1, 0, label = "Baseline Probability
           of High Rating (55.375%)")



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
state_loop <- state_ratings1[,-3,drop=FALSE]

propdiffint(state_loop)
#    category           lowerbound           upperbound
# 1        WV    0.319883740691149    0.417443845515748
# 2        SD    0.248418908617307    0.438952886254488
# 3        ME     0.17181425278021     0.31418168217914
# 4        KS    0.176193606723535    0.258178314458731
# 5        MT    0.142692202385206    0.265936829872859
# 6        AL    0.107889014245707    0.220094856722035
# 7        MS   0.0865365030610542    0.200385807695918
# 8        MA   0.0805016254172948    0.117725928762272
# 9        NH   0.0643555949250947     0.12650506081261
# 10       WI   0.0641110684556013    0.109061200451962
# 11       IA   0.0600826234968391    0.107779695343741
# 12       CT   0.0476701399127813   0.0938141897447927
# 13       IN   0.0323449176183418   0.0927594561391135
# 14       TN   0.0338755263102562   0.0891385698754818
# 15       UT   0.0280718883781055   0.0919530838749467
# 16       WA   0.0395822154934287   0.0802712480944931
# 17       MO   0.0313331552507964   0.0726006015550112
# 18       FL   0.0271866503431936   0.0743663573995104
# 19       OK   0.0191413893709868   0.0812588185292212
# 20       NM -0.00173354675115735   0.0789025362981958
# 21       NE  -0.0048913216272636   0.0792095034454456
# 22       NV  -0.0227048589967832   0.0956559868163322
# 23       LA -0.00591135363696548   0.0674657948977107
# 24       AK  -0.0165487376032174    0.076096792523126
# 25       PA  -0.0026971711226658   0.0314655149381795
# 26       NC -0.00790667010420604   0.0358648374348835
# 27       CA  0.00238235844745951   0.0200121654652699
# 28       VT  -0.0439045670035523   0.0500936621311625
# 29       MI   -0.019458580522679   0.0203530385503889
# 30       OH  -0.0200701069470019   0.0135773011916062
# 31       DC   -0.033487250475881   0.0188292065354133
# 32       MN  -0.0211342078327396  0.00204285223352553
# 33       NJ  -0.0341355749079249   0.0135920966470553
# 34       CO  -0.0352565094932863   0.0051947681789289
# 35       OR  -0.0389979084822292  0.00789858759937692
# 36       GA  -0.0360859861666829  0.00497787222624282
# 37       TX   -0.044671329581928  -0.0164185157175564
# 38       VA  -0.0504513550229238  -0.0114887994172305
# 39       WY   -0.174275625619105    0.108442292285772
# 40       KY  -0.0646785355030457 -0.00124142291691263
# 41       NY  -0.0498232126779201  -0.0254187228059508
# 42       MD  -0.0685275296089424  -0.0305752451263624
# 43       AZ  -0.0778466122165712  -0.0262357527868463
# 44     <NA>  -0.0728733800624952  -0.0424715668687462
# 45       RI   -0.111643305429601  -0.0168357155494199
# 46       IL  -0.0783675654006878  -0.0517805182230056
# 47       ID  -0.0889779045778442  -0.0423977200751263
# 48       HI   -0.127348977678907  -0.0137080693009585
# 49       SC    -0.10083580528378  -0.0534039523619214
# 50       AE   -0.291188747933092    0.136069700314044
# 51       AP   -0.191941426616851 -0.00127285909743474
# 52       DE   -0.192306249370369 -0.00610284153872154
# 53       AR   -0.320600288577238   0.0592541347310841
# 54       ND   -0.218686108283944  -0.0922770951792588
# Extremely predictive

# Dataframe for graphing
state_ratings <- as.data.frame(matrix(NA, nrow=54,ncol=3))
state_ratings[,1] <- "State"
state_ratings[,2] <- state_ratings1[,1]
state_ratings[,3] <- state_ratings1[,4]-55.375
names(state_ratings) <- c("Analysis","Category", "Prop")

# Keep only the top 10
state_ratings <- state_ratings[1:10,]
state_ratings$Prop <- as.double(state_ratings$Prop) # Y-axis numeric

# Graph it!
ggplot(data=state_ratings, aes(x=reorder(Category, -Prop), y=Prop)) +
  geom_bar(stat="identity", fill="lightsalmon") +
  geom_hline(yintercept = 0) +
  labs(x="State", y="Prob of High Rating Compared to Baseline", 
       title="Probability of High Rating by State (Top 10)") +
  annotate("text", x = 2.1, y = 2, 0, label = "Baseline Probability
           of High Rating (55.375%)")


# City --------------------------------------------------------------------

# City is very predictive, but a closer look reveals it is single reviewers
normalized_base_table %>% 
  group_by(City) %>% 
  summarise(n = n()
            , unique_reviewers = n_distinct(reviewer_id)
            , perc_high_rating = sum(rating == 4 | rating == 5)/n*100) %>% 
  #as.data.frame() %>% 
  arrange(desc(perc_high_rating))

# Combined Visuals --------------------------------------------------------

# Create a combined table across all analyses for combined visualization
mastergraph <- as.data.frame(rbind(mode_ratings, 
                                   time_ratings,
                                   date_ratings,
                                   genre_ratings,
                                   state_ratings))
mastergraph$Prop <- as.double(mastergraph$Prop) # y-axis numerical

# Graph it!
ggplot(data=mastergraph, aes(x=reorder(Category, -Prop), y=Prop))+
  geom_bar(fill='indianred', stat="identity")+
  geom_hline(yintercept=0)+
  facet_wrap( ~ Analysis, scales="free_x")+
  labs(x="Category", y="Prob of High Rating Compared to Baseline", 
       title="Probability of High Rating by:", 
       subtitle="Month in Year, Genre, Mode, State, and Time of Day")



