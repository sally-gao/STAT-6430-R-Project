# Assignment:     R Project (STAT 6430)
# Names:          Sally Gao, Stephen Mortensen, Kennan Grant
# Computing IDs:  sg2zv, sam8sp, khg3je

setwd("C:/Users/smort/OneDrive/Documents/Grad School/STAT6430/Assignments/R Final Project")
getwd()

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
