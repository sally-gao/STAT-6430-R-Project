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
# R automatically recognizes the primary key by matching column names.
master <- merge(merge(merge(reviews, movies),reviewers),zip_codes)

# 6. 
master$totGenres <- rowSums(master[,c(11:29)])
genresPercent <- nrow(master[master$totGenres > 1,])/nrow(master)
genresPercent
# 69.95066%

# 7. 
fRvwrs <- master[master$gender == "F",]
fRvwrs$gender

mRvwrs <- master[master$gender =="M",]
mRvwrs$gender

t.int.conf.test <- function(set, count, conf.level) {
  return(c(mean(set) - abs(qt((1-conf.level)/2, count-1)) * (sd(set)/sqrt(count)), mean(set) + abs(qt((1-conf.level)/2, count-1)) * (sd(set)/sqrt(count))))
}

fRtngConf <- t.int.conf.test(femRvwrs$rating, nrow(femRvwrs), 0.95)
fRtngConf
# 3.526243 3.555151

mRtngConf <- t.int.conf.test(mRvwrs$rating, nrow(mRvwrs), 0.95)
mRtngConf
# 3.524903 3.541300

# 8. 
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
#  1      1    148 8.851675
#  2      2     73 4.366029
#  3      3     65 3.887560
#  4      5     57 3.409091
#  5      4     53 3.169856
#  6      7     38 2.272727
#  7      6     36 2.153110
#  8      9     36 2.153110
#  9     10     32 1.913876
# 10      8     31 1.854067
# ... with 246 more rows

# 10.
meanGenre = data.frame(Genre = NA, MeanRating = NA)

j <- 0

for (i in 11:29) {
  print(i)
  meanGenre[j,] <- c(colnames(master[i]), mean(master$rating[master[,i]==1]))
  j <- j+1
}
meanGenre

attach(meanGenre)
meanGenre <- meanGenre[order(MeanRating),]
meanGenre
hiAvgRvw <- tail(meanGenre, 1)
hiAvgRvw
#     Genre       MeanRating
# Film-Noir 3.93195625759417
loAvgRvw <- head(meanGenre, 1)
loAvgRvw
#   Genre       MeanRating
# Fantasy 3.21648690292758

# 11.
oldMeanGenre = data.frame(Genre = NA, MeanRating = NA)

oldMaster <- master[master$age > 30,]

j <- 0

for (i in 11:29) {
  print(i)
  oldMeanGenre[j,] <- c(colnames(oldMaster[i]), mean(oldMaster$rating[oldMaster[,i]==1]))
  j <- j+1
}
oldMeanGenre

attach(oldMeanGenre)
oldMeanGenre <- oldMeanGenre[order(MeanRating),]
oldMeanGenre
hiAvgRvw <- tail(oldMeanGenre, 1)
hiAvgRvw
#     Genre       MeanRating
# Film-Noir 4.00411099691675
loAvgRvw <- head(oldMeanGenre, 1)
loAvgRvw
#  Genre      MeanRating
# Horror 3.2824644549763

yngMeanGenre = data.frame(Genre = NA, MeanRating = NA)

yngMaster <- master[master$age <= 30,]

j <- 0

for (i in 11:29) {
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
# Film-Noir 3.82763744427935
loAvgRvw <- head(yngMeanGenre, 1)
loAvgRvw
#   Genre       MeanRating
# Fantasy 3.08507670850767
