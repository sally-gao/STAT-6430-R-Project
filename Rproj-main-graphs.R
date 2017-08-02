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

# Define prop diff interval funtion for testing -------------------------------
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

propdiffint(high_mode_ratings)
# 0.08481171 0.09431742


high_mode_ratings

mode_ratings <- as.data.frame(matrix(NA, nrow=1,ncol=3))
mode_ratings[1,] <- c("Mode","High Mode", 8.956)
mode_ratings[2,] <- c("Mode","Low Mode", -18.43583)
names(mode_ratings) <- c("Analysis","Category", "Prop")

mode_ratings$Prop <- as.double(mode_ratings$Prop) + 

ggplot(data=mode_ratings, aes(x=Category, y=Prop)) +
  geom_bar(stat="identity", fill="indianred") +
  geom_hline(yintercept = 0) +
  labs(x="Mode Rating of Reviewer", y="Prob of High Rating Compared to Baseline", title="Probability of High Rating by Mode Rating of Reviewer") +
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

time_ratings <- as.data.frame(matrix(NA, nrow=24,ncol=3))
time_ratings[,1] <- "Time"
time_ratings[,2] <- time_ratings1[,1]
time_ratings[,3] <- time_ratings1[,3]-55.375
names(time_ratings) <- c("Analysis","Category", "Prop")

ggplot(data=time_ratings, aes(x=Category, y=Prop)) +
  geom_bar(stat="identity", fill="lightsteelblue") +
  geom_hline(yintercept = 0) +
  labs(x="Hour", y="Prob of High Rating Compared to Baseline", title="Probability of High Rating by Hour of the Day") +
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
# January is significant in a slightly higher rating, but the different is small.

date_ratings1

date_ratings <- as.data.frame(matrix(NA, nrow=13,ncol=3))
date_ratings[,1] <- "Date"
date_ratings[,2] <- date_ratings1[,1]
date_ratings[,3] <- date_ratings1[,3]-55.375
names(date_ratings) <- c("Analysis","Category", "Prop")


ggplot(data=date_ratings, aes(x=Category, y=Prop)) + 
  geom_bar(stat="identity", fill="lightseagreen") +
  geom_hline(yintercept = 0) +
  labs(x="Month", y="Prob of High Rating Compared to Baseline", title="Probability of High Rating by Month of Year") +
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

genre_loop <- genre_rating[,-3,drop=FALSE]
genre_loop[,3] <- genre_loop[,3]+55.375
names(genre_loop) <- c("genre", "n", "perc_high_rating")

propdiffint(genre_loop)
#   category        lowerbound        upperbound
# 1 FilmNoir 0.130875756445984 0.174203585735204
# 2      War 0.101150638410004 0.121206778061585

genre_rating

genre_ratings <- as.data.frame(matrix(NA, nrow=2,ncol=3))
genre_ratings[,1] <- "Genre"
genre_ratings[,2] <- genre_rating[,1]
genre_ratings[,3] <- genre_rating[,4]
names(genre_ratings) <- c("Analysis","Category", "Prop")

genre_ratings

ggplot(data=genre_ratings, aes(x=Category, y=Prop)) + 
  geom_bar(stat="identity", fill="peachpuff") +
  geom_hline(yintercept = 0) +
  labs(x="Genre", y="Prob of High Rating Compared to Baseline", title="Probability of High Rating by Genre (War or Film-Noir)") +
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

state_ratings <- as.data.frame(matrix(NA, nrow=54,ncol=3))
state_ratings[,1] <- "State"
state_ratings[,2] <- state_ratings1[,1]
state_ratings[,3] <- state_ratings1[,4]-55.375
names(state_ratings) <- c("Analysis","Category", "Prop")

state_ratings <- state_ratings[1:10,]

ggplot(data=state_ratings, aes(x=reorder(Category, -Prop), y=Prop)) +
  geom_bar(stat="identity", fill="lightsalmon") +
  geom_hline(yintercept = 0) +
  labs(x="State", y="Prob of High Rating Compared to Baseline", title="Probability of High Rating by State (Top 10)") +
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


mastergraph <- rbind(mode_ratings, time_ratings,date_ratings,genre_ratings,state_ratings)
mastergraph

ggplot(data=mastergraph, aes(x=Category, y=Prop))+
  geom_bar(fill='steelblue', stat="identity")+
  facet_wrap( ~ Analysis, scales="free_x")+
  labs(x="Category", y="Prob of High Rating Compared to Baseline", title="Mode, Time of Day, Month in Year, Genre, and State", subtitle="Proportion of high ratings vs. population")
