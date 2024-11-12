#Author: Anthonyo Kinch-Rice 
#Purpose: Final Project Code 
#Title: Horror and Histograms

#Reading in our data sets 

##The below data set is used for the majority of the visuals. 

fname <- file.choose("C:\\Users\\akinc\\Documents\\horror_movies")

horror <- read.csv(file=fname, header = TRUE, 
                   stringsAsFactors = FALSE) 


##The below data set was used solely to create the horror movie directors graph

i_horror <- file.choose()  

imdb_horror <- read.csv(file=i_horror, header = TRUE, 
                        stringsAsFactors = FALSE)  

####Movie release months and release years graphs 

#Install and library packages 

library(lubridate)

#Release by year

par(bg = "gray")
barplot(table(year(horror$release_date)), xlab = "Release Year", 
        main = "Horror Film Global Releases by Year (1950 - 2022)", 
        ylab = "Total Number of Films Released",
        col = c("red", "black", "darkorange1"))


#Release by month
barplot(table(month(horror$release_date)), ylab = "Number of Titles Released", 
        main = "Horror Film Global Releases by Month (1950-2022)", 
        xlab= "Month",
        col = c("red", "black", "purple"))

#Barplot of directors with most horror film projects 
#First I munged the data to get an idea of which directors were most prevalent 

sort(imdb_horror$Director, decreasing = TRUE)

#Then I subsetted data based on the directors with the most films 

carpenter_sub <- subset(imdb_horror$Director, 
                        imdb_horror$Director == "John Carpenter") 

craven_sub <- subset(imdb_horror$Director, 
                     imdb_horror$Director == "Wes Craven") 

cronenberg_sub <- subset(imdb_horror$Director, 
                         imdb_horror$Director == "David Cronenberg") 

deltoro_sub <- subset(imdb_horror$Director, 
                      imdb_horror$Director == "Guillermo del Toro")

wan_sub <- subset(imdb_horror$Director, 
                  imdb_horror$Director == "James Wan")  

#Next, I created a subset with all those directors combined 

dir_sub <- c(carpenter_sub, craven_sub, cronenberg_sub, deltoro_sub, wan_sub)

#Finally graphed the results 

barplot(table(dir_sub), main= "Directors with Most Horror Film Projects", 
        xlab = "Director", ylab = "Number of Films", 
        col =c("purple", "mediumorchid3", "magenta4",
               "mediumorchid", "darkmagenta"))
par(bg="gray")

## highest rated directors  
#Install the needed package

library(dplyr)

#Subset our data and find averages 
avg_data <- imdb_horror %>%
  group_by(Director) %>%
  summarise(avg_value = mean(Rating, na.rm = TRUE))

#sort the data
sorted_avg_data <- avg_data[order(-avg_data$avg_value), ]

#Further sort. We only want the top results
top_5_directors <- sorted_avg_data[1:5, ]

par(bg="gray")

barplot(top_5_directors$avg_value, 
        names.arg = top_5_directors$Director, 
        main = "Top 5 Directors by Average Rating",
        xlab = "Director", 
        ylab = "Average Rating", 
        col = c("orange","darkorange", "darkorange1","darkorange3"))

##Creating a word cloud for the film taglines 

#Install and library the following packages: 

library(tm)  

library(wordcloud)

#cleaned up the words. First, removed punctuation

horror_tdm <- removePunctuation(horror$tagline)

#Instead of removing stop words or using the ggplot library, i  removed "the" 

horror_tdm2 <- removeWords(horror_tdm, "the") 

#Still was getting "the" in my word cloud. Added the removal of "The" 

horror_tdm3 <- removeWords(horror_tdm2, "The")

##Graph the word cloud 

par(bg="black")
wordcloud(horror_tdm3, scale = c(4, .4), min.freq = 3, max.words = 500, 
          random.order = FALSE, random.color = FALSE, 
          rot.per = 0, colors =c("purple", "firebrick1", "darkorange1")) 

####Regression model 

#Create a linear model

model <- lm(horror$revenue ~ horror$vote_count, data = horror) 

#Analyze liner model results and determine if R and P values are significant

summary(model) 

#Plot linear model 

par(bg="gray")

plot(horror$vote_count, horror$revenue, pch =21, bg="mediumpurple4", 
     main = "Correlation Between Revenue and Vote Count", 
     xlab= "Vote Count", ylab= "Revenue (US Dollars)")  

#Add a regression line

abline(model, col = "orange", lwd = 4)
#Add a legend

legend("topleft", legend=c("Regression Line"), 
            col=c("orange"), lty=1)
 
##histogram based on film runtimes

hist(horror$runtime, xlim = c(0,200), main = "Film Runtime Histogram",  
     xlab = "Runtime (Minutes)", col= c("purple","purple2", "purple3", "purple4"))

####Pass in our final data set 
fname3 <- file.choose("C:\\Users\\akinc\\Documents\\movie_profit_ditrib")

#Our data set below has multiple film genres, we will utilize this information later

distrib <- read.csv(file=fname3, header = TRUE, 
                   stringsAsFactors = FALSE) 

#Subset only horror films 
horror_films <- distrib[distrib$genre == "Horror", ] 


#####Domestic gross treemap

#library the following: 
library(treemap)

######world wide gross tree map 

treemap(horror_films, index = c("distributor"),
        vSize = "production_budget", 
        vColor = "worldwide_gross", 
        type = "dens",
        fontsize.labels = 12, 
        palette = "Oranges")
####Distributors and MPAA ratings alluvial

##library the following: 

library(alluvial)

library(dplyr)

library(ggplot2)

library(ggalluvial) 

###make a 10-color color palette

alluvial_colors <- c( "red", "darkred", "orange", "darkorange","darkorange3", 
                     "chartreuse3", "chartreuse4", "darkorchid3", "darkmagenta", "black")

#filter our data 
filtered_data <- horror_films %>%
  select(distributor, mpaa_rating) %>%
  filter(!is.na(distributor), !is.na(mpaa_rating)) %>%
  group_by(distributor, mpaa_rating) %>%
  summarise(count = n()) %>%
  ungroup()

#further filtering, selecting only top 12 distributors

top_distributors <- horror_films %>%
  filter(!is.na(distributor)) %>%
  count(distributor, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(distributor) 

filtered_data_top <- filtered_data %>%
  filter(distributor %in% top_distributors)

ggplot(data = filtered_data_top,
       aes(axis1 = distributor, axis2 = mpaa_rating, y = count)) +
  geom_alluvium(aes(fill = distributor), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Distributor", "MPAA Rating"), expand = c(.05, .05)) +
  labs(title = "Distribution of MPAA Ratings by Distributor for Horror Films",
       y = "Number of Films") + 
  scale_fill_manual(values = alluvial_colors) +
  theme_minimal() 

####Genre percentage pie chart 

my_pallette <- colorRampPalette(c("orange3", "purple4", "orange3", "purple4", "red"))

genre_counts <- table(distrib$genre)

genre_percentages <- round(100 * genre_counts / sum(genre_counts), 1)

genre_labels <- sprintf("%s - %s%%", names(genre_counts), genre_percentages)

par(bg= "gray")

pie(genre_counts, labels = genre_labels, col = my_pallette(length(genre_counts)),
    main = "Film Genre Distribution (1946-2016)")

####other visuals that did not make the final poster
  
##Scatter plots  

plot(horror$vote_count, horror$vote_average, xlab = "Vote Count", 
     ylab="vote Average", pch = 21, bg = "magenta", 
     main ="What affect does vote count have on the average nummber of votes?") 

plot(horror$budget, horror$revenue, xlab= "Budget (US Dollars)", 
     ylab = "Revenue (US Dollars)", 
     main = "Does a larger budget equate to a larger revenue?", 
     pch = 21, bg= "orange")

plot(horror$vote_average, horror$budget, xlab="Budget", 
     ylab = "Vote Average", pch = 21, bg ="red", 
     main = "Do Higher Budget Films Have a Higher Vote Average?")

#Horror movie run times box plot
boxplot(horror$runtime, horizontal =TRUE, col = "red", 
        xlab = "Runtime (Minutes)", 
        main = "Movie Run Times") 

##domestic gross tree map 
treemap(horror_films, index = c("distributor"),
        vSize = "production_budget", 
        vColor = "domestic_gross", 
        type = "dens",
        fontsize.labels = 12, 
        palette = "Purples") 

