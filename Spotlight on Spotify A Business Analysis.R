#------ Data Collection -------#

#Setting directory, importing all the library reading csv file, checking summary, head and tail of the dataframe
getwd()
setwd("C:\\Users\\hardi\\Downloads")

install.packages("ggplot")
install.packages("gplots")

library(dplyr)
library(ggplot2)
library(gplots)
library(gplots)

spotifyog <- read.csv("songs_normalize.csv")
View(spotifyog)

summary(spotifyog)
str(spotifyog)

head(spotifyog)
tail(spotifyog)


#------ Data Cleaning -------#


#Checking for duplicates
duplicated(spotifyog)
sum(duplicated(spotifyog)) #89 Duplicates found


#Cleaning the duplicates
spotifyds <- distinct(spotifyog) #Removing the duplicates
sum(duplicated(spotifyds)) #0 Duplicates found


#Checking for incorrect/uneven values
genretable <- table(spotifyds$genre)
genretable

cdf <- spotifyds[!(spotifyds$genre=="set()"),] #values with set() genre in the cell block removed from df
cdf

#Cleaned data for genre of music
cleaned <- table(cdf$genre) 
cleaned

#Barplot for uncleaned and cleaned genre
barplot(height = genretable, ylim = c(0, 500), 
        col = "lightgreen",
        main = "Uncleaned Genre", 
        xlab = "Genre", ylab = "Frequency")

barplot(height = cleaned, ylim = c(0, 500),
        col = "lightgreen",
        main = "Cleaned Genre", 
        xlab  = "Genre", ylab = "Frequency")


#------ Data Analysis -------#


#Popularity-wise spread
hist(spotifyds$popularity, 
     main = "Popularity Frequency Spread", 
     xlab = "Popularity", 
     col = "lightblue",
     xlim = c(0,100), ylim = c(0,800))

#Year-wise spread
hist(spotifyds$year, 
     xlim = c(1995, 2020), ylim = c(0,250), 
     col = "lightblue",
     main = "Year-wise Spread", 
     xlab = "Year")

#Mean of hits per year
meanperyear <- table(spotifyds$year)
mean(meanperyear)


#Top 10 artists featuring max number of times
topartists <- table(spotifyds$artist)
topartists
View(topartists)

top10artists <- head(sort(topartists, decreasing = TRUE), n=10)
top10artists


#Barplot for the top 10 artists
barplot(top10artists,
        las = 2, 
        col = "lightpink", 
        cex.names = 0.7)


#Pie chart for Explicit content, % wise
explicitcontent <- table(spotifyds$explicit)
explicitcontent

pie_labels <- paste0(round(100 * explicitcontent/sum(explicitcontent), 2), "%") #Converting the count to percentages
pie(explicitcontent, 
    labels = pie_labels, 
    main = "Non-Explicit vs Explicit")

legend("topleft", 
       legend = c("Non-Explicit", "Explicit"), 
       fill =  c("white", "lightblue"))


#------ Data Correlations -------#


#Scatterplot for duration vs popularity 
plot(x= spotifyds$duration_ms, y=spotifyds$popularity, 
     col= rgb(1, 0.15, 1, 0.30),
     xlab = "Duration", ylab = "Popularity",
     main = "Duration vs Popularity")


#Scatterplot same as above but with outlier removed
plot(x= spotifyds$duration_ms, y=spotifyds$popularity, 
     col= rgb(1, 0.15, 1, 0.30), 
     xlab = "Duration", ylab = "Popularity", 
     main = "Duration vs Popularity( Outliers Removed )", 
     xlim = c(150000, 3e+05), ylim = c(10, 80))


#Graph of Energy vs Tempo 
ggplot(spotifyds, 
       x=spotifyds$energy, y= spotifyds$tempo) + 
      aes(x=spotifyds$energy, y= spotifyds$tempo, col = 'red') + 
      geom_density_2d_filled() + 
      xlab("Energy") + ylab("Tempo")


#------ Additonal Data Attributes -------#


#Number of hits divided per year
hitsperyear <- table(spotifyds$year)
hitsperyear

median(hitsperyear)


#Mean density plot for the duration time of the songs
mean <- mean(spotifyds$duration_ms)
mean

den <- density(spotifyds$duration_ms)

plot(den, 
     frame = FALSE, 
     col = "blue", 
     main = "Duration Density Graph",
     xlab = "Duration", 
     ylim = c(0.0e+00, 1.5e-05))