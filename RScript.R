##Step 1 initial exploratory analysis

#Load data

df<- read.csv("https://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv")

#Take a look at the data: 

View(df)

#Load library:

install.packages("tidyverse")

#Import library

library(tidyverse)

#name() is used to get a list of column names
names(df)

# str() check the data types, it gives us info about the data set
str(df)

#sapply() gives the same info as str() however sapply() only gives datatype for each column name
sapply(df, class)


#To get info on the number of colmns and rows 
dim(df) #so there are 74 rows and 8 columns
#We can get the same information separately
nrow(df) #using the nrow(), provides info on the number of rows 
ncol(df) ##using the ncol(), provides info on the number of columns 


#what are the names of the top 10 films?
head(df["Film"], 10)

## Step 2 - Cleaning the data

# to check for missing values, is.na() is used
colSums(is.na(df)) #the results provide 1 missing value in Audience.score.. and Rotten.Tomatoes and 3 missing values in Profitability


#Deleting missing values
df <- na.omit(df) #running this code has deleted 4 rows from our dataset and saved the new results into df

# To check to make sure that the rows have been removed, the same code has been rerun
colSums(is.na(df))

#I have noticed that row 39 - film name "No reservation", there is a string with missing value in column 'lead studio'. 
#Upon checking the movie name "No reservation", I have found that the Lead studio is 'Warner Bros.'
#Therefore, I am going to add this value 

df[38,3] <- "Warner Bros."


##Filtering - Finding statisticial information from the data set

# summary() will give the statistics of the data set
summary(df)

#What is the average for Worldwide Gross?
mean_worldwide_gross <- mean(df$Worldwide.Gross)

#What is the lowest Worldwide Gross?
min_worldwide_gross <- min(df$Worldwide.Gross)

#What is the Maximum Worldwide Gross?
max_worldwide_gross <-max(df$Worldwide.Gross)

#which films have more than 500.50 worldwide gross?
filter(df, Worldwide.Gross > 500.50) #I have used the filter() function to find this information. 

#which film's audience score is 85 or above?
subset(df, subset = Audience..score.. >= 85) #subset() function is used the same way as filter()

#which films are above the average worldwide gross?
#count() function counts the number of films above the worldwide gross average
count(filter(df, Worldwide.Gross > mean_worldwide_gross))


##Tomorrow from here

##Step 3 - Exploratory Data Analysis
#Summary Statistics:
summary(df)

library(ggplot2)

#scatterplot
ggplot(df, aes(x=Lead.Studio, y=Rotten.Tomatoes.., color = Genre)) + geom_point()+ 
  scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+ 
  theme(axis.text.x = element_text(angle = 90)) + labs(title = "Rotten Tomatoes Ratings by Lead Studio and Genre",
                 x = "Lead Studio",
                 y = "Rotten Tomatoes Ratings"
          )

#bar chart
ggplot(df, aes(x=Year, fill = "#964B00")) + geom_bar() + labs(title = "Films by year",
                                            x = "Year",
                                            y = "Number of films"
)

#Pie Chart
genre_count <- table(df$Genre)
pie(genre_count, labels = df$Genre, main = "Film genres")


#Tree map
install.packages("treemap")
library(treemap)

tm <- treemap(
  df,
  index = "Genre",
  vSize = "Profitability",
  title = "Genre by Profitability"
)

#To display the treemap
print(tm)


#Export clean data
write.csv(df, "clean_df.csv")




