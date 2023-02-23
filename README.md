## Business Objective

There is an upstart in the ride-sharing market: The new start-up firm **rYdZ** (pronounced rides) is driver-owned and operated. In addition to providing safe rides at competitive prices, the?mission of **rYdZ** is to provide a working wage to **rYdZ** drivers. But the leadership team at **rYdZ** believes there is a problem: the two giants in the industry, **Lyft** and **Uber**, are coordinating to set prices for rides that are artificially low? The team at **rYdZ** has produced a data set of more than 100 fares offered to drivers from **Lyft** and **Uber**. The objective is  to analyze this data set and infer whether there is some sort of coordination between **Lyft** and **Uber** to set prices, as well as understand if either is pricing based on miles driven, or perhaps, based on geography.

## Data Set Description: 
The data set contains variables for the **ride number**, the **fare** (in dollars and cents) of a ride offered to a driver from Lyft, and the **distance** of that ride (in miles). There are at least 100 observations (rows) in the dataset, and possibly more. Each observation was done at roughly the same time for Uber and Lyft (the data for the ride in a row was collected at roughly the same time).

## Data Analysis


```{r}
testDF[5,2] #Using subsetting to return the output of the 5th row and 2nd column which contains 5th Lift fare.Here 5 is the row number and 2 is the column number
```

### Describing the fares provided by Lyft and Uber (separately) using descriptive statistics :
```{r}
summary(testDF) #Using summary function to get the descriptive statistics of the entire dataframe.

We can see that the mean fare charged by Lyft is 24 dollars and 54 cents .The maximum fare charged by Lyft is 33.45 and minimum fare charged is 13.39

#We can see that the mean fare charged by Uber  is 24 dollars and 85 cents .The maximum fare charged by Uber  is 46.59 and minimum fare charged is 18.39

#Overall Uber fares are higher than Lyft fares
```

### Describing the shape of the distribution for Lyft fares and  Uber fares
```{r}
hist(testDF$Lyft_13) #Using hist function to get histogram of Lyft fares and we can see that the distribution is close to normal distribution
hist(testDF$Uber_13)#Using hist function to get histogram of Uber fares and we can see that distribution is close to normal distribution
```

### Based on the fares offered by both companies, on average checking which  company is more expensive, Lyft or Uber? By how much? 
```{r}
#Mean of Lyft  fares is 24.54  while  Mean of Uber fares is 24.85 so on an average Uber is more expensive by 31 cents
```

### Creating a new attribute, called 'diff' in testDF, that represents the difference in fares between Uber and Lyft for each observation - in other words, the difference for each row
```{r}
testDF$diff <- testDF$Uber_13-testDF$Lyft_13 #Creating a new attribute diff in testDF which is the difference in fares between Uber and Lyft for each observation

head(testDF) #We can see that new variable diff gets added to our dataset DF

```

### Describing the shape of the distribution for this new variable
```{r}
hist(testDF$diff) #We can see that diff variable follows the shape close to normal distribution
```

### Sorting testDF, based on the new attribute (*diff*), and storing the sorted dataframe in 'sortedDF'
```{r}
sortedDF <- testDF[order(testDF$diff, decreasing = FALSE), ]  #Using subsetting and order function to sort the diff variable in the dataframe testDF in ascending order and storing the output in the variable sortedDF

head(sortedDF,1) #Using head function to display first  row of the sorted dataframe
tail(sortedDF,1) #Using tail function to display last row of the sorted dataframe
```

### Creating an X-Y scatterplot of the Lyft and Uber fares using the sortedDF dataframe 
```{r}
x <- sortedDF$Lyft_13 #Storing Lyft fares in a variable x
y <- sortedDF$Uber_13 #Storing Uber fares in the variable y
# Plot with main and axis titles

plot(x, y, main = "Scatterplot of Uber and Lyft Fares",
     xlab = "Lyft Fares", ylab = "Uber Fares") #Creating a scatterplot of Lyft and Uber Fares with Lyft fares on the X-axis ,Uber Fares on the Y axis and giving titles and subtitles to the scatterplot

#The scatterplot does not show an obvious pattern or relationship
```

### Generating a linear model trying to predict Lyft fares based on the distance of the trip. Interpreting the coefficients of the statistically significant predictors in the model
```{r}
#install.packages("MASS") #Installing Mass package
library(MASS) #Loading MASS library

lmOut <- lm(Lyft_13 ~ Lyft_13_distance, data= sortedDF) #Using lm function to create a linear model where Lyft fares is the dependent variable and distance of the trip is the independent variable.These variables are coming from the sortedDF dataframe
summary(lmOut) #Generates summary of the linear model

#We can see that that the p value of both the intercept and distance is less than 0.05 so both are statistically significant.

#We can also say that with increase in distance by 1 mile , the fare goes up by 0.03350 dollars

```

### Generating a similar model for the Uber trips to Interpret the coefficients of the statistically significant predictors in the model
```{r}
lmOut1 <- lm(Uber_13 ~ Uber_13_distance, data= sortedDF) #Using lm function to create a linear model where Uber fare is the dependent variable and distance of the trip is the independent variable.These variables are coming from the sortedDF dataframe
summary(lmOut1) #Generates summary of the linear model

#We can see that that the p value of both the intercept and distance is less than 0.05 so both are statistically significant.

#We can also say that with increase in distance by 1 mile , the fare goes up by 0.96827 dollars
```

### Which model is better?
```{r}
#Multiple R-squared of Lyft model is   0.03393 while that of Uber model is 0.9446 .Also p value of the overall equation of lmout is higher than p value of the F-statistic of lmout1 so lmout1 is a better model. We can also say that 94.46% of the change in Uber Fare is explained by change in distance , while only 3% of the change in Lyft fares is explained by the change in distance so lmout1 is a far better model
```

### Model's prediction of the Lyft fare for a 2.39 mile trip? 
```{r}
predDF <- data.frame(Lyft_13_distance = 2.39) #Creating prediction for distance =2.39 miles and storing it in predDF
predict(lmOut, predDF) #Using predict command on our model lmout , we can see that the fare prediction for distance of 2.39 miles is 23.00 Dollars
```

### Generating a map where each state is shaded according to the average fare for Uber
```{r}
#install.packages("ggmap") #Installs ggmap package
library(ggmap) #Loads ggmap library

#install.packages("maps") #Installs maps package
#install.packages("mapproj") #Installs mapproj package

library(maps) #Loads maps library
library(mapproj) #Loads mapproj library

df <- sortedDF%>% group_by(Uber_13_state)
df <- df %>% summarise(
  avgfare = mean(Uber_13) 
) # Calculating average Uber fare in each state using group_by and summarise function

head(df)

df$Uber_13_state <- tolower(df$Uber_13_state) #Converting state names to lower case using tolower function and overwriting Uber_13_state column
head(df)

us <- map_data("state") #Loading US state map and storing it in us
head(us)

map2 <- ggplot(df, aes(map_id=Uber_13_state))
map2 #Using ggplot to get states in df on the map


map2 <- map2 + geom_map(map=us, aes(fill=avgfare))
map2 #Using fill in the map as per the average fare

map2 <- map2 + expand_limits(x=us$long, y=us$lat)
map2

map2 <- map2 + coord_map() + ggtitle("US State Wise Average Uber Fare")
map2

#We can see from the map that average Uber fare is highest in New York,then Texas and then Florida

```

# CONCLUSION
```{r}
#Based on the scatterplot we saw ,it can be deduced that there was no relationship or significant pattern that was seen between Uber and Lyft Fares and hence they are not corrdinating prices.Also the distribution of the difference in Uber and Lyft fares follows close to a normal distribution 
