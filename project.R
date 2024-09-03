---
title: "Assignment - The Nature of Data"
output: pdf_document
---

Name: Rizwan Karim Rafat

Student ID: 22061272

Subject Code: MATH7016

By including this statement, I, the author of this work, verify that:

* I hold a copy of this assignment that we can produce if the original is lost or damaged.
* I hereby certify that no part of this assignment/product has been copied from any other student’s work or from any other source except where due acknowledgement is made in the assignment.
* No part of this assignment/product has been written/produced for us by another person except where such collaboration has been authorised by the subject lecturer/tutor concerned.
* I am aware that this work may be reproduced and submitted to plagiarism detection software programs to detect possible plagiarism (which may retain a copy on its database for future
plagiarism checking).
* I hereby certify that we have read and understood what the School of Computing, Data and Mathematical Sciences defines as minor and substantial breaches of misconduct as outlined in the learning guide for this unit.

\newpage

# Question 1

## Test if there is a significant association between the passenger's rating and the passenger's gender for the rides taken on weekends. What does being associated mean in this context? Interpret your findings.

At first we read the CSV file and store it in a variable named "rides".

```{r}
rides <- read.csv("Rides.csv")
head(rides)
```

Then we create a new data frame named "weekend_rides" where we filter the rows where the day of the week is either Saturday or Sunday and select the passenger gender and passenger rating column only for analysis.

```{r}
library(dplyr)
weekend_rides <- rides %>%
  filter(DayofWeek == "Saturday" | DayofWeek == "Sunday") %>%
  select(PassengerGender, PassengerRating)
head(weekend_rides)
```

Now we check the size of the "weekend_rides" data set.

```{r}
nrow(weekend_rides)
```

For a better understanding we create a table where we can understand the number of ratings from each gender.

```{r}
table(weekend_rides$PassengerGender)
```

### Hypothesis:
  
H0: There is no significant association between the passenger's rating and gender for rides taken on weekends.

HA: There is a significant association between the passenger's rating and gender for rides taken on weekends.

Now we perform a chi-squared test on our observed data set.

```{r, warning=FALSE}
obs_chisq <- chisq.test(table(
  weekend_rides$PassengerGender,
  weekend_rides$PassengerRating
))
obs_chisq
```

We get a p-value of 0.2488 which is very high.

Since the data set is small and we cannot confirm if the data is normally distributed, we have to do simulation.

```{r, warning=FALSE}
perm_chisq <- replicate(1000, {
  perm_rating <- sample(weekend_rides$PassengerRating)
  chisq.test(table(weekend_rides$PassengerGender,
                   perm_rating))$statistic
})
```

```{r}
p_val <- sum(perm_chisq >= obs_chisq$statistic) / 1000
p_val
```

### Conclusion

After the simulation we get a p-value of `r p_val` which is higher than our threshold (0.05). We can conclude that we do not have enough evidence to reject the null hypothesis i.e. we do not have enough evidence which shows that there is a significant association between the passenger's rating and gender for rides taken on weekends.

In this context associated means a potential relationship between the passenger's gender and their rating for the rides.

# Question 2

## Test whether the mean of tip that were held on Thursday's for male drivers are greater than female drivers.

### Hypothesis:

H0: Mean of tip that were held on Thursday's for male drivers are equal to female drivers.

HA: Mean of tip that were held on Thursday's for male drivers are greater than female drivers.

At first we create a data frame named "thursday_rides" where we filter the rows where the day of the week is Thursday and the gender is either male or female and select the gender of the driver and tip only.

```{r}
library(dplyr)
thursday_rides <- rides %>%
  filter(DayofWeek == "Thursday") %>%
  filter(DriverGender == "Male" | DriverGender == "Female") %>%
  select(DriverGender, Tip)
head(thursday_rides)
```

Now we check the size of the "thursday_rides" data set.

```{r}
nrow(thursday_rides)
```

For a better understanding of the data frame we create a table summarizing the total number of male and female.

```{r}
table(thursday_rides$DriverGender)
```

We now demonstrate the data in a box plot to understand the mean, the spread and the outliers of male and female.

```{r}
boxplot(Tip~DriverGender, thursday_rides, horizontal = TRUE)
```

From the box plot above we can observe that the mean tip of male is lower than that of female. To further confirm our findings, we can simulate our data and find the p-value.

At first we find the difference of mean between the genders.

```{r}
m <- mean(thursday_rides$Tip[thursday_rides$DriverGender == "Male"])
f <- mean(thursday_rides$Tip[thursday_rides$DriverGender == "Female"])
dif <- m - f
dif
```

The difference of mean is `r dif`.

Now we simulate our data set 1000 times.

```{r}
thursday_rides_rep <- replicate(1000, {
  DriverGender.sim <- sample(thursday_rides$DriverGender)
  - diff(aggregate(Tip ~ DriverGender.sim, thursday_rides, mean)$Tip)
})
hist(thursday_rides_rep)
```

```{r}
pVal <- mean(thursday_rides_rep > dif)
pVal
```

### Conclusion

After the simulation we get a p-value of `r pVal` which is higher than our threshold (0.05). So we can conclude that we do not have enough evidence to reject the null hypothesis i.e. we do not have enough evidence to say that the mean of tip that were held on Thursday's for male drivers are greater than female drivers.

# Question 3

## Compute the 98% confidence interval for the difference in the mean fare charged for rides starting from Olympic Park versus those starting from Circular Quay.
* First, use bootstrapping to compute the confidence interval.
* Then approximate the confidence interval based on a t-distribution.
* How do the results compare? Justify your answer.

To compute the 98% confidence interval for the difference in the mean fare charged for rides starting from Olympic Park versus those starting from Circular Quay at first we create a data frame. In the data frame we filter the rows where the rides start from Olympic Park or Circular Quay and then we select the pickup location and fare for the rides.

```{r}
olympic_circular_rides <- rides %>%
  filter(PickupLoc == "Olympic Park" |
           PickupLoc == "Circular Quay") %>%
  select(PickupLoc, Fare)
head(olympic_circular_rides)
```

```{r}
boxplot(Fare~PickupLoc, olympic_circular_rides)
```

We can observe that the variances are not equal.

Now we check the size of the "olympic_circular_rides" data set.

```{r}
nrow(olympic_circular_rides)
```
Since the "olympic_circular_rides" data set size is small (less than 30), we calculate the confidence interval from bootstrapping.

```{r}
olympic <-
  olympic_circular_rides$Fare[olympic_circular_rides$PickupLoc == "Olympic Park"]
circular <-
  olympic_circular_rides$Fare[olympic_circular_rides$PickupLoc == "Circular Quay"]

boot <- replicate(1000, {
  sc <- sample(olympic, replace = TRUE)
  sw <- sample(circular, replace = TRUE)
  mean(sc) - mean(sw)
})

hist(boot)
```

From the histogram above we can observe that the average difference in the mean fare is close to 30 where the spread is between 10 and 45. To further investigate, we will calculate the 98% confidence interval for the difference.

```{r}
CI <- quantile(boot, c(0.01, 0.99))
CI
```
```{r}
t.test(olympic, circular,
       alternative = "two.sided",
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.98)
```

### Conclusion

We are 98% confident that the true population difference is between `r CI[1]` and `r CI[2]`.

# Question 4

## Test if the mean fare charged for rides is different for the days of the week? If so, find which day has the highest fare charged.

### Hypothesis:

H0: Mean fare charged for rides is not different for the days of the week.

HA: Mean fare charged for rides is different for the days of the week.

```{r}
boxplot(Fare ~ DayofWeek, rides)
```

From the box plot above we can observe that the variances are not equal.

```{r}
table(rides$DayofWeek)
```

From the box plot above we can observe the mean fare charged on different days of the week. The box plot suggests that the mean values are different for the days of the week. To further investigate this, we have to calculate the f-statistic and p-value.

At first we replicate the data set 1000 to find the simulated result.

```{r}
x <- replicate(1000, {
  DayofWeek.perm <- sample(rides$DayofWeek)
  oneway.test(Fare ~ DayofWeek.perm, data = rides, var.equal = FALSE)$statistic
})
hist(x)
```

From the histogram above we can suggest that the replication resulted in a right-skewed histogram. This means that the data is positively skewed, meaning that the majority of the observations are on the lower end of the distribution, while a few observations have higher values.

Now we calculate the f-statistic for the original data set.

```{r}
Fstat <- oneway.test(Fare ~ DayofWeek, data = rides, var.equal = FALSE)$statistic
Fstat
```

P-value is calculated by comparing the simulated f-statistics to the f-statistic of the original data set:

```{r}
pVal <- mean(x > Fstat)
pVal
```
### Conclusion

Since p-value is `r pVal` which is lower than our threshold (0.05), we can say that there is evidence of a difference of the mean fare charged for rides and we have enough evidence to reject the null hypothesis.

```{r}
ns <- table(rides$DayofWeek) # obtain sample size of each category
ns
```

To find the day when the highest fare was charged, first we have to do the post-hoc pairwise comparison.

```{r}
x <- replicate(1000, {
  DayofWeek.perm <- sample(rides$DayofWeek) # shuffle the categories
  fit0 <- aov(Fare ~ DayofWeek.perm, data = rides) # compute ANOVA to obtain MSE
  MSE <- summary(fit0)[[1]][2, 3] # Extract the MSE
  means <- aggregate(Fare ~ DayofWeek.perm, data = rides, mean)[, 2] # compute means of categories
  Ts <- outer(means, means, "-") / sqrt(outer(1 / ns, 1 / ns, "+")) # t-statistics
  Ts = Ts / sqrt(MSE) # Scale by pooled standard deviation
  max(abs(Ts)) # keep largest t statistic
})
hist(x) # examine distribution of maximum t statistics
```

Now we compute the t statistic for each pair of categories from the original data.

```{r}
fit = aov(Fare ~ DayofWeek, data = rides)
MSE = summary(fit)[[1]][2, 3]
means = aggregate(Fare ~ DayofWeek, data = rides, mean)[, 2]
Ts = outer(means, means, "-") / sqrt(outer(1 / ns, 1 / ns, "+"))
Ts = Ts / sqrt(MSE)
Ts
```

```{r}
pVal <- mean(x > Ts[4,2]) # Sunday vs Monday
pVal
```

The p-value is `r pVal`.

```{r}
TukeyHSD(fit)
```

The p-value for Sunday versus Monday after performing TukeyHSD is 0.0004464 which is the lowest. So we can confirm they have the highest difference of mean i.e. the highest fare is charged on Sunday.

### Conclusion

We can conclude that Sunday has the highest fare charged.

# Question 5

* Draw an appropriate plot to show the relationship between the tip provided by the passenger and the fare charged for the ride taken in the mornings. Interpret your plot.
* Test if there is a linear relationship between the tip provided by the passenger and the fare charged for the ride taken in the mornings.
* Can we predict the tip provided by the passenger based on the fare charged for the ride taken in the mornings?
* If so predict the tip provided by the passenger when fare charged for the ride is 83.4 AUD.
* How good is your estimate? Discuss the suitability and/or strength of your model.

```{r}
library(dplyr)
morning_rides <- rides %>%
  filter(PickupTime == "Morning") %>%
  select(Fare, Tip, PickupTime)
head(morning_rides)
```

```{r}
plot(Tip ~ Fare, data = morning_rides, pch = 16)
fit = lm(Tip ~ Fare, data = morning_rides)
abline(fit, lwd = 2)
```
### Hypothesis:

H0: There is no linear relationship between the tip provided by the passenger and the fare charged for the ride taken in the mornings.

HA: There is a linear relationship between the tip provided by the passenger and the fare charged for the ride taken in the mornings.

```{r}
cor0 <- cor(morning_rides$Fare, morning_rides$Tip)
cor0
```

```{r}
simCor = replicate(10000, {
  postShuffle = sample(morning_rides$Tip)
  cor(morning_rides$Fare, postShuffle)
})
hist(simCor)
```

```{r}
hist(simCor, xlim = c(-1, 1))
abline(v = c(-cor0, cor0), lwd = 2)
```

```{r}
pVal = mean(simCor > cor0) + mean(simCor < (-cor0))
pVal
```

### Conclusion

P-value is `r pVal` which is higher than our threshold (0.05), so we can say that we do not have enough evidence to reject the null hypothesis i.e. we do not have enough evidence to say that there is a linear relationship between the tip provided by the passenger and the fare charged for the ride taken in the mornings

Since the correlation value is `r cor0` which is very low, we can suggest that we cannot predict the tip provided by the passenger based on the fare charged for the ride taken in the mornings.

Although the model is very weak, we can still calculate the tip using the following method:

```{r}
print(fit)
```

Prediction for the tip provided by the passenger when fare charged for the ride is 83.4 AUD:

```{r}
y = 4.600602 + (-0.002723) * 83.4
y
```

Predicted Tip: 4.373504

```{r}
summary(fit)
```

From the Multiple R-squared: 0.0006343, which is close to 0, we can suggest that it's bad regression model which may lead to a bad estimate.