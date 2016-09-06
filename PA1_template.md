### <b> Loading and preprocessing the data: </b>

The data is loaded:

    setwd("C:/Users/VAL/Documents/The Johns Hopkins University/R/Assignment_7")
    Activity <- read.csv("activity.csv")

The data is summarized:

    summary(Activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

### <b> What is mean total number of steps taken per day? </b>

For this section, the missing values are ignored.

    Act_No_NA <- Activity[!is.na(Activity$steps),]

#### 1) Calculate the total number of steps taken per day.

    Total = as.numeric()
    j = 0
    for (i in unique(Act_No_NA$date))
    {
            j = j+1
            Total[j] = sum(Act_No_NA[Act_No_NA$date == i, ]$steps)
    }

The total number of steps taken per day looks like:

    head(data.frame(Days = unique(Act_No_NA$date), Total.number.of.steps = Total))

    ##         Days Total.number.of.steps
    ## 1 2012-10-02                   126
    ## 2 2012-10-03                 11352
    ## 3 2012-10-04                 12116
    ## 4 2012-10-05                 13294
    ## 5 2012-10-06                 15420
    ## 6 2012-10-07                 11015

#### 2) Make a histogram of the total number of steps taken each day.

    hist(Total,
         10,
         xlab = "Total number of steps per day", 
         main = "Histogram of the total number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

#### 3) Calculate and report the mean and median of the total number of steps taken per day.

The mean of the total number of steps taken per day:

    mean(Total)

    ## [1] 10766.19

The median of the total number of steps taken per day:

    median(Total)

    ## [1] 10765

### <b> What is the average daily activity pattern? </b>

#### 1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

    Average = as.numeric()
    Inter = as.numeric()
    for (h in 0:471)
    {
            Inter[h+1] <- 5*h
            Average[h+1] <- mean(Act_No_NA[Act_No_NA$interval == 5*h,]$steps)
    }
    Table <- data.frame(Inter, Average)
    Table_No_NA <- Table[!is.na(Table$Average),]
    plot(Table_No_NA$Inter, 
         Table_No_NA$Average, 
         type = "l",
         xlab = "5-minute interval",
         ylab = "Average number of steps taken",
         main = "Average number of steps taken across all days")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

#### 2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The maximum number of steps in a 5-minute interval averaged on all days:

    Table_No_NA[which.max(Table_No_NA$Average), ]$Inter

    ## [1] 835

### <b> Imputing missing values: </b>

#### 1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    nrow(Activity[is.na(Activity), ])

    ## [1] 2304

#### 2) Devise a strategy for filling in all of the missing values in the dataset.

Before the substitution, the data looked like:

    head(Activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

I choose the mean for a 5-minute interval to replace missing values.

    for (i in 1:nrow(Activity))
    {
            if (is.na(Activity$steps[i]))
            {
                    Int <- Activity$interval[i]
                    Av <- Table_No_NA[Table_No_NA$Inter == Int,]$Average
                    Activity$steps[i] = Av
            }
    }

#### 3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

After the substitution, the data look like:

    head(Activity)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

#### 4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    Total2 = as.numeric()
    j = 0
    for (i in unique(Activity$date))
    {
            j = j+1
            Total2[j] = sum(Activity[Activity$date == i, ]$steps)
    }

    hist(Total2,
         10,
         xlab = "Total number of steps per day", 
         main = "Histogram of the total number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)

The mean of the total number of steps taken per day:

    mean(Total2)

    ## [1] 10766.19

The median of the total number of steps taken per day:

    median(Total2)

    ## [1] 10766.19

The mean does not differ from the estimate from the first part of the
assignment. Nevertheless, the median differs from the first part of the
assignement. As a consequence, imputing missing values in the data
changes the median of the total daily number of steps. One may notice
that in such a case, both the mean and the median are the same.

### <b> Are there differences in activity patterns between weekdays and weekends? </b>

#### 1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    for (i in 1:nrow(Activity))
    {
            if (weekdays(as.Date(Activity$date[i])) == "Saturday")
            {
                    Activity[i,4] <- "Weekdend"
            }else if (weekdays(as.Date(Activity$date[i])) == "Sunday")
            {
                    Activity[i,4] <- "Weekend"
            }else{
                    Activity[i,4] <- "Weekday"
            }
    }

    names(Activity)[4] <- "Days"

THe new dataset looks like:

    head(Activity)

    ##       steps       date interval    Days
    ## 1 1.7169811 2012-10-01        0 Weekday
    ## 2 0.3396226 2012-10-01        5 Weekday
    ## 3 0.1320755 2012-10-01       10 Weekday
    ## 4 0.1509434 2012-10-01       15 Weekday
    ## 5 0.0754717 2012-10-01       20 Weekday
    ## 6 2.0943396 2012-10-01       25 Weekday

#### 2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    par(mfrow = c(1, 2))

    WE <- Activity[Activity$Days == "Weekend",]
    WD <- Activity[Activity$Days == "Weekday",]

    MWE <- as.numeric(); MWD <- as.numeric()

    INT <- unique(Activity[Activity$Days == "Weekdend",]$interval)
    j = 0
    for (i in 1:length(INT))
    {
            j = j+1
            MWE[j] <- mean(WE[WE$interval == INT[i],]$steps)
    }

    plot(INT, 
         MWE, 
         xlab = "5-minute interval", 
         ylab = "Average number of steps taken", 
         main = "Average across weekend days",
         type = "l")

    INT2 <- unique(Activity[Activity$Days == "Weekday",]$interval)
    j = 0
    for (i in 1:length(INT2))
    {
            j = j+1
            MWD[j] <- mean(WD[WD$interval == INT2[i],]$steps)
    }

    plot(INT2, 
         MWD, 
         xlab = "5-minute interval", 
         ylab = "Average number of steps taken", 
         main = "Average across weekday days",
         type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-20-1.png)

One may notice that during the weekdays, the test object is very active
earlier in the days. Nevertheless, the object is clearly more active
throughout the weekends than throughout the weekdays.
