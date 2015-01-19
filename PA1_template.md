# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


Let's create a dataframe called activitydata. 

We'll also create a true date variable (which contains both date and time).


```r
activitydata <- read.table(file = unzip("activity.zip"),
                           header = TRUE,sep = ",")

activitydata$time <- as.character(activitydata$interval)


for(i in 1:nrow(activitydata)) {
  if (nchar(activitydata[i,"time"]) == 1) 
      {activitydata[i,"time"] = paste("000",activitydata[i,"time"],sep = "")
  } else if (nchar(activitydata[i,"time"]) == 2) 
      {activitydata[i,"time"] = paste("00",activitydata[i,"time"],sep = "")
  } else if (nchar(activitydata[i,"time"]) == 3) 
      {activitydata[i,"time"] = paste("0",activitydata[i,"time"],sep = "")
  } else 
      {activitydata[i,"time"] = activitydata[i,"time"]
  }
}

activitydata$datetime <- strptime(paste(activitydata$date,activitydata$time),
                                  "%Y-%m-%d %H%M")
```

## What is mean total number of steps taken per day?

Let's first aggregate data by date



```r
activityaggregate <- aggregate(steps ~ date, data=activitydata,FUN =sum)
```

Let's then plot a histogram.


```r
hist(activityaggregate$steps, breaks = 10,  
     main = "Total steps taken by day", 
     xlab = "Steps")
```

![](PA1_template_files/figure-html/histogram-1.png) 


And finally calculate the mean and median steps


```r
mean(activityaggregate$steps)
```

```
## [1] 10766.19
```

```r
median(activityaggregate$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Let's plot the time series. 



```r
plot(x=activitydata$datetime,y=activitydata$steps, type = "l",  xlab ="", 
     ylab = "Steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


The maximum number of steps happens in interval



```r
activitydata[activitydata$steps == max(activitydata$steps,na.rm = TRUE) & is.na(activitydata$steps) == FALSE,c("datetime","steps")]
```

```
##                  datetime steps
## 16492 2012-11-27 06:15:00   806
```


## Imputing missing values

Number of missing values

```r
NAs <- is.na(activitydata$steps) 
sum(NAs)
```

```
## [1] 2304
```


We will use average steps by interval as the substitute value



```r
avg_steps_per_interval <- mean(activitydata$steps,na.rm = TRUE)
```


Let's create new dataset and do the replacement of missing values



```r
activitymodified <- activitydata

for(i in 1:nrow(activitymodified)) {
  if (is.na(activitymodified[i,"steps"])==TRUE) 
  {activitymodified[i,"steps"] = avg_steps_per_interval
  }
}
```



Let's aggregate the dataset with missing values replaced
and do the plotting.



```r
activityaggregatemod <- aggregate(steps ~ date, data=activitymodified,FUN =sum)

hist(activityaggregatemod$steps,breaks = 10,  
     main = "Total steps taken by day (w/missing imputed)", 
     xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


Let's calculate the median and mean.  There's actually virtually
no difference to the previous values.


```r
mean(activityaggregatemod$steps)
```

```
## [1] 10766.19
```

```r
median(activityaggregatemod$steps)
```

```
## [1] 10766.19
```

But there's naturally difference between the total steps taken.


```r
sum((activityaggregatemod$steps))
```

```
## [1] 656737.5
```

```r
sum((activityaggregate$steps))
```

```
## [1] 570608
```





## Are there differences in activity patterns between weekdays and weekends?

