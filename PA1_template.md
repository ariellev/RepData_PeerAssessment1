# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
d <- read.csv("activity.csv", header = T, na.strings = "NA", stringsAsFactor = F)
d[,2] <- as.Date(d[,2])
str(d)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(d)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

```r
require(dplyr)
total_1 <- d %>% group_by(date) %>% summarize(sum = sum(steps, na.rm = T))
hist(x = total_1$sum, xlab = "total steps", ylab = "days", main = "total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_1 <- mean(total_1$sum)
mean_1
```

```
## [1] 9354.23
```

```r
med_1 <- median(total_1$sum)
med_1
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
require(ggplot2)
# calculating average daily
s <- d %>% group_by(interval) %>% summarize( avg = mean(steps, na.rm = T)) 

# finding the maximum
max <- subset(s, s$avg == max(s$avg))

# plotting
max_label <- sprintf("(%s,%s)", max$interval, round(max$avg,2))
qplot(data = s, x = interval, y = avg, ylab = "average", main = "cross-day interval average") + geom_line() + geom_point(data=max, aes(x=interval, y=avg), colour="red", size=3) + annotate("text", max$interval + 200, max$avg, label = max_label, color = "red", size = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

#### Maximum Average 

```r
max
```

```
## Source: local data frame [1 x 2]
## 
##   interval      avg
## 1      835 206.1698
```


## Imputing missing values
#### Total number of incomplete observations

```r
nrow(d[!complete.cases(d),])
```

```
## [1] 2304
```

#### Copying original data set and filling in missing values with steps mean

```r
# calculating daily means
s <- d %>% group_by(date) %>% summarize(mean = round(mean(steps, na.rm = T),0))
s
```

```
## Source: local data frame [61 x 2]
## 
##          date mean
## 1  2012-10-01   NA
## 2  2012-10-02    0
## 3  2012-10-03   39
## 4  2012-10-04   42
## 5  2012-10-05   46
## 6  2012-10-06   54
## 7  2012-10-07   38
## 8  2012-10-08  NaN
## 9  2012-10-09   44
## 10 2012-10-10   34
## ..        ...  ...
```

```r
# replacing NAs from previous calculation, with "mean of means" 
s_mean <- round(mean(s$mean,na.rm=T),0)
s$mean <- unlist(lapply(s$mean, function(x) {if (is.na(x)) s_mean else x}))
s
```

```
## Source: local data frame [61 x 2]
## 
##          date mean
## 1  2012-10-01   37
## 2  2012-10-02    0
## 3  2012-10-03   39
## 4  2012-10-04   42
## 5  2012-10-05   46
## 6  2012-10-06   54
## 7  2012-10-07   38
## 8  2012-10-08   37
## 9  2012-10-09   44
## 10 2012-10-10   34
## ..        ...  ...
```

```r
# flling in NAs of original data set with daily means
merged <- merge(d, s)
imp <- merged %>% mutate(steps = ifelse(is.na(steps), mean, steps)) %>% select(steps, date, interval)
head(imp)
```

```
##   steps       date interval
## 1    37 2012-10-01        0
## 2    37 2012-10-01        5
## 3    37 2012-10-01       10
## 4    37 2012-10-01       15
## 5    37 2012-10-01       20
## 6    37 2012-10-01       25
```

```r
# verifiyng completeness 
nrow(imp[!complete.cases(imp),])
```

```
## [1] 0
```

#### Histogram

```r
total_2 <- imp %>% group_by(date) %>% summarize(sum = sum(steps, na.rm = T))
hist(x = total_2$sum, xlab = "total steps", ylab = "days", main = "total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
# new mean and median
mean_2 <- mean(total_2$sum)
mean_2
```

```
## [1] 10751.74
```

```r
med_2 <- median(total_2$sum)
med_2
```

```
## [1] 10656
```
#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# well in order to see that, lets put all information into a single data frame
data.frame(mean = c(mean_1, mean_2), median = c(med_1, med_2), row.names = c("before", "after"))
```

```
##            mean median
## before  9354.23  10395
## after  10751.74  10656
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# creating a factor variable
f <- factor (c("weekday", "weekend"))

# impWD = "imputed with Week Days"
impWD <- mutate(imp, type = ifelse(as.POSIXlt(date)$wday < 6, levels(f)[1], levels(f)[2]))
xtabs(~type, impWD)
```

```
## type
## weekday weekend 
##   15264    2304
```

```r
avg_weekdays <- impWD %>% filter(type == "weekday") %>% group_by(interval) %>% summarize(avg = mean(steps))
avg_weekdays$type <- f[1]
avg_weekends <- impWD %>% filter(type == "weekend") %>% group_by(interval) %>% summarize(avg = mean(steps))
avg_weekends$type <- f[2]

# binding together into a single data set
avgs <- rbind( avg_weekdays, avg_weekends) 

# plotting
require(lattice)
xyplot( avgs$avg ~ avgs$interval | avgs$type, layout = c(1,2), xlab = "interval", ylab = "Number of steps", type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
