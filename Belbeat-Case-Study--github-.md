Bellbeat Case Study
================
Brittany I
2024-04-02

## Case Study Scenario

Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that
manufactures health-focused smart products. Sršen used her background as
an artist to develop beautifully designed technology that informs and
inspires women around the world. Collecting data on activity, sleep,
stress, and reproductive health has allowed Bellabeat to empower women
with knowledge about their own health and habits. Since it was founded
in 2013, Bellabeat has grown rapidly and quickly positioned itself as a
tech-driven wellness company for women. By 2016, Bellabeat had opened
offices around the world and launched multiple products. Bellabeat
products became available through a growing number of online retailers
in addition to their own e-commerce channel on their website. The
company has invested in traditional advertising media, such as radio,
out-of-home billboards, print, and television, but focuses on digital
marketing extensively.

Bellabeat invests year-round in Google Search, maintaining active
Facebook and Instagram pages, and consistently engages consumers on
Twitter. Additionally, Bellabeat runs video ads on Youtube and display
ads on the Google Display Network to support campaigns around key
marketing dates.

Sršen knows that an analysis of Bellabeat’s available consumer data
would reveal more opportunities for growth. She has asked the marketing
analytics team to focus on a Bellabeat product and analyze smart device
usage data in order to gain insight into how people are already using
their smart devices. Then, using this information, she would like
high-level recommendations for how these trends can inform Bellabeat
marketing strategy.

## Preparing my environment

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(skimr)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

## Importing my chosen CSVs

``` r
daily_activity <- read_csv("CSVs/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
```

    ## Rows: 940 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): ActivityDate
    ## dbl (14): Id, TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDi...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
daily_sleep <- read_csv("CSVs/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
```

    ## Rows: 413 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): SleepDay
    ## dbl (4): Id, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
hourly_steps <- read_csv("CSVs/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
```

    ## Rows: 22099 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityHour
    ## dbl (2): Id, StepTotal
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
hourly_calories <- read_csv("CSVs/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
```

    ## Rows: 22099 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityHour
    ## dbl (2): Id, Calories
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

I chose these CSVs in particular for the most efficient points of
comparison. The daily activity dataset includes most of the metrics
taken. I would like to compare the amount of activity, steps taken, and
calories burned to the amount of sleep to see if there is any
correlation. I hypothesize that there will be correlation between steps
taken and calories burned, so I will be comparing those two CSVs as
well.

## Cleaning and preparing the data

To view changes to the dataframes as I make them, I also called on the
`View()` function periodically.

``` r
View(daily_activity)
View(daily_sleep)
View(hourly_calories)
View(hourly_steps)
```

### Cleaning the data

In order to properly compare and later merge the tables, I chose to
change all of the column names to lowercase. This helps ensure that the
syntax being used across all tables matches.

``` r
daily_activity<- rename_with(daily_activity, tolower)
clean_names(daily_activity)
```

    ## # A tibble: 940 × 15
    ##            id activitydate totalsteps totaldistance trackerdistance
    ##         <dbl> <chr>             <dbl>         <dbl>           <dbl>
    ##  1 1503960366 4/12/2016         13162          8.5             8.5 
    ##  2 1503960366 4/13/2016         10735          6.97            6.97
    ##  3 1503960366 4/14/2016         10460          6.74            6.74
    ##  4 1503960366 4/15/2016          9762          6.28            6.28
    ##  5 1503960366 4/16/2016         12669          8.16            8.16
    ##  6 1503960366 4/17/2016          9705          6.48            6.48
    ##  7 1503960366 4/18/2016         13019          8.59            8.59
    ##  8 1503960366 4/19/2016         15506          9.88            9.88
    ##  9 1503960366 4/20/2016         10544          6.68            6.68
    ## 10 1503960366 4/21/2016          9819          6.34            6.34
    ## # ℹ 930 more rows
    ## # ℹ 10 more variables: loggedactivitiesdistance <dbl>,
    ## #   veryactivedistance <dbl>, moderatelyactivedistance <dbl>,
    ## #   lightactivedistance <dbl>, sedentaryactivedistance <dbl>,
    ## #   veryactiveminutes <dbl>, fairlyactiveminutes <dbl>,
    ## #   lightlyactiveminutes <dbl>, sedentaryminutes <dbl>, calories <dbl>

``` r
daily_sleep <- rename_with(daily_sleep, tolower)
clean_names(daily_sleep)
```

    ## # A tibble: 413 × 5
    ##            id sleepday       totalsleeprecords totalminutesasleep totaltimeinbed
    ##         <dbl> <chr>                      <dbl>              <dbl>          <dbl>
    ##  1 1503960366 4/12/2016 12:…                 1                327            346
    ##  2 1503960366 4/13/2016 12:…                 2                384            407
    ##  3 1503960366 4/15/2016 12:…                 1                412            442
    ##  4 1503960366 4/16/2016 12:…                 2                340            367
    ##  5 1503960366 4/17/2016 12:…                 1                700            712
    ##  6 1503960366 4/19/2016 12:…                 1                304            320
    ##  7 1503960366 4/20/2016 12:…                 1                360            377
    ##  8 1503960366 4/21/2016 12:…                 1                325            364
    ##  9 1503960366 4/23/2016 12:…                 1                361            384
    ## 10 1503960366 4/24/2016 12:…                 1                430            449
    ## # ℹ 403 more rows

``` r
hourly_calories <- rename_with(hourly_calories, tolower)
clean_names(hourly_calories)
```

    ## # A tibble: 22,099 × 3
    ##            id activityhour          calories
    ##         <dbl> <chr>                    <dbl>
    ##  1 1503960366 4/12/2016 12:00:00 AM       81
    ##  2 1503960366 4/12/2016 1:00:00 AM        61
    ##  3 1503960366 4/12/2016 2:00:00 AM        59
    ##  4 1503960366 4/12/2016 3:00:00 AM        47
    ##  5 1503960366 4/12/2016 4:00:00 AM        48
    ##  6 1503960366 4/12/2016 5:00:00 AM        48
    ##  7 1503960366 4/12/2016 6:00:00 AM        48
    ##  8 1503960366 4/12/2016 7:00:00 AM        47
    ##  9 1503960366 4/12/2016 8:00:00 AM        68
    ## 10 1503960366 4/12/2016 9:00:00 AM       141
    ## # ℹ 22,089 more rows

``` r
hourly_steps <- rename_with(hourly_steps, tolower)
clean_names(hourly_steps)
```

    ## # A tibble: 22,099 × 3
    ##            id activityhour          steptotal
    ##         <dbl> <chr>                     <dbl>
    ##  1 1503960366 4/12/2016 12:00:00 AM       373
    ##  2 1503960366 4/12/2016 1:00:00 AM        160
    ##  3 1503960366 4/12/2016 2:00:00 AM        151
    ##  4 1503960366 4/12/2016 3:00:00 AM          0
    ##  5 1503960366 4/12/2016 4:00:00 AM          0
    ##  6 1503960366 4/12/2016 5:00:00 AM          0
    ##  7 1503960366 4/12/2016 6:00:00 AM          0
    ##  8 1503960366 4/12/2016 7:00:00 AM          0
    ##  9 1503960366 4/12/2016 8:00:00 AM        250
    ## 10 1503960366 4/12/2016 9:00:00 AM       1864
    ## # ℹ 22,089 more rows

As well as cleaning our names and regulating our syntax, I will use the
`duplicated()` function to check for and remove duplicates.

``` r
sum(duplicated(daily_activity))
```

    ## [1] 0

``` r
sum(duplicated(daily_sleep))
```

    ## [1] 3

``` r
sum(duplicated(hourly_calories))
```

    ## [1] 0

``` r
sum(duplicated(hourly_steps))
```

    ## [1] 0

From this I have learned that we have duplicate entries in our
daily_sleep table. I will use the `distinct()` and `drop_na()` functions
to remove those duplicates.

``` r
daily_sleep <- daily_sleep %>% distinct() %>% drop_na()
```

The `sum()` and `duplicated()` functions will allow us to check that our
duplicates have been successfully removed.

``` r
sum(duplicated(daily_sleep))
```

    ## [1] 0

### Merging Daily tables

To compare daily activity to daily sleep, I will be merging the two
tables.

#### Preparing Daily Tables

As the variables ID and date will be the same, I need to convert the
date time in the sleep table into date instead.

Using the `class()` function, I can see that the “sleepday” column is
currently a character field rather than a date-time field. To make the
date/times into simply dates, I first need to make the column a
date-time field instead.

``` r
class(daily_sleep$sleepday)
```

    ## [1] "character"

``` r
daily_sleep$sleepday=as.POSIXct(daily_sleep$sleepday, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
```

Once the column is datetime instead of character, I can use the
`as.Date()` function to convert it to date.

``` r
daily_sleep$sleepday <- as.Date(daily_sleep$sleepday)
```

I can once again check that this worked using the `class()` function.

``` r
class(daily_sleep$sleepday)
```

    ## [1] "Date"

Using the `class()` function, we can see that the “activitydate” column
in the daily_activity table is also character instead of date. This is
easier to reformat as there are no timestamps to worry about; the
`as.Date()` function allows for easy reformatting.

``` r
class(daily_activity$activitydate)
```

    ## [1] "character"

``` r
daily_activity$activitydate <- as.Date(daily_activity$activitydate, format = "%m/%d/%Y")
```

Once this is done, I can rename the “activitydate” and “sleepdate”
columns to just “date”. This will give the date columns in both tables
the same syntax.

``` r
daily_activity <- dplyr::rename(daily_activity, c("date" = "activitydate"))
daily_sleep <- dplyr::rename(daily_sleep, c("date" = "sleepday"))
```

Before merging the two tables, I chose to add a column to the
daily_activity table to total each participant’s active minutes tracked.
This will help us in our analysis later to compare time spent active to
time asleep.

``` r
daily_activity <- daily_activity %>% mutate(activeminutes = veryactiveminutes + fairlyactiveminutes + lightlyactiveminutes)
```

Before merging the two tables, I also chose to add a column to the
daily_sleep table to convert total minutes slept to total hours slept.
This allows the computer to do the math for us if we want to see time
slept in hours instead of minutes. I did the same for converting the
active minutes in the daily_activity table.

``` r
daily_sleep <- daily_sleep %>% mutate(totalhoursasleep = totalminutesasleep/60)
daily_activity <- daily_activity %>% mutate(activehours = activeminutes/60)
```

#### Merging Daily Tables

Once this is done I can merge the two tables together.

``` r
daily_comparison <- merge(daily_activity, daily_sleep)
```

It should be noted here that there were more rows in the daily_activity
table than the daily_sleep table due to participants not recording sleep
or not recording consistently. There is still plenty of data for
comparison, but I did want to point out the discrepancy for
transparency’s sake.

``` r
n_distinct(daily_activity$id)
```

    ## [1] 33

``` r
n_distinct(daily_sleep$id)
```

    ## [1] 24

``` r
n_distinct(daily_comparison$id)
```

    ## [1] 24

This shows us that 33 individual participants logged their daily
activities, but only 24 individual participants logged sleep - so our
merged table shows only those 24.

``` r
n_distinct(daily_activity$date)
```

    ## [1] 31

``` r
n_distinct(daily_sleep$date)
```

    ## [1] 31

``` r
n_distinct(daily_comparison$date)
```

    ## [1] 31

This shows us that participants logged their daily activities and sleep
for 31 days. Our merged table also shows those days.

### Merging Hourly Tables

We just merged our tables of daily activity. Likewise, to compare hourly
steps taken to hourly calories burned, I will need to merge these two
tables as well. I haven chosen to leave the “activityhour” column in
POSIXct instead of converting this into date format like the previous
table so that we can compare the activity by each hour.

``` r
hourly_calories$activityhour=as.POSIXct(hourly_calories$activityhour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_steps$activityhour=as.POSIXct(hourly_steps$activityhour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
```

Now we can merge the hourly tables.

``` r
hourly_comparison <- merge(hourly_calories, hourly_steps)
```

We can also check to see the number of participants that logged their
hourly activities as we did for the daily activities.

``` r
n_distinct(hourly_calories$id)
```

    ## [1] 33

``` r
n_distinct(hourly_steps$id)
```

    ## [1] 33

## Exploring the Data

Now that we have tidied up and arranged our data, let’s look at
exploring it further.

### Averaging the Data

To summarize the daily data for each participant and get their average
for the participation period, and then to view these averages in
ascending order, we will use the following code.

``` r
daily_average <- daily_comparison %>% group_by(id) %>% summarise(average_daily_steps = mean(totalsteps), average_daily_calories = mean(calories), average_sleep_time_minutes = mean(totalminutesasleep), average_sleep_time_hours = mean(totalhoursasleep))

ordered_daily_average <- daily_average %>% group_by(id) %>% arrange(average_daily_steps, average_daily_calories, average_sleep_time_minutes, average_sleep_time_hours)

View(ordered_daily_average)
```

### Categorizing Participants

As our only identifier for the individual participants is an ID number,
we need another way of classifying the participants into groups. Using
[a 2011 study from the International Journal of Behavioral Nutrition and
Physical
Activity](https://ijbnpa.biomedcentral.com/articles/10.1186/1479-5868-8-79),
we can arrange the participants categorically to help Bellabeat
understand what the needs of their consumers are.

Activity level - Steps per day

- Basal - \<2,500
- Limited - 2,500–4,999
- Low - 5,000–7,499
- Somewhat active - 7,500–9,999
- Active - 10,000-12,499
- Very active - \>12,500

``` r
daily_average <- daily_average %>% mutate(activity_level = case_when(
average_daily_steps < 2500 ~ "basal", 
average_daily_steps >= 2500 & average_daily_steps < 5000 ~ "limited", 
average_daily_steps >= 5000 & average_daily_steps < 10000 ~ "somewhat active", 
average_daily_steps >= 10000 & average_daily_steps < 12500 ~ "active", 
average_daily_steps >= 12500 ~ "very active"
))
```

A user who is “limited” in their daily average steps would need
different suggestions or products than a user who is “very active.”
Having categories like these will allow Bellabeat to more easily tailor
suggestions to their userbase based on their average activity level.

### Comparing Steps Taken to Calories Burned

First, let’s compare daily calories burned to daily steps taken.

``` r
ggplot(data = daily_average) + geom_point(mapping = aes(x = average_daily_steps, y =  average_daily_calories)) + labs(title = "Average Daily Steps Taken vs Calories Burned", x = "Average Daily Steps", y = "Average Daily Calories") + theme(text = element_text(color = "#ff9582"))
```

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Calories%20Burned%20vs%20Steps%20Taken-1.png)<!-- -->

``` r
ggplot(data = daily_comparison, aes(x = totalsteps, y = calories)) + geom_jitter() + geom_smooth(color = "#ff9582") + labs(title = "Daily Steps vs Calories Burned", x = "Steps Taken", y = "Calories Burned") + theme(text = element_text(color = "#ff9582"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Calories%20Burned%20vs%20Steps%20Taken-2.png)<!-- -->

``` r
ggplot(data = hourly_comparison, aes(x = calories, y = steptotal)) + geom_jitter() + geom_smooth(color = "#ff9582") + labs(title = "Hourly Steps vs Calories Burned", x = "Steps Taken", y = "Calories Burned") + theme(text = element_text(color = "#ff9582"))
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Calories%20Burned%20vs%20Steps%20Taken-3.png)<!-- -->

These plots show us that our initial hypothesis was right - participants
burned more calories when they took more steps daily. While it is less
clear with the daily averages, likely due to the smaller amount of data,
the tables that show each day individually for the participants gives a
much clearer picture.

The suggestion here for Bellabeat would be to recommend a higher daily
step count to users who would like to burn more calories daily.

### Comparing Daily Metrics to Time Asleep

Moving on to sleep - according to [the US Department of Health and Human
services](https://newsinhealth.nih.gov/2013/04/benefits-slumber), sleep
affects everything from energy to mood to even your overall health!

#### Activity Levels vs Time Asleep

In order for Bellabeat to be able to offer guidance and suggestions to
their userbase, I would first like to see if there is any correlation
between activity levels and sleep.

``` r
ggplot(data = daily_comparison, aes(x = activeminutes, y = totalminutesasleep)) + geom_point() + geom_smooth(color = "#ff9582") + labs(title = "Minutes Active vs Time Asleep", x = "Active Minutes", y = "Minutes Asleep") + theme(text = element_text(color = "#ff9582"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Activity%20Level%20vs%20Time%20Asleep-1.png)<!-- -->

``` r
ggplot(data = daily_comparison, aes(x = sedentaryminutes, y = totalminutesasleep)) + geom_point() + geom_smooth(color = "#ff9582") + labs(title = "Sedentary Minutes vs Time Asleep", x = "Sedentary Minutes", y = "Minutes Asleep") + theme(text = element_text(color = "#ff9582"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Activity%20Level%20vs%20Time%20Asleep-2.png)<!-- -->

While there doesn’t seem to be a particularly strong correlation between
active time spent and total sleep, there is definitely a negative
correlation between sedentary time spent and total sleep.

#### Steps Taken vs Time Asleep

Next, I would like to look at steps taken versus time spent sleeping.

``` r
ggplot(data = daily_comparison, aes(x = totalsteps, y = totalminutesasleep)) + geom_point() + geom_smooth(color = "#ff9582") + labs(title = "Steps Taken vs Time Asleep", x = "Total Steps", y = "Minutes Asleep") + theme(text = element_text(color = "#ff9582"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Steps%20Taken%20vs%20Time%20Asleep-1.png)<!-- -->

The amount of steps taken daily, much like time spent active, does not
seem to have a strong correlation to time spent asleep.

#### Calories Burned vs Time Asleep

Finally, for the daily activity to sleep comparisons, I would like to
look at calories burned versus time spent asleep.

``` r
ggplot(data = daily_comparison, aes(x = calories, y = totalminutesasleep)) + geom_point() + geom_smooth(color = "#ff9582") + labs(title = "Calories Burned vs Time Asleep", x = "Calories Burned", y = "Minutes Asleep") + theme(text = element_text(color = "#ff9582"))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Belbeat-Case-Study--github-_files/figure-gfm/Plotting%20Calories%20Burned%20vs%20Time%20Asleep-1.png)<!-- -->

While there is a slightly more positive correlation between calories
burned and time spent asleep, it is much like our comparisons between
sleep to time spent active and sleep to daily steps taken in that there
is not a very strong correlation.

### Summarizing Comparisons

To summarize, there is not a strong correlation between time spent
sleeping and steps taken, time spent active, or calories burned. There
is a decent negative correlation between time spent sedentary and time
spent sleeping. So although it does not appear that more active
time/steps taken/calories burned can guarantee more sleep, there does
appear to be a trend that more time spent sedentary leads to less sleep
than if participants were more active.  
The suggestion here for Bellabeat would be to suggest more time active
to users who have a higher amount of sedentary time.

## Conclusions and Recommendations

To conclude this analysis, I would like to offer some suggestions for
Bellabeat moving forward:

1 - While this fitness tracker data was helpful for initial analysis, I
would strongly recommend gathering data from Bellabeat products and
conducting a similar analysis. The fitness tracker data used here is
limited by the fact that it only really matches up to some of Bellbeat’s
products - the Time wellness watch and the Leaf wellness tracker.
Bellabeat’s userbase may use their app and the Spring water bottle in
different ways. The data gathered from the non-Bellabeat fitness tracker
is also not very current, and therefore may not accurately represent the
needs of the current Bellabeat userbase.

2 - From this analysis, we gathered that fitness tracker users who have
a higher amount of sedentary time tend to get less sleep. We also
confirmed that users who had a higher step count tended to burn more
calories. Some suggestions for marketing would be to go for the less
sedentary time leads to more sleep angle, and to share the correlation
that more steps taken means more calories burned. Marketing should
highlight that Bellabeat products track these metrics and the app allows
users to see these metrics in real time.
