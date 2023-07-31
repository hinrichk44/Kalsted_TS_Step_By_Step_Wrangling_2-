# 1.0 Goal ----

# Gain exposure to timetk data wrangling functionality




# 2.0 Libraries ----


library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)

# 3.0 Data ----


# Here we have a 7198 x 4 tibble
# Google Analytics - Summary Hourly
# This data was extracted using the googleAnalyticsR package
# We have the date (hourly data in Y-M-D-H format), page views, organic searches, and sessions by hour
# Repeated page views are counted and there may be spikes when promoted
# Organic searches are not spiked when promoted since this traffic is directed
# Sessions have spikes when promoted
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl 


# Here we have a 23,672 x 10 tibble
# This is a database export from Mailchimp that contains raw opt-in data
# Often you get data that is not a tidy time series. This data is at user-level. Will need to be prepared before we can analyze it
# It's important to note that there are four columns with timestamp data included
# Each of those columns is formatted as YYYY-MM-DD
# We can use one of those four columns as our time variable for a time series analysis
mailchimp_users_tbl  <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl 


# Here we have a 91 x 2 tibble that is also in a simple time series format
transactions_tbl  <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl 


# 4.0 Summarize By Time  ----


# * Converting Mailchimp data to time series ----


# Here we are doing a count of opt-ins by day
# We are turning our raw data into a time series format
mailchimp_users_ts <- mailchimp_users_tbl %>%
  # summarize_by_time() comes from the timetk package
  # It's for aggregating time series using time stamps and value
  # Works like summarise() but for time series
  summarise_by_time(
    # .date_var is going to be the date column we want to summarize
    # In this case we are going to use the column "optin_time"
    .date_var = optin_time,
    # This is the metric for how we will summarize our timestamp data
    # We are picking "day" for this example, but could be month or year
    .by = "day",
    # We want to summarize the number of observations per day
    # n() performs a frequency count
    # We will create a new column called "optins" that will contain that summarization
    # So now we have a 608 x 2 tibble with the timestamp and then the number of observations per that timestamp
    # For example, on 2018-07-03 there were 10 people that opted in to receiving emails
    optins = n()
  ) %>%
  # pad_by_time comes from the timetk library
  # It performs time series padding
  # It fills in any gaps to convert the time series to a regular time series
  # So now we have the time stamps filled in, but the column for "optins" now has a bunch of NA values
  # We can fix that by adding in .pad_value and in this case we will make the value 0
  # This makes sense since there are no opt-ins for those filled in days
  # We now have a 634 x 2 tibble
  pad_by_time(.date_var = optin_time, 
              .by = "day",
              .pad_value = 0)



# Here we are doing a count of opt-ins by week
# We are turning our raw data into a time series format
mailchimp_users_ts_2 <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "1 week",
    # So now we have a 89 x 2 tibble with the timestamp and then the number of observations per that timestamp
    optins = n()
  )






# Here we are doing a count of opt-ins by day
# We are turning our raw data into a time series format
mailchimp_ratings_ts <- mailchimp_users_tbl %>%
  group_by(member_rating) %>%
  summarise_by_time(optin_time,
                    .by = "day", 
                    optins = n()) %>%
  ungroup()


# * Converting Google analytics data to time series ----


# Here we will be doing some data wrangling to get the dataset into a proper time series format
google_analytics_ts <- google_analytics_summary_tbl %>%
  # ymd_h() comes from the lubridate package
  # It parses dates formatted as text strings that are in YMDH format into a date-time format
  # Always parse character dates into date or date-time
  # This makes them easier to work with for timetk and modeltime
  mutate(dateHour = ymd_h(dateHour)) %>%
  # We now have a 300 x 4 tibble
  summarise_by_time(
    .date_var = dateHour,
    .by = "day",
    # .fns stands for "functions"
    # We are applying the sum function across the three columns we chose
    across(pageViews:sessions, .fns = sum)
  )


# * Converting Transactions data to time series ----


# Here we will be doing some data wrangling to get the dataset into a proper time series format
transactions_ts <- transactions_tbl %>%
  # We now have a 21 x 2 tibble
  summarise_by_time(purchased_at,
                    .by = "1 month",
                    revenue = sum(revenue))


# Here we will be doing some data wrangling to get the dataset into a proper time series format
transactions_ts_2 <- transactions_tbl %>%
  # We now have a 21 x 2 tibble
  summarise_by_time(purchased_at,
                    .by = "1 month",
                    revenue = sum(revenue),
                    # The default time aggregation is for "floor", which is 1st date in time period specified
                    # Here we are switching to "ceiling", which is 1st date in NEXT time period
                    # For a monthly time series, anything in August would be pushed to September
                    .type = "ceiling")


# Here we will be doing some data wrangling to get the dataset into a proper time series format
transactions_ts_3 <- transactions_tbl %>%
  # We now have a 21 x 2 tibble
  summarise_by_time(purchased_at,
                    .by = "1 month",
                    revenue = sum(revenue),
                    # round takes midpoint of month and anything below is rounded down, anything above is rounded up
                    .type = "round")


# Here we will be doing some data wrangling to get the dataset into a proper time series format
transactions_ts_4 <- transactions_tbl %>%
  # We now have a 21 x 2 tibble
  summarise_by_time(purchased_at,
                    .by = "1 month",
                    revenue = sum(revenue),
                    .type = "ceiling") %>%
  # %-time% is "minus-time" and is used to subtract a period of time from a timestamp
  # So since we are aggregating by MONTHLY ceiling, then subtracting one day we get the last day of the month
  mutate(purchased_at = purchased_at %-time% "1 day")




# 5.0 Pad By Time ----


# Here we will be doing some data wrangling to get the dataset into a proper time series format
transactions_ts_5 <- transactions_tbl %>%
  # Here we are converting the transactions data from weekly into daily
  # At first, this leads to lots of NA values for each day we added
  # We need to figure out a way to spread the data across each day per week
  pad_by_time(.date_var = purchased_at, 
              .by = "day",
              # The original start date is 2018-06-03
              # We are making a start date of 2018-06-01 to make sure we get the first of June
              .start_date = "2018-06-01") %>%
  # Here is how we will spread the revenue across each day of the week
  # mutate_by_time() performs time-based mutate that applies calculation over a time window
  # It comes from the timetk package
  # We now have a 633 x 2 tibble
  mutate_by_time(.by = "week",
                 # If seasonality is expected you would have to alter this calculation
                 revenue = sum(revenue, na.rm = TRUE) / 7)




# 6.0 Filter By Time ----


# Here we are visualizing our Mailchimp time series data
mailchimp_users_ts_plot <- mailchimp_users_ts %>%
  # filter_by_time() filters a time series to a continuous region (slice) between two boundaries (start and end date)
  # It comes from the timetk package
  filter_by_time(
    # We see a huge spike in subscriptions on 2018-11-19 and that's a big outlier
    # Lets make our start date one day AFTER that spike
    .start_date = "2018-11-20"
  ) %>%
  plot_time_series(optin_time, optins)

mailchimp_users_ts_plot


# Here we are visualizing our Mailchimp time series data
mailchimp_users_ts_plot_2 <- mailchimp_users_ts %>%
  filter_by_time(
    # Here we just want to look at data from December 2019
    .start_date = "2019-12",
    .end_date = "2019-12"
  ) %>%
  plot_time_series(optin_time, optins)

mailchimp_users_ts_plot_2


mailchimp_users_ts_plot_3 <- mailchimp_users_ts %>%
  filter_by_time(
    .start_date = "2019-12",
    # %+time% is "plus time" and offsets a date by adding a duration
    # Offsetting is when you take a date and add a duration to it
    # This generates a new date some time in the future or past
    # So we will add 6 weeks
    .end_date = "2019-12-01" %+time% "6 weeks"
  ) %>%
  plot_time_series(optin_time, optins)

mailchimp_users_ts_plot_3 




# 4.0 Mutating By Time -----


# Here we will visualize average revenue in the Transactions data
transactions_revenue_plot <- transactions_tbl %>%
  mutate_by_time(
    .by = "month",
    # Here we will find the average revenue per month
    revenue_mean = mean(revenue)) %>%
  pivot_longer(contains("revenue")) %>%
  # So now we are comparing monthly revenue to average monthly revenue
  plot_time_series(purchased_at, value, name, .smooth = FALSE)

transactions_revenue_plot


# Here we will visualize average revenue in the Transactions data
transactions_revenue_plot_2 <- transactions_tbl %>%
  mutate_by_time(
    .by = "3 months",
    # Here we will find the average and median revenue per three months
    revenue_mean = mean(revenue),
    revenue_median = median(revenue)) %>%
  pivot_longer(contains("revenue")) %>%
  plot_time_series(purchased_at, value, name, .smooth = FALSE)

transactions_revenue_plot_2




# 5.0 Joining By Time ----


# Here we are doing a count of opt-ins by day
# We are turning our raw data into a time series format
mailchimp_users_ts_2018 <- mailchimp_users_tbl %>%
  summarise_by_time(
    .date_var = optin_time,
    .by = "day",
    optins = n()
  ) %>%
  # We have a 641 x 2 tibble
  pad_by_time(.date_var = optin_time, 
              .by = "day",
              .pad_value = 0,
              .start_date = "2018-06")


# Here we are combining the Mailchimp and Google analytics data together
mailchimp_google_ts <- mailchimp_users_ts_2018 %>%
  left_join(google_analytics_ts, by = c("optin_time" = "dateHour"))


# Here we are plotting the missing values in our combined dataset
mailchimp_google_ts_missing <- mailchimp_google_ts %>%
  # plot_missing() comes from the DataExplorer package
  # It shows which columns have missing values and the percentage missing
  # Looks like lots of missing data in sessions, organicSearches, and pageViews
  plot_missing()

mailchimp_google_ts_missing


# Here we are formatting our combined time series into a better dataset
mailchimp_google_ts_2 <- mailchimp_google_ts %>%
  drop_na() %>%
  # Without feature transformation we can have outliers and scale throw off relationship
  # Here we are applying the log1p function to the columns optins through sessions
  # It performs a Log Plus 1 Transformation -- log(x + 1)
  mutate(across(optins:sessions, .fns = log1p)) %>%
  # Here we are applying the standardize_vec() function to the columns optins through sessions
  # It performs a Center & Scale transformation. Mean = 0 and Standard Deviation = 1
  mutate(across(optins:sessions, .fns = log1p))
  

# Now we are visualizing our combined time series  
mailchimp_google_ts_2_plot <- mailchimp_google_ts_2
  pivot_longer(optins:sessions) %>%
  plot_time_series(optin_time, value, .color_var = name)


mailchimp_google_ts_2_plot


# Now we are looking at cross correlation factors within our time series
# Is optins a function of page views + organic searches + sessions?
mailchimp_google_ts_2_ccf <- mailchimp_google_ts_2 %>%
  plot_acf_diagnostics(optin_time, optins, .ccf_vars = pageViews:sessions)

mailchimp_google_ts_2_ccf


mailchimp_google_ts_2_ccf_2 <- mailchimp_google_ts_2 %>%
  plot_acf_diagnostics(optin_time, 
                       optins, 
                       .ccf_vars = pageViews:sessions,
                       # High cross correlation at Lag 0 indicates page views change with day subscribers change
                       # There is a high positive correlation between those
                       # More traffic to the website leads to more subscribers
                       .show_ccf_vars_only = TRUE)

mailchimp_google_ts_2_ccf_2




# 6.0 Working With The Index ----


# * Extracting a time series index ----


# A Time Series Index is the sequence of time stamps that make up your time series
mailchimp_index <- mailchimp_users_ts %>%
  # tk_index() extracts the time series index from a dataframe
  tk_index()
  

# * Making a time series from scratch ----


# Here we are just creating a random index of values
values <- 1:100


# Here we are converting our values into an official time series
# tk_make_timeseries() makes a time series from scratch
# It is similar to seq.Date() or seq.POSIXct() but much more flexible
# We will state that the time series starts in 2011
# By default this starts in January of 2011 and goes in daily increments
values_ts <- tk_make_timeseries("2011", length_out = 100)


values_ts_2 <- tk_make_timeseries("2011", 
                                  by = "month",
                                  length_out = 100)


# Now here we are creating a nicely formatted time series
# So we have a 100 x 2 tibble with a time stamp and the 1:100 values
actual_values_ts <- tibble(
  date = tk_make_timeseries("2011", 
                            by = "month",
                            length_out = 100),
  values
)


# * Adjusting a time series for holidays ----


# Here we are simply looking at all of the holidays in the year 2011
# tk_make-holiday_sequence() generates a date sequence of holidays between a start and end date
holidays_2011 <- tk_make_holiday_sequence("2011")


# Here we are extracting all holidays based on the New York Stock Exchange between 2011-2021
holidays_2011_2021 <- tk_make_holiday_sequence(
  start_date = "2011",
  end_date = "2021",
  calendar = "NYSE")


# Here we are extracting the actual names of the holidays from 2011-2021
holiday_names_2011_2021 <- tk_make_holiday_sequence(
  start_date = "2011",
  end_date = "2021",
  calendar = "NYSE") %>%
  # tk_get_holiday_signature() converts a date sequence to a series of features indicating what holiday occurs on the date
  # All features return 0 if not identified as a holiday
  tk_get_holiday_signature()


# * Offsetting a time series ----


# Here we are adding one day
# The resulting answer is 2011-01-02
"2011-01-01" %+time% "1 day"


# Here we are subtracting one day
# The resulting answer is 2010-12-31
# You can obviously do this with months and years too, not just days
"2011-01-01" %-time% "1 day"


# * Extending a time series ----


# So here we are making a time series that deals with the month of January in 2011
# Then, we are going to extend that time series by one month
ts_2011 <- tk_make_timeseries("2011-01") %>%
  # tk_make_future_timeseries() extends a time series index into the future
  tk_make_future_timeseries(length_out = "1 month")


# So here we are making a time series for 2011 divided into quarters (3 months)
# Then, we are going to extend that time series by four more quarters
ts_2011_two <- tk_make_timeseries("2011", by = "quarter") %>%
  tk_make_future_timeseries(length_out = 4)




# 7.0 Future Frame ----


# * Creating a future time series ----


# Here we are extending our Google time series by two months
google_analytics_ts_future <- google_analytics_ts %>%
  # future_frame() is a tool for creating a new tibble with the time series index extended into the future
  future_frame(.length_out = "2 months")
  

# * Modeling future time series ----


# Here we are creating a simple linear regression model using the Google analytics data
# lm() creates a linear regression model
# "pageViews is a function of dateHour plus weekday dateHour
google_analytics_ts_model <- lm(pageViews ~ as.numeric(dateHour) + wday(dateHour, label = TRUE),
   data = google_analytics_ts)


# Now we are predicting future values using the linear regression model we created just above
# This works because each model feature is a calendar feature dependent on the dateHour column
google_analytics_ts_model_predictions <- predict(google_analytics_ts_model, newdata = google_analytics_ts_future)


# * Visualizing our predictions----


# Here we are creating a full dataset of our actual values and our predicted values
google_analytics_ts_model_predictions_plot <- google_analytics_ts %>%
  select(dateHour, pageViews) %>%
  add_column(type = "actual") %>%
  bind_rows(
    google_analytics_ts_future %>%
      mutate(
        pageViews = google_analytics_ts_model_predictions,
        type = "prediction"
      )
  ) %>%
  plot_time_series(dateHour, pageViews, type, .smooth = FALSE)


google_analytics_ts_model_predictions_plot
