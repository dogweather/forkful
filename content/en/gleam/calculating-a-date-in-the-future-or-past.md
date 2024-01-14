---
title:    "Gleam recipe: Calculating a date in the future or past"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Why

Calculating dates in the future or past can be a useful tool for planning and organizing events, meetings, or appointments. It can also be helpful for setting reminders or creating a schedule for tasks.

# How To

To calculate a date in the future or past, we can use the `Calendar` module in Gleam. First, we need to import the `Calendar` module into our project. We can do this by adding the following line to the top of our Gleam file:

```
import gleam/calendar
```

Next, we can create a function that takes in a date, a time, and a number of days as parameters and returns a new date in the future or past. Let's call this function `calculate_date` and define it as follows:

```
pub fn calculate_date(date: Date, time: Time, days: Int) -> Date {
  let new_time = Calendar.add_days(date, days)
  Time.add_seconds(time, new_time)
}
```

We can then call this function in our code and pass in the desired date, time, and number of days. For example:

```
Calendar.new(~year=2021, ~month=8, ~day=20, ~hours=10, ~minutes=30, ~seconds=0)
  |> calculate_date(~time=Calendar.midnight(), ~days=7)
```

This code would return the date 7 days in the future from August 20th, 2021 at 12:00 AM.

# Deep Dive

The `Calendar` module in Gleam provides several functions for manipulating dates and times. We can use the `add_days` function to add or subtract a specific number of days from a given date. The `Time` module also has a `add_seconds` function which can be used to add a time interval to a given time.

Additionally, the `Date` and `Time` modules have many other functions for handling dates and times, such as converting between time zones, formatting dates and times, and comparing dates and times.

It is important to note that all dates and times in Gleam are represented as immutable types, meaning they cannot be modified directly. Instead, functions such as `add_days` and `add_seconds` return a new date or time object with the desired changes.

# See Also

- [Gleam Documentation on the Calendar Module](https://gleam.run/modules/calendar/)
- [Learn Gleam by Examples](https://github.com/gleam-lang/learn-gleam-by-example)