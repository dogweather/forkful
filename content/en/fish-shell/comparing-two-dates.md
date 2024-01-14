---
title:                "Fish Shell recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Date comparison is a common task in programming, especially when working with time-sensitive data. Being able to efficiently compare two dates can help streamline processes and ensure accurate results. In this blog post, we will explore how to compare two dates using the Fish Shell programming language.

## How To
To compare two dates in Fish Shell, we will use the `date` command, which allows us to manipulate and format date and time information. Here is an example of comparing two dates:

```
set start_date (date -u -f "%Y-%m-%d" "2020-01-01")
set end_date (date -u -f "%Y-%m-%d" "2020-01-05")

if test $start_date -gt $end_date
    echo "Start date is after end date"
end
```

In this example, we use the `date` command to format the dates to the same format, `%Y-%m-%d`. Then, we use the `test` command with the `>` operator to compare the two dates. If the start date is after the end date, we print a message.

Another useful tool when working with dates is the `diff` command, which calculates the difference between two dates. For example:

```
diff (date -u "2020-01-01") (date -u "2020-01-05")
```

This will return the difference between the two dates in seconds. To convert it to a more user-friendly format, we can use the `duration` function:

```
duration (diff (date -u "2020-01-01") (date -u "2020-01-05"))
```

This will return the difference as days, hours, minutes, and seconds.

## Deep Dive
When comparing dates, it is essential to consider time zones and daylight saving time. The `date` command allows us to specify the time zone using the `-u` flag. We can also use the `tzoffset` function to account for daylight saving time.

```
set start_date (date -u -f "%Y-%m-%d" "2020-01-01")
set end_date (date -u -f "%Y-%m-%d" "2020-01-05")
set tz (tzoffset "America/Los_Angeles")

if test ($start_date + $tz) -gt ($end_date + $tz)
    echo "Start date is after end date"
end
```

Another factor to consider is leap years. The `date` command has built-in support for leap years. We can use the `%j` format to get the day of the year and compare it to determine if it is a leap year.

```
set start_date (date -u -f "%j" "2020-01-01")
set end_date (date -u -f "%j" "2021-01-05")

if test $start_date -gt $end_date
    echo "Start date is after end date"
end
```

## See Also
- Fish Shell date command: https://fishshell.com/docs/current/cmds/date.html
- Fish Shell diff command: https://fishshell.com/docs/current/cmds/diff.html
- Fish Shell duration function: https://fishshell.com/docs/current/cmds/duration.html

By using the `date` command and other built-in functions, we can effectively compare two dates in Fish Shell and handle different factors such as time zones and leap years. Having a good understanding of date manipulation in Fish Shell can greatly improve our coding efficiency and accuracy.