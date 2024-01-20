---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparing Two Dates in The Fish Shell

## What & Why?

Comparing two dates means to determine whether one date falls before, after, or at the same time, as another one. This is done by programmers to perform date-time-based operations, such as scheduling tasks or calculating durations.

## How to:

In Fish Shell you can compare two dates using the `date` command and epoch time, referred to as Unix timestamps.

The following is a basic example of comparing two dates:

```Fish Shell
set date1 (date --date='2021-12-01' +%s)
set date2 (date --date='2022-12-31' +%s)

if test $date1 -gt $date2
    echo "Date1 is greater than Date2"
else if test $date1 -eq $date2
    echo "Date1 is equal to Date2"
else
    echo "Date1 is less than Date2"
end

# Output:

# Date1 is less than Date2
```

This code converts two dates into Unix timestamps using the `date` command and then compares those timestamps.

## Deep Dive

The concept of comparing dates is not a new phenomenon. It has been prevalent since the inception of programming. This is typically accomplished via Unix timestamps, a method of tracking time that measures the number of seconds that have passed since the Unix Epoch (1st January 1970).

Alternatives to using Unix timestamps for comparing dates include using date libraries in various programming languages.

In the Fish Shell, `date` command along with Unix timestamps is often favored for date comparisons due to its simplicity and efficiency. The `date` command translates human-readable dates into Unix time, which is an integer that can be compared like any other number.

## See Also

For more about date comparisons and working with Unix timestamps, check out:

3. [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)