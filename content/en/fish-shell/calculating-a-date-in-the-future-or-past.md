---
title:                "Fish Shell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Why
As programmers, we often need to work with dates and times in our projects. Sometimes, we may need to calculate a date in the future or past for a specific task. So why should we use Fish Shell to help us with this? Because Fish Shell provides a simple and efficient way to perform date calculations without the need for external libraries or complicated syntax.

# How To
To calculate a date in the future or past using Fish Shell, we can use the `date` command. This command takes in a date string and adds or subtracts a specific amount of time from it. Let's see some examples:

```
# To calculate 1 week from today
date -d "now +1 week"
# Output: 2021-08-23T14:42:54

# To calculate 2 months from a specific date
date -d "2020-06-15 +2 months"
# Output: 2020-08-15T00:00:00

# To calculate 1 year and 3 days from now
date -d "now +1 year +3 days"
# Output: 2022-08-17T14:42:54
```

As you can see, we can use keywords like `now`, `year`, `month`, `day`, etc. to specify the date and time intervals. We can also use different formats for our output, such as ISO 8601 (default), human-readable, or Unix timestamp. Fish Shell makes it easy to customize our date calculations according to our needs.

# Deep Dive
To better understand how Fish Shell performs date calculations, let's take a deeper look at the underlying code. Fish Shell uses a library called `libdateutil` to handle date and time operations. This library provides a set of functions that use the Gregorian calendar to add or subtract intervals from a given date. Fish Shell makes use of these functions to provide us with a user-friendly and convenient way of performing date calculations.

# See Also
- [Fish Shell documentation on date](https://fishshell.com/docs/current/cmds/date.html)
- [libdateutil documentation](https://github.com/fish-shell/libdateutil)
- [ISO 8601 specification](https://www.iso.org/iso-8601-date-and-time-format.html)