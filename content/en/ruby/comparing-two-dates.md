---
title:                "Comparing two dates"
html_title:           "Ruby recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a way for programmers to determine the difference between two specific dates. This can be useful for tracking time intervals, calculating age, or sorting data chronologically. It also allows for logic-based operations such as determining which date is earlier or later.

## How to: 

To compare two dates in Ruby, we can use the `Date` class which is part of Ruby's standard library. We can create date objects by specifying the year, month, and day using the `.new` method. 

```
require 'date'

date1 = Date.new(2021, 3, 13) 
date2 = Date.new(2020, 5, 1)
```

We can then use the `.jd` (Julian day) method to return the number of days since the creation of the Julian calendar. Once we have the Julian day for both dates, we can subtract one from the other to determine the difference in days.

```
p date1.jd - date2.jd # 317 days
```

Alternatively, we can use the `.between?` method to check if a date falls between two other dates. This method takes two dates as arguments and returns a boolean value.

```
date3 = Date.new(2020, 12, 25)

p date3.between?(date2, date1) # true
```

## Deep Dive

The concept of comparing dates dates back to antiquity when different civilizations used various calendars to track time. The Julian calendar, which was introduced by Julius Caesar in 45 BC, was the first to use the concept of a leap year.

In addition to the `Date` class, there are other ways to compare dates in Ruby. The `Time` class also has methods such as `.to_i` (returns the number of seconds since the epoch) and `.strftime` (converts a time object into a specific format) that can be useful for comparison purposes.

When comparing dates, it's important to consider time zones and daylight saving time. Ruby's `DateTime` class takes these factors into account and is more appropriate for comparing dates accurately.

## See Also

- [Ruby documentation on Date class](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby documentation on Time class](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby documentation on DateTime class](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)