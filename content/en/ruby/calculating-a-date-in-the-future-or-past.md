---
title:    "Ruby recipe: Calculating a date in the future or past"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 
Calculating dates in the future or past may seem like a trivial task, but it has multiple practical applications in programming. From scheduling tasks to managing project timelines, the ability to accurately determine future or past dates can greatly enhance the functionality of a program.

## How To
To calculate a date in the future or past, we can use the `Date` class in Ruby. It provides a convenient way to manipulate and perform calculations on dates. Let's take a look at a few examples:

```
require 'date'

# Calculating 30 days from today
future_date = Date.today + 30
puts future_date #=> 2021-11-19

# Calculating 3 months from a custom date
new_year = Date.new(2022, 1, 1)
future_date = new_year >> 3
puts future_date #=> 2022-04-01

# Calculating 2 weeks before today
past_date = Date.today - 14
puts past_date #=> 2021-10-24
```

In the first example, we use the `+` operator to add 30 days to the current date. Similarly, in the second example, we use the `>>` operator to add 3 months to a specified date. Finally, in the third example, we use the `-` operator to subtract 14 days from today's date.

To make more complicated calculations, we can also use the `advance` method, which takes in a hash of units and amounts. For example:

```
# Calculating 4 years and 6 months from a given date
custom_date = Date.new(2000, 1, 1)
future_date = custom_date.advance(years: 4, months: 6)
puts future_date #=> 2004-07-01
```

## Deep Dive
Behind the scenes, the `Date` class uses the proleptic Gregorian calendar system to calculate dates. This system considers every year to have 365 days, except for leap years which have 366 days. By default, Ruby considers any year divisible by 4 a leap year, but this can be modified by setting the `italy_uses_julian_calendar` attribute to `true`.

Additionally, the `Date` class also takes into account different time zones and daylight saving time. It uses the International Atomic Time (TAI) and Coordinated Universal Time (UTC) systems to handle these variations in time.

A useful tip when working with dates is to always check for edge cases such as leap years, different time zones, and other potential inconsistencies.

## See Also
- [Ruby Documentation on the Date Class](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Ruby DateTime vs. Date Class](https://stackoverflow.com/questions/4677416/difference-between-the-date-classes-in-ruby)
- [Working with Dates in Ruby](https://www.rubyguides.com/2015/03/ruby-date-format/)