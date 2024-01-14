---
title:    "Ruby recipe: Calculating a date in the future or past"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be useful for a variety of reasons, such as scheduling events, managing deadlines, or working with time-sensitive data.

## How To

To calculate a date in the future or past, we can use the `Date` class in Ruby. First, we need to require the `date` library:

```Ruby
require 'date'
```

Next, we can use the `Date.today` method to get the current date and then add or subtract a specific number of days, months, or years to get the desired date. For example, to get the date 1 week from today:

```Ruby
Date.today + 7
```

This will output the new date in the format yyyy-mm-dd. We can also specify the number of days, months, or years in the `Date.new` method:

```Ruby
Date.new(2020, 12, 25) + 7 # 2021-01-01
```

We can also use the `Date.parse` method to convert a string into a date object:

```Ruby
Date.parse("2020-12-25") + 7 # 2021-01-01
```

We can also calculate the difference between two dates using the `#difference` method:

```Ruby
(Date.today + 7).difference(Date.today) # 7
```

## Deep Dive

Ruby's `Date` class uses the proleptic Gregorian calendar, which means it can handle dates from 4712 BC to infinity. It also supports internationalization and can format dates in different languages and time zones.

Additionally, the `Date` class can handle leap years and adjust for daylight saving time. It also provides methods for working with time zones, such as converting a date to a specific time zone or getting the UTC offset for a given date.

In conclusion, the `Date` class in Ruby is a powerful tool for working with dates in the future or past.

## See Also

- [Ruby Date class documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby DateTime class documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Tutorial on working with dates in Ruby](https://www.rubyguides.com/2015/08/ruby-date/)
- [Additional date and time manipulation methods in Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-ruby)