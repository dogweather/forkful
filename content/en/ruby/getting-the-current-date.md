---
title:    "Ruby recipe: Getting the current date"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

As a Ruby programmer, one common task you may encounter is getting the current date. This is useful for a variety of reasons, such as keeping track of time-sensitive actions or displaying the current date on a webpage. Luckily, Ruby has built-in methods that make getting the current date a breeze.

## How To

To get the current date in Ruby, we can use the built-in `Date` class. Here's an example of how we can get today's date and output it in a specific format using the `strftime` method:

```Ruby
today = Date.today
puts today.strftime("%B %d, %Y")
```

This would output the current date in the format "Month Day, Year", for example "January 01, 2021". We can also get the current date without specifying a format by simply calling the `to_s` method on the `Date` object:

```Ruby
today = Date.today
puts today.to_s
```

This would output the date in the format "Year-Month-Day", for example "2021-01-01".

Other useful methods for getting the current date include `year` for getting the current year, `month` for getting the current month, and `day` for getting the current day of the month.

## Deep Dive

Under the hood, the `Date` class in Ruby relies on the Unix epoch timestamp, which represents the number of seconds since January 1st, 1970. By starting from this fixed point, Ruby can accurately calculate the current date and time.

If you need to work with time zones, Ruby also has a `Time` class which can handle time zone conversions and more precise timestamps. However, for most basic use cases, the `Date` class is sufficient and easier to work with.

## See Also

For further reading on working with dates and times in Ruby, check out these resources:

- [Ruby Date and Time](https://www.rubyguides.com/2015/12/ruby-date/)
- [Ruby Time class](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby Date class](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)