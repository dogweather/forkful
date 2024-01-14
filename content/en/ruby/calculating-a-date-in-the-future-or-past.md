---
title:                "Ruby recipe: Calculating a date in the future or past"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may often encounter situations where you need to work with dates and time. One common task is calculating a date in the future or past. This could be useful for creating reminders, scheduling tasks, or organizing events. In this blog post, we will explore how to use Ruby to easily calculate a date in the future or past.

## How To

To calculate a date in the future or past using Ruby, we can use the `Time` class. This class allows us to work with dates and times in a simple and efficient way. Let's take a look at an example:

```Ruby
# Calculate a date 7 days from now
future_date = Time.now + (7 * 24 * 60 * 60)
puts future_date

# Calculate a date 1 year from now
future_date = Time.now + (365 * 24 * 60 * 60)
puts future_date
```

In the first example, we use `Time.now` to get the current date and time, and then add 7 days to it. We do this by multiplying the number of days (7) by the number of seconds in a day (24 * 60 * 60). In the second example, we add 1 year to the current date by multiplying the number of days in a year (365) by the number of seconds in a day.

We can also calculate a date in the past by subtracting a certain number of seconds from the current date.

```Ruby
# Calculate a date 2 months ago
past_date = Time.now - (2 * 31 * 24 * 60 * 60)
puts past_date
```

In this example, we subtract 2 months (roughly 31 days) from the current date.

## Deep Dive

The `Time` class in Ruby actually represents dates and times as the number of seconds since January 1, 1970. This is known as the Unix Epoch. When we add or subtract a certain number of seconds to a `Time` object, we are actually adding or subtracting a certain amount of time to or from the original date.

Another interesting aspect of working with dates and time in Ruby is the `Date` class. This class is specifically designed for working with dates and has many useful methods for manipulating and formatting dates. We can convert a `Time` object to a `Date` object and vice versa using the `to_date` and `to_time` methods respectively.

## See Also

To learn more about working with dates and time in Ruby, check out the official documentation and these helpful resources:

- [Official Ruby documentation on Time and Date](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby's Time and Date classes explained](https://www.rubyguides.com/2015/06/ruby-time/)
- [Working with dates and time in Ruby tutorial](https://www.rubyguides.com/2019/01/ruby-datetime-time-now-strftime/)
- [Calculating future and past dates in Ruby](https://www.rubyguides.com/2018/11/find-future-past-dates-with-ruby/)

Happy coding!