---
title:    "Ruby recipe: Comparing two dates"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Comparing dates is a common task in programming, especially in projects that involve managing and manipulating time-related data. In Ruby, there are built-in methods that make it easy to compare dates and perform operations based on the comparison. Understanding how to compare dates in Ruby is an important skill for any developer working with time-based data.

## How To
Comparing two dates in Ruby is a simple process that involves using the built-in `Date` class. Here is an example of how to compare two dates:

```ruby
date_1 = Date.new(2021, 5, 15)
date_2 = Date.new(2021, 7, 2)

puts date_1 < date_2 # Prints "true"
puts date_1 > date_2 # Prints "false"
puts date_1 == date_2 # Prints "false"
```

In the above example, we create two `Date` objects and use the comparison operators to compare them. This allows us to determine if one date comes before, after, or is the same as the other date. Ruby also has methods like `Date#<=>` and `Date#eql?` that can be used to compare dates.

Additionally, the `DateTime` and `Time` classes in Ruby also have comparison methods that work similarly to the `Date` class. So, if you are working with time data that includes both date and time, these classes can be used to make comparisons as well.

## Deep Dive
When comparing dates in Ruby, it's important to understand how the comparison is actually being performed. Under the hood, Ruby is converting the date object to an integer representation before comparing them. This representation is the number of days since "January 1, 4712 BCE" in the proleptic Julian calendar. This means that Ruby is comparing the number of days that have passed since this date for each date object and determining which one is greater.

It's also important to note that the `Date` class only compares the date portion of a DateTime object, so if you want to compare date and time, you will need to use the `DateTime` or `Time` class.

## See Also
- [Ruby Date and Time Methods](https://www.ruby-lang.org/en/documentation/stdlib/date/)
- [Comparing Dates in Ruby](https://ruby-doc.org/core-3.0.1/Date.html#method-i-3C-3D-3E)
- [Understanding Date and Time in Ruby](https://www.theodinproject.com/paths/full-stack-ruby-on-rails/courses/ruby-programming/lessons/time-and-date-ruby-programming)