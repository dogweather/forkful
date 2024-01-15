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

## Why

Many programs and applications require the use of dates to function properly. As programmers, it is important to know how to compare two dates in order to validate and manipulate the data accurately. This article will guide you through the process of comparing dates in Ruby.

## How To

To compare two dates in Ruby, we can use the `Date` class and its built-in methods. First, we need to create two `Date` objects representing the dates we want to compare.

```ruby
date1 = Date.new(2021, 8, 15)
date2 = Date.new(2021, 7, 10)
```

Next, we can use the `==` operator to check if the two dates are equal.

```ruby
date1 == date2
# Output: false
```

We can also use the `<` or `>` operators to compare the dates by their chronological order.

```ruby
date1 < date2
# Output: false

date1 > date2
# Output: true
```

If we want to check for equality based on a specific aspect of the date, such as the month or day, we can use the `#day`, `#month`, and `#year` methods.

```ruby
date1.day == date2.day
# Output: false

date1.month == date2.month
# Output: false

date1.year == date2.year
# Output: true
```

## Deep Dive

Under the hood, Ruby converts each `Date` object to an integer value representing the number of days since its starting point, which is January 1, 4713 BCE in the Julian calendar. This allows for easy comparison between dates.

Additionally, the `Date` class includes the `#<=>` method which returns `-1` if the first date is earlier than the second, `0` if they are equal, and `1` if the first date is later than the second.

```ruby
date1 <=> date2
# Output: 1
```

In the case of time zones, we can use the `DateTime` class to compare dates considering the time zone difference. This class also has the `#<=>` method and works similarly to the `Date` class.

## See Also

- [Ruby Date class documentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Ruby DateTime class documentation](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/DateTime.html)
- [Ruby comparison operators](https://ruby-doc.org/core-3.0.1/doc/syntax/precedence_rdoc.html#table-Comparison-Operators)