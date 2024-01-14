---
title:                "Ruby recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

As a Ruby programmer, you may come across situations where you need to compare two different dates. This could be to check if one date is before or after another, or to calculate the duration between two dates. In order to effectively work with dates in your code, it's important to understand how to compare them.

## How To

To compare two dates in Ruby, we can use the `Date` class and its built-in methods. Let's take a look at some coding examples to see how this works:

```Ruby
# Define two dates
date_1 = Date.new(2020, 10, 1)
date_2 = Date.new(2020, 10, 15)

# Check if date_1 is before date_2
puts date_1 < date_2 # Output: true

# Calculate the duration between date_1 and date_2 in days
puts (date_2 - date_1).to_i # Output: 14
```

In the above code, we first define two dates using the `Date.new` method. Then, we use the `<` operator to compare if `date_1` is before `date_2`. Next, we use the `-` method to calculate the duration between the two dates and then use the `to_i` method to convert it to an integer value, which gives us the result in days.

We can also use other methods such as `>` (greater than), `==` (equal to), and `!=` (not equal to) to compare two dates. It's important to note that when using the `<` or `>` operators, the dates must be in the same format (year, month, day) in order for the comparison to be accurate.

## Deep Dive

Behind the scenes, the `Date` class uses the Julian Day System to compare dates. This system assigns a unique number to each day starting from January 1, 4713 BC. When we subtract one date from another, we are essentially calculating the difference between their Julian Day numbers.

Additionally, we can also use the `DateTime` class to compare dates and times in Ruby. This class is similar to `Date` but includes a time component. We can also use the `Time` class for more accurate comparisons, as it includes fractions of a second.

## See Also

For more information on working with dates in Ruby, check out these helpful resources:

- [Ruby's Date class documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Comparing Dates in Ruby](https://www.webascender.com/blog/comparing-dates-in-ruby/)
- [Understanding Dates and Time in Ruby](https://blog.appsignal.com/2020/10/07/understanding-dates-and-time-in-ruby.html)