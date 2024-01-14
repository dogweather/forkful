---
title:    "Ruby recipe: Converting a date into a string"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a date into a string in your Ruby code? Converting dates into strings is a common task in programming, as it allows for the display of dates in a specific format. In this blog post, we will cover the basics of how to convert a date into a string using Ruby.

## How To

To convert a date into a string in Ruby, we use the `strftime` method. This method takes in a string as a parameter which specifies the format we want the date to be displayed in. Here is an example of how we can use `strftime` to convert a date into a string:

```Ruby
date = Date.new(2021, 10, 31)
puts date.strftime("%B %d, %Y")
```

The above code will output `"October 31, 2021"`. Let's break down the code to understand what is happening:

* `Date.new(2021, 10, 31)` creates a new date object with the specified year, month, and day.
* `strftime` is called on the `date` object and takes in `%B %d,%Y` as a parameter.
* `%B` is a format specifier for the full month name.
* `%d` is a format specifier for the day of the month.
* `%Y` is a format specifier for the full year.

There are many other format specifiers that can be used with `strftime` to customize the output of the string. For example, `%A` is a format specifier for the full day of the week, and `%m` is a format specifier for the month as a number.

## Deep Dive

When converting a date into a string, it is important to consider the format you want the date to be displayed in. Different countries and cultures have different date formats, so it is important to choose a format that is appropriate for your audience. Additionally, keep in mind that `strftime` uses the local timezone by default, so if you want to display the date in a specific timezone, you will need to include that in your formatting string.

## See Also

* [Ruby Date and Time Formats](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
* [Ruby strftime Method](https://ruby-doc.org/core-3.0.2/Time.html#method-i-strftime)
* [Ruby on Rails Date and Time Formatting](https://guides.rubyonrails.org/i18n.html#adding-date-time-formats)