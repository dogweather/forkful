---
title:                "Converting a date into a string"
html_title:           "Ruby recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a common task in programming, especially when working with user input or data that needs to be displayed in a specific format. With Ruby, this process can be easily accomplished using built-in methods.

## How To
To convert a date into a string, you can use the `strftime` method. This method takes in a format string as an argument and returns a formatted string representing the date. Here's an example:

```Ruby
date = Time.new(2021, 4, 1) # creates a new Time object for April 1st, 2021
date.strftime("%m/%d/%Y") # outputs "04/01/2021"
date.strftime("%B %d, %Y") # outputs "April 01, 2021"
```

In the above code, the `%m`, `%d`, and `%Y` placeholders are used to specify the month, day, and year respectively. The `%B` placeholder represents the full month name.

You can also use the `strftime` method to format time and date specific information such as hour, minute, second, and timezone. Here's an example:

```Ruby
time = Time.new(2021, 4, 1, 14, 30, 0, "+02:00") # creates a new Time object for April 1st, 2021 at 2:30 PM in UTC+02:00 timezone
time.strftime("%I:%M %p %Z") # outputs "02:30 PM UTC+02:00"
```

The `%I` and `%M` placeholders are used to specify the hour and minute respectively, while `%p` represents the AM or PM designator and `%Z` represents the timezone.

## Deep Dive
In Ruby, dates are represented using the `Time` class. This class stores dates and times as the number of seconds since January 1st, 1970, known as the "epoch". However, we often prefer to work with dates in a more human-readable format, which is where the `strftime` method comes in.

Besides the placeholders mentioned above, the `strftime` method accepts many other placeholders that allow for more customized date and time formatting. These placeholders include `%a` for the abbreviated day name, `%A` for the full day name, `%y` for the last two digits of the year, and many more. You can refer to the [Ruby documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Time.html#method-i-strftime) for a complete list of all placeholders and their corresponding formats.

## See Also
- [Time class documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Time.html)
- [Date and time formatting in Ruby](https://www.rubyguides.com/2015/02/ruby-date-time/)