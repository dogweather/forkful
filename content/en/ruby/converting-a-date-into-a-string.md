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

## What & Why?

Converting a date into a string simply means turning a date object into a string of characters. Programmers often do this in order to display the date in a desired format or to manipulate it in some way.

## How to:

```Ruby
# Converting a current date into a string using the default format
date = Date.today
puts date.to_s

# Output: 2021-05-04

# Converting a specific date into a string with a specific format
date = Date.new(2021, 10, 31)
puts date.strftime("%B %d, %Y")

# Output: October 31, 2021
```

## Deep Dive

Converting dates into strings has been a common practice in programming since the early days of computing. In traditional programming languages, dates were often stored as numbers, making it necessary to convert them into a string in order to display them to users or perform calculations using date math.

In contrast, newer programming languages, such as Ruby, have built-in date classes that can handle date objects more easily, allowing for simpler conversion to strings. However, some older legacy systems still rely on the conversion of dates into strings.

There are also alternative methods for converting dates into strings, such as using regular expressions or third-party libraries. But for most basic cases, the built-in methods in Ruby are sufficient.

Implementation wise, converting a date into a string involves using the `to_s` method, which is available for both the `Date` and `DateTime` classes. Custom formatting can also be achieved using the `strftime` method, which takes in a string representing the desired format and returns the date as a string accordingly.

## See Also

To learn more about converting dates into strings, check out the official Ruby documentation on [Date](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html) and [DateTime](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html).

For alternative methods and libraries, refer to the [Ruby Toolbox](https://ruby-toolbox.com/categories/date_and_time.html).