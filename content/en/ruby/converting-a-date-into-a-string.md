---
title:                "Ruby recipe: Converting a date into a string"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a straightforward task, but it is an essential skill for any programmer working with dates and time in their Ruby projects. It allows for more readable and user-friendly date representations, making your code more accessible for others to understand.

## How To

To convert a date into a string in Ruby, we can use the `strftime` method. This method takes in a format string, which specifies how we want our date to be displayed.

```Ruby
# Current Date
date = Date.today

# Convert to string using format "Day, Month Date, Year"
puts date.strftime("%A, %B %d, %Y")

# Output: Wednesday, July 21, 2021
```

The `%A` represents the full day name, `%B` represents the full month name, and `%d` represents the day of the month, while `%Y` represents the full year. You can also use other format codes to display the date in a different format. You can find a full list of format codes in the Ruby [documentation](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime).

## Deep Dive

Behind the scenes, the `strftime` method uses the `strftime` format codes to format the date and time according to the current locale. The `strftime` method can also be used with the `Time` and `DateTime` objects, in addition to the `Date` object.

One crucial thing to note is that the format string must be surrounded by single quotes. Otherwise, the string may be interpreted as something else, resulting in an error or unexpected output.

Another interesting feature of the `strftime` method is that you can use it to parse a string into a date or time object.

```
# Parse string "2021-07-21" into a Date object
Date.strptime("2021-07-21", "%Y-%m-%d")

# Output: #<Date: 2021-07-21 ...>
```

This is useful when working with user input or data from an external source that may not be in the correct format. Additionally, the `strftime` method allows for formatting the date based on your timezone, which can be specified using the `in_time_zone` method.

## See Also

- [Ruby Date and Time Documentation](https://ruby-doc.org/core-3.0.1/Date.html)
- [Ruby DateTime Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/DateTime.html)
- [Ruby Time Documentation](https://ruby-doc.org/core-3.0.1/Time.html)