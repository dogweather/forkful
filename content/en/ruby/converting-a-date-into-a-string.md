---
title:                "Ruby recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string may seem like a trivial task in programming, but it has its own significance. In simple terms, it allows us to display a date in a more readable format for users, instead of just showing it as a numerical value. Whether it's for displaying birthdays, event dates, or scheduling tasks, converting dates into strings is an essential skill for any Ruby developer.

## How To
Converting a date into a string can be done in a few simple steps using Ruby's built-in methods. First, we need to create a date object using the `Date` class, providing the specific date we want to work with. For example, `my_date = Date.new(2021, 9, 15)` would create a date object for September 15, 2021.

Next, we can use the `strftime` method to format the date into a string according to our desired format. For example, `my_date.strftime("%B %d, %Y")` would convert the date into a string in the format of Month-Day-Year, giving us "September 15, 2021".

We can also use the `to_s` method to simply convert the date object into a string without any specific formatting. For example, `my_date.to_s` would give us "2021-09-15". 

To display the date in the correct time zone, we can use the `localtime` method and specify the desired time zone like so: `my_date.localtime("America/New_York")`.

Here's a code snippet showing these methods in action:

```Ruby
require 'date'

my_date = Date.new(2021, 9, 15)

puts my_date.strftime("%B %d, %Y")
## Output: September 15, 2021

puts my_date.to_s
## Output: 2021-09-15

puts my_date.localtime("America/New_York")
## Output: 2021-09-15 00:00:00 -0400
```

## Deep Dive
Now, let's take a deeper dive into the `strftime` method. This method uses a special formatting syntax to convert the date into a string in a specific format. Some commonly used format specifiers are:

- `%a`: abbreviated weekday name (e.g. Wed)
- `%A`: full weekday name (e.g. Wednesday)
- `%b`: abbreviated month name (e.g. Sep)
- `%B`: full month name (e.g. September)
- `%d`: day of the month (01-31)
- `%Y`: full year (e.g. 2021)
- `%M`: minute (00-59)
- `%H`: hour (00-23)
- `%p`: AM/PM designation 

We can also use `%m` for the month in numeric form (01-12) or `%w` for the day of the week as a number (0-6, with 0 being Sunday).

Additionally, we can add other characters or symbols in between the format specifiers to customize the output. For example, `my_date.strftime("%B %d, %Y is a %A")` would give us "September 15, 2021 is a Wednesday". 

One thing to keep in mind is that `strftime` is a method for the `Time` class, but can also be used with the `Date` class. However, some format specifiers, like `%H` and `%M`, won't have any effect on a `Date` object since it does not have a time component.

## See Also
- [Ruby Date class documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby Time class documentation](https://ruby-doc.org/core-3.0.0/Time.html)
- [A beginner's guide to date formatting in Ruby](https://blog.appsignal.com/2018/08/21/ruby-magic-episode-8-dates-and-times-in-ruby-part-1.html)