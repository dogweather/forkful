---
title:                "Calculating a date in the future or past"
html_title:           "Ruby recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 
Calculating dates in the future or past is a common and useful task in many programming projects. It can help with tasks such as scheduling events, setting deadlines, or calculating payment due dates. With the flexibility and precision of Ruby's date and time methods, it's easy to manipulate dates to fit your specific needs.

## How To
To calculate a date in the future or past in Ruby, you can use the `Date` or `DateTime` classes and their methods. Here's an example of how to add 5 days to the current date and print the result:

```Ruby
require 'date'
today = Date.today
future_date = today + 5 # adding 5 days
puts future_date # outputs 5 days from now
```
Output:
```
2021-08-25
```
You can also subtract days using a negative value. For example, to get yesterday's date, you can use `today - 1`.

If you need more precise calculations, you can use `DateTime` instead of `Date`. Here's an example of calculating 5 hours and 30 minutes into the future:
```Ruby
require 'date'
now = DateTime.now
future_time = now + Rational(5.5, 24) # adding 5.5 hours
puts future_time # outputs 5.5 hours from now
```
Output:
```
2021-08-25T05:30:00+00:00
```

## Deep Dive
Ruby's `Date` and `DateTime` classes offer a variety of methods for calculating dates and times in the future or past. For example, you can use `prev_day` and `next_day` to get the previous or next day, `yesterday` and `tomorrow` for the previous or next day's date, or `strptime` to parse a date from a string.

Additionally, you can use `strftime` to format dates and times according to your needs. This method uses a specific formatting string to output the date and time in a specified format. Check out the [Ruby documentation](https://ruby-doc.org/stdlib/libdoc/date/rdoc/DateTime.html) for a full list of formatting options.

## See Also
- [Ruby documentation for Date and DateTime classes](https://ruby-doc.org/stdlib/libdoc/date/rdoc/DateTime.html)
- [Article on time and date calculations in Ruby](https://www.sitepoint.com/ruby-date-time-classes/)
- [Blog post on manipulating dates in Ruby](https://infinum.com/the-capsized-eight/articles/the-basics-of-time-and-date-manipulation-in-ruby)

**Happy coding!**