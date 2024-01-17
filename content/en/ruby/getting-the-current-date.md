---
title:                "Getting the current date"
html_title:           "Ruby recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date is a common task in programming that involves retrieving the current date and time from the system. Programmers may need to do this for a variety of reasons, such as timestamping events, calculating time differences, or displaying the current date and time to the user.

## How to:
To get the current date in Ruby, we can use the built-in `Time` class. Here's an example code for getting the current date and time:

```Ruby
current_date = Time.now
puts current_date
```

This will output the current date and time in the format `YYYY-MM-DD HH:MM:SS +TZ`, where TZ is the system's timezone. For example, `2021-05-14 12:30:00 +0800`.

If you only need to get the current date without the time, you can use the `Date` class instead. Here's an example code:

```Ruby
current_date = Date.today
puts current_date
```

This will output the current date in the format `YYYY-MM-DD`, without any time or timezone information.

## Deep Dive:
Ruby has built-in libraries for working with dates and times, such as `Time`, `Date`, and `DateTime`. These classes provide various methods for manipulating and formatting date and time objects.

Before Ruby 1.9, developers had to use the `DateTime` class to work with time zones. However, the introduction of the `Time` class in Ruby 1.9 made it easier to handle time information, including time zones.

Aside from the built-in classes, there are also third-party gems available for more advanced date and time manipulations, such as `Chronic` and `ActiveSupport`.

## See Also:
- [Ruby Time Class Documentation](https://ruby-doc.org/core-3.0.1/Time.html)
- [Ruby Date Class Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Chronic Gem](https://github.com/mojombo/chronic)
- [ActiveSupport Time Extension](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)