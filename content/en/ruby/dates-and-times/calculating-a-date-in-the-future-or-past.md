---
date: 2024-01-20 17:32:04.402602-07:00
description: "Calculating a future or past date means finding out what the date will\
  \ be, or was, after or before a specified time interval. Programmers do this for\u2026"
lastmod: 2024-02-19 22:05:19.024723
model: gpt-4-1106-preview
summary: "Calculating a future or past date means finding out what the date will be,\
  \ or was, after or before a specified time interval. Programmers do this for\u2026"
title: Calculating a date in the future or past
---

{{< edit_this_page >}}

## What & Why?

Calculating a future or past date means finding out what the date will be, or was, after or before a specified time interval. Programmers do this for features like reminders, subscriptions, or historical data analysis.

## How to:

Ruby makes playing with dates a breeze using its built-in `Date` class and the `active_support` gem for some extra sugar. Here's how it's done:

```Ruby
require 'date'
require 'active_support/core_ext/integer'

# Get today's date
today = Date.today
puts "Today is: #{today}"

# Calculate a date 10 days in the future
future_date = today + 10
puts "10 days from now will be: #{future_date}"

# Calculate a date 30 days in the past
past_date = today - 30
puts "30 days ago it was: #{past_date}"

# More complex calculations with active_support
puts "In 2 months, it will be: #{2.months.from_now.to_date}"
puts "100 days ago, it was: #{100.days.ago.to_date}"
```

Sample output:

```
Today is: 2023-04-07
10 days from now will be: 2023-04-17
30 days ago it was: 2023-03-08
In 2 months, it will be: 2023-06-07
100 days ago, it was: 2022-12-28
```

## Deep Dive

Before Ruby absorbed date calculation functionalities into its standard and additional libraries, developers often had to manually calculate dates, considering leap years, different month lengths, and time zones—quite the headache.

The standard `Date` class does a lot out of the box. You can add (`+`) or subtract (`-`) days easily. However, for more intuitive time period manipulations, like "2 months from now", we rely on `active_support`, extracted from Ruby on Rails. This gem uses extensions to standard Ruby classes, making such calculations human-friendly.

When calculating past or future dates, consider time zones if you're also factoring in times (`DateTime` or `Time` objects). Ruby's `Time` class and `active_support` can handle this but require a bit more setup.

Alternatives exist, like the `time-lord` and `ice_cube` gems, offering more syntactic sugar or specialized features (like recurring events), respectively.

## See Also

- Dealing with time zones in Ruby: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html)
- 'time-lord' gem for more human-like expressions: [https://github.com/krainboltgreene/time-lord](https://github.com/krainboltgreene/time-lord)
- 'ice_cube' gem for handling recurring events: [https://github.com/seejohnrun/ice_cube](https://github.com/seejohnrun/ice_cube)
