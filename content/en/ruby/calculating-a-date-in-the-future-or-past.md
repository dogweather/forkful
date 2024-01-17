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

## What & Why?

Calculating a date in the future or past is the process of determining what a specific date will be, either ahead of or behind the current date. Programmers often do this in order to schedule events, set reminders, or track dates for various tasks.

## How to:

To calculate a date in the future or past, we can use the ```DateTime``` class in Ruby. Here's an example of how to find the date 3 weeks from now:

```Ruby
require 'date'
today = Date.today
future_date = today + 21 # 21 days from today
puts future_date.strftime("%A, %B %e") # outputs the date in the format: Day of week, Month Day 
# Example output: Friday, October 23
```

To calculate a date in the past, we can use negative values. Here's an example:

```Ruby
past_date = today - 7 # 7 days before today
puts past_date.strftime("%A, %B %e") # outputs the date in the format: Day of week, Month Day 
# Example output: Friday, September 25
```

## Deep Dive

Calculating dates has been a common practice in programming since the early days of computer programming. Before the creation of high-level languages like Ruby, programmers had to use complex algorithms to determine dates. However, with the ```DateTime``` class in Ruby, the process has become much simpler.

An alternative to using the ```DateTime``` class in Ruby would be to use the ```Time``` class. The main difference is that the ```DateTime``` class is more accurate in terms of accounting for leap years and different time zones.

To implement the calculation of a date in the future or past, the ```DateTime``` class uses the ```+``` and ```-``` methods to add or subtract days, months, or years from a given date. It also has other methods such as ```.next_day``` and ```.prev_day``` for more precise calculations.

## See Also

To learn more about the ```DateTime``` class in Ruby, check out the official documentation: https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html

For an in-depth explanation of how the ```DateTime``` class works, you can read this article: https://open.appacademy.io/learn/full-stack-online/ruby/date-and-time---day-1

To explore other ways of calculating dates in Ruby, you can take a look at the ```Time``` class: https://ruby-doc.org/core-2.7.1/Time.html