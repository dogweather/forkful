---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparing Dates in Ruby: A No-Nonsense Guide

## What & Why?

Comparing two dates in programming, in our case Ruby, means discerning which date comes before or after another, or if they're the same. Often used in features such as filters, tracking changes, or organizing records chronologically.

## How to:

Ruby provides a very straightforward way to compare dates. Use the `<`, `>`, `==`, `<=`, or `>=` operators.

Check this out:

```Ruby
require 'date'

date1 = Date.new(2022, 12, 31)
date2 = Date.new(2023, 1, 1)

puts date1 > date2  # false
puts date1 < date2  # true
puts date1 == date2 # false
```

If you run the snippet, you’ll see it prints `false`, `true`, `false` – just as expected.

## Deep Dive

Ruby's `Date` class has been around since the early versions, offering simple yet powerful techniques for date manipulation and comparison. At the core, when comparing dates, Ruby converts dates to a Julian Day Number, a continuous count of days since the beginning of the Julian Period (January 1, 4713 BC), and performs the comparison operation.

Alternatives for comparison include using Time or DateTime classes, but Date is generally more efficient and sufficient for most needs. Remember, in Ruby, the DateTime class is a subclass of Date, so the comparison techniques remain the same.

For implementation, key factors to understand are that the Date class accounts for leap years, and respects time zones if provided (although Date itself is zone-unaware). Also, when comparing, Ruby implicitly handles type conversion if one operand is a `DateTime` and the other is a `Date`.

## See Also

Ruby's `Date` class - [Official Documentation](https://ruby-doc.org/standard-2.5.1/libdoc/date/rdoc/Date.html)

Detailed guide on time and date in Ruby - [Tutorial](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)

Julian Day - [Wikipedia](https://en.wikipedia.org/wiki/Julian_day)