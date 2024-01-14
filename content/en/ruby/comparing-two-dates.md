---
title:                "Ruby recipe: Comparing two dates"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing two dates may seem like a simple task, but it can actually be quite useful in many programming scenarios. Whether you're working with date calculations, event scheduling, or data analysis, being able to accurately compare two dates is a valuable skill to have in your coding arsenal.

## How To

To begin, let's create two date objects using the Ruby Date class:

```Ruby
date_1 = Date.new(2021, 1, 1)
date_2 = Date.new(2020, 12, 31)
```

Next, we can compare these two dates using the comparison operators (>, <, ==, etc.):

```Ruby
date_1 > date_2  # false
date_1 < date_2  # true
date_1 == date_2 # false
```

We can also use the built-in methods provided by the Date class, such as `Date#before?` and `Date#after?`:

```Ruby
date_1.before?(date_2)  # false
date_1.after?(date_2)   # true
```

To get a more specific comparison, we can use the `Date#strftime` method to format the dates in a specific way. For example, we can compare the years of the two dates:

```Ruby
date_1.strftime("%Y") > date_2.strftime("%Y") # true
```

## Deep Dive

When comparing two dates, it's important to understand that the comparison is not just based on the date itself, but also takes into account the time zone in which the dates are being compared. This can be especially important when dealing with events that cross over different time zones.

Another important factor to consider is the accuracy of the date comparison. Depending on the precision of the date objects, the comparison may not be as accurate as desired. For example, if the dates have a precision of only days, any time differences within the same day will not be taken into account.

Furthermore, comparing dates that fall on a leap day or during a leap year can also lead to unexpected results. It's important to have a thorough understanding of how the Ruby Date class handles these scenarios.

## See Also

- [Ruby Date Class Documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Date Calculation in Ruby](https://www.rubyguides.com/2019/05/ruby-date/)
- [Comparing Dates in Different Time Zones](https://stackoverflow.com/questions/1457711/ruby-how-to-compare-two-dates-regardless-of-timezones)