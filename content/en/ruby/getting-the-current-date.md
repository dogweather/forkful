---
title:                "Ruby recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know what day it is in your Ruby program? Whether it's for scheduling tasks, displaying the current date to users, or just for personal curiosity, there are plenty of reasons why you may want to get the current date in your Ruby code.

## How To

Getting the current date in Ruby is a simple task that can be accomplished using the `Date` and `Time` classes. Let's take a look at some code examples to see how it works.

```Ruby
# First, require the Date and Time classes
require 'date'
require 'time'

# To get the current date, create a new Date object and call the today method
current_date = Date.today
# => #<Date: 2020-09-02 ((2459084j,0s,0n),+0s,2299161j)>
```

As you can see, the `today` method returns a `Date` object containing the current date. But what if we also want to know the current time? That's where the `Time` class comes in.

```Ruby
# To get the current time, create a new Time object and call the now method
current_time = Time.now
# => 2020-09-02 09:30:00 +0000
```

If you want to format the current date and time in a specific way, you can use the `strftime` method. This method allows you to format the date and time using a specific string of characters. For example, if you want to display the date as "September 2, 2020", you can do so by using the following code:

```Ruby
# Use the strftime method with desired format
current_date.strftime("%B %e, %Y")
# => "September 2, 2020"
```

You can find a full list of available formatting options for `strftime` in the Ruby documentation. Experiment with different formats to find the one that fits your needs.

## Deep Dive

Under the hood, both the `Date` and `Time` classes use the system's current date and time. However, you can also create `Date` and `Time` objects with different dates and times.

```Ruby
# Create a Date object with a specific date
date = Date.new(2019, 12, 25)
# => #<Date: 2019-12-25 ((2458849j,0s,0n),+0s,2299161j)>

# Create a Time object with a specific time
time = Time.new(2020, 9, 2, 12, 30, 0)
# => 2020-09-02 12:30:00 +0000
```

You can also perform various operations on `Date` and `Time` objects, such as adding or subtracting days, hours, or minutes. This can be useful in situations where you need to work with specific dates and times in your program.

## See Also

- [Ruby Date Class Documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Ruby Time Class Documentation](https://ruby-doc.org/core-2.7.1/Time.html)
- [Ruby strftime Method Documentation](https://ruby-doc.org/core-2.7.1/Time.html#method-i-strftime)