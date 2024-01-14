---
title:                "Ruby recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past can come in handy for a variety of tasks such as scheduling appointments, setting reminders, or creating a countdown. With the help of Ruby programming, this task can be easily automated and customized to fit your specific needs.

## How To
To calculate a date in the future or past, there are a few different methods we can use in Ruby. Let's take a look at some examples using the built-in `DateTime` class.

### Example 1: Calculating a date in the future
We can use the `+` operator to add a certain number of days to the current date. Let's say we want to find out the date that is 7 days from now. Our code would look like this:

```Ruby
require 'date'

current_date = DateTime.now
future_date = current_date + 7 # adding 7 days to the current date
puts "The future date is: #{future_date}"
```

**Output:** The future date is: 2020-05-14T07:30:09+05:30

### Example 2: Calculating a date in the past
Similarly, we can use the `-` operator to subtract days from the current date. Let's find out the date that was 2 weeks ago:

```Ruby
require 'date'

current_date = DateTime.now
past_date = current_date - 14 # subtracting 14 days from the current date
puts "The past date was: #{past_date}"
```

**Output:** The past date was: 2020-04-21T07:30:09+05:30

### Example 3: Customizing the output
We can also customize the output of our calculated date by using the `strftime` method. This allows us to specify the format in which we want our date to be displayed. In the following example, we will calculate and format the date to display the day, month, and year only:

```Ruby
require 'date'

future_date = DateTime.now + 30 # adding 30 days to the current date
formatted_date = future_date.strftime("%d %B %Y") # formatting the date to display day, month, and year
puts "The future date is: #{formatted_date}"
```

**Output:** The future date is: 09 June 2020

## Deep Dive
Now that we have seen some basic examples of calculating dates in Ruby, let's dive deeper into the `DateTime` class. This class has many useful methods that we can use to manipulate and format dates. Some notable ones are `parse` which allows us to convert a string into a date object, `now` which gives us the current date and time, and `strptime` which can parse a string based on a provided format. To learn more about these methods and others, check out the official Ruby documentation [here](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html).

## See Also
- [How to Use Time and Date in Ruby](https://www.rubyguides.com/2015/05/working-with-time-and-dates-in-ruby/)
- [Ruby DateTime Class](https://www.rubyguides.com/2019/05/ruby-datetime/)
- [Date Formatting in Ruby](https://mixandgo.com/learn/ruby_date_formatting_strftime)