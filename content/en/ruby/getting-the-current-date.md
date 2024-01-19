---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in a program refers to retrieving the accurate real-time date from the system. Programmers do this for time-tracking, setting deadlines, or managing events in their applications.

## How to: 

To fetch the current date in Ruby, you just need to use the `Date` class from Ruby's standard library. Here's the basic way:

```Ruby
require 'date'

puts Date.today
```
When you run this code, your output will look something like this:

```Ruby
2022-10-05
```
That's your current date in a YYYY-MM-DD format!

## Deep Dive

Initially, developers manually calculated dates and handled timezones. But with evolution of programming languages like Ruby, these tasks have become much easier.

There are a couple of alternatives for the `Date.today` method in Ruby:

1. `Time.new`: It provides both the date and time.
2. `DateTime.now`: Similar to `Time.new`, but with date/time data available down to a fraction of a second.

Here's the implementation of these alternatives:
 
```Ruby
require 'time'
require 'date'

puts Time.new
puts DateTime.now
```
The `Date` class automatically handles Leap years and varying days in different months complication, so you don't have to. It also provides a bunch of handy methods to manipulate and format date/time data.

## See Also
 
Ruby's standard library documentation has a lot of details on managing date & time:

1. [Date Class](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
2. [Time Class](https://ruby-doc.org/stdlib-3.1.1/libdoc/time/rdoc/Time.html)
3. [DateTime Class](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/DateTime.html)

They are your go-to references when dealing with date/time in Ruby. Happy coding!