---
title:                "Getting the current date"
date:                  2024-01-20T15:16:20.667739-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Grabbing the current date in Ruby is as simple as it sounds: retrieving today's date. Programmers need it for tasks ranging from logging and timestamping to scheduling and validity checks.

## How to:
Ruby makes it easy-peasy to get the current date. Here’s how:

```ruby
require 'date'

# Get the current date
current_date = Date.today
puts current_date
```

Running this snippet will print something like this (depending on the day you run it):

```
2023-04-07
```

Want the time too? Here's the code:

```ruby
require 'time'

# Get the current date and time
current_datetime = Time.now
puts current_datetime
```

And the output will include the timestamp:

```
2023-04-07 12:34:56 +0900
```

## Deep Dive
Once upon a time, Rubyists needed external libraries to manage dates and times. Enter the standard Ruby library with `Date` and `Time` classes, and the need for extras was mostly history.

`Date` handles, well, dates - day, month, and year. For more precision, `DateTime` combines date and time, but if you need just the time or more granular details like seconds or time zones, `Time` has your back.

Alternatives to Ruby's built-in classes include gems like 'timecop' for testing time-dependent code, and 'chronic' for parsing natural language dates.

Under the hood, `Date.today` pulls your system's date. It keeps things simple but ignores time zones. `Time.now` goes farther, accounting for time zones with a default offset from Coordinated Universal Time (UTC).

## See Also
* Ruby's docs on the Time class: [https://ruby-doc.org/core-2.7.0/Time.html](https://ruby-doc.org/core-2.7.0/Time.html)
* The 'timecop' gem for mocking with time: [https://github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop)
* The 'chronic' gem for natural language date parsing: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
