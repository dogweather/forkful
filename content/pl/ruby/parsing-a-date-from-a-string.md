---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:38:27.410677-07:00
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing a date from a string is turning text into a date/time object. Programmers do it to make sense of dates in logs, user input, and saved data.

## How to: (Jak to zrobić?)
```Ruby
require 'date'

# Parse a date from a string
date_string = "2023-04-10"
parsed_date = Date.parse(date_string)

puts parsed_date           # Outputs: 2023-04-10
puts parsed_date.class     # Outputs: Date

# Parse a date and time
datetime_string = "2023-04-10 14:30"
parsed_datetime = DateTime.parse(datetime_string)

puts parsed_datetime       # Outputs: 2023-04-10T14:30:00+00:00
puts parsed_datetime.class # Outputs: DateTime
```
Remember, Date and DateTime are part of Ruby's standard library. No gem installation needed.

## Deep Dive (Głębsze Zagłębienie)
Rubies didn't always come with great date parsing. It used to be a pain. Now, the built-in `Date` and `DateTime` classes make it easy. They've got `parse` methods that read various date formats automatically. You try '2023-01-01', '01/01/2023', or 'January 1, 2023' – it just works.

But there's more. If you need to parse more obscure formats, or need stricter parsing, there's `strptime` – it lets you specify the exact format. There's also Chronic, a gem that's really forgiving about input. Great for user-provided data.

Speaking of gem alternatives, Rails has ActiveSupport's `TimeWithZone` for time zone support. It's more robust if you need to handle different time zones.

Finally, time zones: `DateTime.parse` assumes UTC if no zone is provided. If you need specific time zone handling, you must take extra steps.

## See Also (Zobacz Również)
- [Ruby DateTime documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)
- [Chronic gem](https://github.com/mojombo/chronic)
- [Rails ActiveSupport TimeWithZone](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
