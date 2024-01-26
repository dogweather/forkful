---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:38:18.312510-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date means translating a string into something the computer gets—like an actual date object. Programmers do it because we often snag dates as strings from places like forms, files, or the web, and we need to work with them in a structured, reliable way.

## How to:

Ruby's got your back with its `Date` library, which makes turning strings into dates as easy as pie. Just don't forget to `require 'date'` before you dive in.

```ruby
require 'date'

# Parse a date (ISO format) from a string
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# What if our format goes rogue? Try this.
begin
  custom_date = Date.strptime('03/31/2023', '%m/%d/%Y')
  puts custom_date
rescue ArgumentError
  puts "That's not a valid date format, buddy."
end
# => 2023-03-31
```

## Deep Dive

Back in the day, Ruby was less forgiving with date formats. Coders had to manually wrestle with strings to extract dates. Now, `Date.parse` automatically sniffs out most common date formats, and if it gets confused, `Date.strptime` lets you specify the exact format to avoid misinterpretations.

Alternative-wise, if you're dealing with more complex date-time data, `DateTime` might be your huckleberry, especially for parsing times as well. Moreover, for those playing outside the Ruby standard library, there's the `Chronic` gem by the clever folks at GitHub, which understands a heap of natural language date expressions.

Underneath all this simplicity, Ruby's parsing is actually powered by date format templates that match different parts of the date string with corresponding date elements (year, month, day, etc.). So when your string doesn't match the expected pattern, you'll need to give Ruby a heads-up with `strptime` and the right format directives.

## See Also

- For timezone fans, the `ActiveSupport::TimeWithZone` documentation in Rails might be interesting: [https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
- Chronic gem for natural language date parsing: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
