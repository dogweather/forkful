---
title:                "Calculating a date in the future or past"
html_title:           "Lua recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Caclulating a date in the future or past involves maneuvering dates by adding or subtracting days, months, or years. Programmers do it to process tasks such as setting up reminders, calculating due dates, or creating time-based analytics.

## How to:

Lua does not natively support date manipulation beyond its `os.date` and `os.time` functions. Fortunately, we can roll our own implementation or use libraries like Date.lua.

It's easy to get started with Date.lua. Here's how to add and subtract days from a date:

```Lua
local date = require("date")

local d = date(true) -- today's date

print(d) -- today's date in yyyy-mm-dd format

-- Adding 10 days
newDate = d:adddays(10)
print(newDate) -- New date after 10 days in yyyy-mm-dd format

-- Subtracting 7 days
pastDate = newDate:adddays(-7)
print(pastDate) -- past date 7 days before new date

```
## Deep Dive:

Historically, dealing with date & time in programming languages has always been tricky due to variances in calendars, timezones, and daylights saving time. Lua's simplistic built-in date/time handling reflects its mission to stay lightweight and lean.

As for alternatives, Lua doesn't have direct support for dates like Python’s datetime or Java’s Calendar, but there are several libraries available. Apart from Date.lua, other popular libraries include LuaDate and Chronic.

In terms of implementation, the os.date function returns a table with the current date and time, by default. With this function, you can also format the date and time in many different ways. The os.time function returns the current time since the epoch (1970-01-01 UTC) in seconds.

It should also be noted that Lua, being an extension programming language, provides these capabilities so that it can be embedded into applications which require such manipulations. 

## See Also:

- Lua’s os library: https://www.lua.org/manual/5.3/manual.html#6.9
- Date and Time in Lua: http://lua-users.org/wiki/DateAndTime
- Lua libraries for date and time handling: https://luarocks.org/modules/tieske/date, https://github.com/truemedian/lua-date, https://github.com/leafo/chronic.lua