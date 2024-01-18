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

Calculating a date in the future or past means determining a date that is a certain number of days, weeks, months, or years away from a given date. This is often used by programmers in applications that involve scheduling, time tracking, or event planning.

## How to:

To calculate a date in the future or past in Lua, we can use the built-in os.date() function along with the os.time() function to convert a specific date into a timestamp. Then, we can use the os.date() function again to convert the adjusted timestamp back into a date format.

```
Lua
-- Calculate a date 7 days from today
local timestamp = os.time()
local future_date = os.date("%m/%d/%Y", timestamp + (7 * 86400)) -- 86400 seconds in a day

print(future_date) -- Outputs the date 7 days from today in mm/dd/yyyy format
```
In this example, we first use os.time() to get the current timestamp, which represents the current date and time in seconds. We then add 7 days worth of seconds (86400 seconds * 7) to the timestamp and use os.date() to convert it back into a date format, using the "%m" for month, "%d" for day, and "%Y" for year placeholders.

We can also calculate a date in the past by subtracting a certain number of seconds from the timestamp. For example, to calculate a date 30 days in the past, we would use "timestamp - (30 * 86400)".

## Deep Dive

There are other ways to calculate dates in the future or past, such as using libraries or third-party packages, but the built-in os.date() function in Lua is a simple and efficient option. It is worth mentioning that the calculation is based on the Gregorian calendar, which is the most commonly used calendar today.

In addition to calculating dates based on seconds, we can also use the os.time() function to adjust dates based on weeks, months, or years. For example, "timestamp + (2 * 7 * 86400)" would add 2 weeks to the current date, while "timestamp + (6 * 30 * 86400)" would add 6 months.

## See Also

- [Lua Date and Time functions](https://www.lua.org/pil/22.1.html)
- [os.date() function documentation](https://www.lua.org/manual/5.4/manual.html#6.9)
- [os.time() function documentation](https://www.lua.org/manual/5.4/manual.html#6.8)