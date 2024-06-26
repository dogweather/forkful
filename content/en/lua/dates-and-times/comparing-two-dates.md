---
date: 2024-01-20 17:33:14.787238-07:00
description: "How to: Lua doesn't have built-in date comparison functions, but we\
  \ can use the `os.time()` function to convert dates to a numerical format (epoch\
  \ time)\u2026"
lastmod: '2024-03-13T22:45:00.216750-06:00'
model: gpt-4-1106-preview
summary: Lua doesn't have built-in date comparison functions, but we can use the `os.time()`
  function to convert dates to a numerical format (epoch time) and then compare them.
title: Comparing two dates
weight: 27
---

## How to:
Lua doesn't have built-in date comparison functions, but we can use the `os.time()` function to convert dates to a numerical format (epoch time) and then compare them. Easy peasy.

```Lua
-- Convert dates to epoch time
local date1 = os.time({year=2023, month=4, day=1})
local date2 = os.time({year=2023, month=4, day=15})

-- Compare the dates
if date1 > date2 then
  print("Date1 is later than Date2.")
elseif date1 < date2 then
  print("Date1 is earlier than Date2.")
else
  print("Date1 is the same as Date2.")
end
```

Sample output if run with these dates:

```
Date1 is earlier than Date2.
```

## Deep Dive
Back in the day, Lua didn't come with a date type. Programmers relied on the `os.time()` function for date-time operations, which is still used today. `os.time()` returns the time in seconds since the epoch (a.k.a. Unix time, which started on January 1, 1970). This is useful because it converts dates into numbers, simplifying comparisons.

As for alternatives, you could write a custom comparator for date tables, compare each field (year, month, day) manually, or use a third-party date library like `LuaDate`.

When using `os.time()`, be mindful of time zones and daylight saving changes. The function assumes you're providing local time unless you specify otherwise.

## See Also
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/
- LuaDate, a date and time module: https://github.com/Tieske/date
- Understanding Unix timestamp: https://en.wikipedia.org/wiki/Unix_time
