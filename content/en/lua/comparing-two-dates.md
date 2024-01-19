---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

# A Primer to Comparing Dates in Lua

## What & Why?

Comparing two dates is the practice of determining the time difference between these two points. This allows programmers to make decisions based on the age, duration, or sequence of events, like sending birthday reminders or scheduling tasks.

## How to:

Lua does not include built-in support for working with dates and times; therefore, we use the os library (specifically os.time() function) to convert dates to timestamps, which are easier to compare. Here's a simple example:

```Lua
date1 = os.time({year = 2022, month = 10, day = 17})  -- First date
date2 = os.time({year = 2021, month = 10, day = 17})  -- Second date

if date1 > date2 then
    print("date1 is later than date2")
else
    print("date2 is later than date1")
end
```

The output:

```Lua
date1 is later than date2
```

## Deep Dive

Comparing dates has been a ubiquitous need for programmers since computer handling of time-related data is widespread. More complex systems require more advanced libraries, like LuaDate or the Time and Date Manipulation library in LuaRocks.

Alternatives to using `os.time()` include third-party libraries such as LuaDate or built-in features of host environments (like Lua OS Library in WoW Addon programming). The `os.time()` approach works by converting structured date-time data (year, month, day) into Unix timestamps (seconds since 1970). Two timestamps can be directly compared as they are numbers.

Remember that when using `os.time()`, it assumes local time. If you are working with dates in different timezones, conversion is necessary.

## See Also

To better understand date and time in Lua, check these resources out:

- Lua Reference for Time Functions: `www.lua.org/manual/5.4/manual.html#6.9`
- LuaRocks Time and Date Manipulation Library: `luarocks.org/modules/Tieske/date`
- LuaDate: `github.com/Tieske/date`

And keep coding!