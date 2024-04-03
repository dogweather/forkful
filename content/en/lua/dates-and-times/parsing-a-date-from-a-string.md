---
date: 2024-02-03 19:02:42.879082-07:00
description: "Parsing a date from a string involves converting textual representations\
  \ of dates and times into a format that can be easily manipulated, stored, or\u2026"
lastmod: '2024-03-13T22:45:00.214082-06:00'
model: gpt-4-0125-preview
summary: Parsing a date from a string involves converting textual representations
  of dates and times into a format that can be easily manipulated, stored, or compared
  within a Lua program.
title: Parsing a date from a string
weight: 30
---

## How to:
Lua does not have built-in support for date and time manipulation beyond the limited functionality provided by the `os.date` and `os.time` functions. However, these can be leveraged for basic parsing, and for more complex requirements, the `luadate` library, an external library, can be utilized.

**Using `os.date` and `os.time`:**
```lua
-- Convert a human-readable date to a timestamp and back
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- Convert timestamp back to a human-readable format
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- Output: 2023-09-21 15:00:00
```

**Using `luadate` (third-party library):**
To use `luadate`, ensure it is installed via LuaRocks or your package manager of choice. `luadate` adds extensive date and time parsing and manipulation capabilities.

```lua
local date = require('date')

-- Parse a date string directly
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- Output: 2023-09-21 15:00:00

-- Adding durations
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- Output: 2023-09-28 15:00:00
```

The `luadate` library offers a more intuitive and powerful way to work with dates, including parsing from strings, formatting, and arithmetic operations on dates, which considerably simplifies working with temporal data in Lua.
