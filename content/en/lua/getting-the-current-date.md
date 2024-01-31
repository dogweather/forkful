---
title:                "Getting the current date"
date:                  2024-01-20T15:15:33.324576-07:00
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Grabbing the current date in programming lets us track when stuff happens. We need timestamps for logs, records, or just to throw a "Happy New Year" message at the right time.

## How to:

In Lua, snagging the current date and time is a breeze with the `os.date` function. Check it out:

```lua
local current_time = os.date("*t")  -- gets table with date and time components
print("Year:", current_time.year)
print("Month:", current_time.month)
print("Day:", current_time.day)

-- Want a formatted string instead? Easy.
print(os.date("%Y-%m-%d")) -- prints in YYYY-MM-DD format
```

Sample Output:
```
Year: 2023
Month: 4
Day: 14
2023-04-14
```

## Deep Dive

Lua's `os.date` has been around since the earliest days, a staple for when you need the date/time. It's based on the C `time.h` library functions, so it's not reinventing the wheel – Lua keeps it familiar.

Alternatives? Sure, you can also use `os.time` to get the seconds since the UNIX epoch and play with it, or use external libraries for broader functionality if needed. But `os.date` and `os.time` cover most bases just fine.

Implementation wise, `os.date("*t")` gets you a table with year, month, day, and more. Format it with `os.date()` by passing a format string, like `"%Y-%m-%d"` for a standard date.

Pro tip: Working with time zones? `os.date` can handle that too – use the `!"` prefix in your format string, and Lua will use Coordinated Universal Time (UTC) instead of local time.

## See Also

- Lua's `os` library documentation: http://www.lua.org/manual/5.4/manual.html#6.9
- Online Lua demo environment to test code snippets: https://www.lua.org/cgi-bin/demo
- Format specifiers for `os.date`: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
