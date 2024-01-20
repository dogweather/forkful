---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in programming is simply retrieving the ongoing date as per the system's time and calendar. Programmers use this to reference or record real-time events, like timestamps for data handling or order tracking.

## How to:

Here's an easy Lua code for fetching the current date:

```Lua
os_date = os.date("*t")
print("Date: "..os_date.day.."/"..os_date.month.."/"..os_date.year)
```

When run, it may display:

```Lua
Date: 17/4/2023
```

In this case, the 'os' module's 'date' method is used to get a table containing the current date details, which we then print in a specific format.

## Deep Dive

Historically in Lua, the 'os' library's date function has been the easiest way to get the current date. No special libraries required - it's all in-built.

Alternatives include libraries like 'luadate', which provides more comprehensive date/time functions - but for basic needs, 'os.date' serves well.

The 'os.date' function internally calls the C library function 'localtime', which converts the time in seconds since the Unix epoch into a broken-down time, used here as a table of date and time values. It's fast, efficient, and accurate based on your system's clock.

## See Also

Find more Lua tricks at:

- [Programming in Lua (official manual)](http://www.lua.org/pil/index.html)
- [Lua-Users wiki (community-contributed snippets)](http://lua-users.org/wiki/)
- [LuaRocks (Lua package manager, with 'luadate' and other libraries)](https://luarocks.org/)