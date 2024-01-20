---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:37:15.204476-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means converting text that represents a date into a format a program can understand and work with. Programmers do it because systems often receive dates as text, and they need to compare, store, or manipulate these dates programmatically.

## How to:

Lua doesn't have a built-in date parser, but you can get the job done with `os.time` and pattern matching. Let's say you've got a date string `date_str` and you want to turn it into a table that Lua can handle:

```lua
local date_str = "2023-04-05" -- ISO 8601 format
local pattern = "(%d+)-(%d+)-(%d+)"
local year, month, day = date_str:match(pattern)
local date_table = {year = year, month = month, day = day}

print(os.time(date_table)) -- Sample output: 1679785200
```

And that's your date, parsed and ready!

## Deep Dive

Lua is quite minimalist, so for parsing dates, you often roll your own solution or use a library. Historically, handling dates in Lua was mostly manual, involving string pattern matching and the `os.date` and `os.time` functions.

If you're not into reinventing the wheel, check out libraries like `Penlight` or `date.lua`. These give you more flexibility and power when dealing with dates.

As for implementation, remember that Lua's pattern matching isn't regex; it's simpler and sometimes that means a bit more work to parse complex date formats. Always test your patterns thoroughly!

## See Also

- Lua 5.4 Reference Manual for `os.time` and pattern matching: https://www.lua.org/manual/5.4/
- Penlight library's documentation: https://stevedonovan.github.io/Penlight/api/
- date.lua library on GitHub for a dedicated date parsing solution: https://github.com/Tieske/date