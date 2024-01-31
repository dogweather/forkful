---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:37:41.587647-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"

category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що таке й Навіщо?)
Parsing a date from a string means converting text into a date object. Programmers do it to manipulate dates, compare them, or save them efficiently.

## How to: (Як зробити:)
```Lua
-- Lua 5.4 example

-- Load the OS library for date and time functions
local os = require("os")

-- Parse a date in the format "dd/mm/yyyy"
function parseDate(dateString)
  local pattern = "(%d+)%/(%d+)%/(%d+)"
  local day, month, year = dateString:match(pattern)
  -- Convert to numerical values
  day, month, year = tonumber(day), tonumber(month), tonumber(year)
  -- Return os.time table
  return os.time({year = year, month = month, day = day})
end

-- Usage example
local dateStr = "27/03/2023"
local parsedDate = parseDate(dateStr)
print(os.date("%A, %d %B %Y", parsedDate))  -- Output: Monday, 27 March 2023
```

## Deep Dive (Поглиблений Розділ)
Parsing dates in Lua isn't built-in like in some languages. Historically, Lua focuses on a small set of core features, relying on external libraries or custom functions for specifics like date parsing.

Alternatives:
1. `os.date` and `os.time` functions work for basic needs. They are part of the standard library.
2. Patterns (Lua's version of regex) to extract parts of the string manually.
3. External libraries, like `luadate` if advanced date manipulation is needed.

Implementation details:
- `os.time` creates a time object from a table. 
- Patterns can be complex based on the date format. Always validate to avoid errors.
- Don't forget time zones and locales when dealing with dates.

## See Also (Дивись Також)
- Official Lua documentation: http://www.lua.org/manual/5.4/manual.html#6.9
- LuaDate library for more complex operations: https://github.com/Tieske/date
- A tutorial on Lua patterns: https://www.lua.org/pil/20.2.html
