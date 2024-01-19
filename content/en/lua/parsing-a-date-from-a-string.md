---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string in programming involves reading a text input in the format of a date (such as '12/01/2021') and transforming it into a data representation that the program can use. This is an essential task for handling user input, processing data from APIs or databases, and for various other use cases.

## How to:

Lua does not have a built-in date parsing method. But, thanks to its string manipulation functions, we are able to parse dates from strings.

Let's say we have a date in this format 'dd/mm/yyyy'. Here's a simple example in Lua:

```Lua
function stringToDate(dateStr)
    local day, month, year = dateStr:match("(%d+)/(%d+)/(%d+)")
    return { day = tonumber(day), month = tonumber(month), year = tonumber(year) }
end

local date = stringToDate("12/01/2021")
print(date.day, date.month, date.year) -- outputs: 12 01 2021
```
On running this script, the Lua interpreter will print `12 01 2021`, representing day, month, and year, respectively.

## Deep Dive 

Lua, a lightweight scripting language, doesn't provide an extensive date-time API like Python or JavaScript. It only offers a simple `os.date` function that formats current time or a given timestamp. Hence, to parse a date from a string, programmers need to resort to string manipulation.

An alternative approach is to use Lua-based libraries, such as lua-date, which provide higher-level date and time manipulation functions. Lua-date can handle different date formats and timezones, providing a more flexible solution.

The implementation detail of our script uses a basic regular expression to extract day, month, and year from the string, which are then converted to a number using `tonumber` due to Lua treating matched strings as text by default.

## See Also

For more details and advanced uses, you can check out the following resources:
* [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
* [Lua-date library on GitHub](https://github.com/Tieske/date)
* [Lua String Manipulation Tutorial](https://www.tutorialspoint.com/lua/lua_strings.htm)