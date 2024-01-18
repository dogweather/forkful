---
title:                "Converting a date into a string"
html_title:           "Lua recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string in Lua is the process of changing a date object, which is formatted as a numerical value, into a textual representation. Programmers do this to display dates in a more user-friendly and readable format, such as "Month Day, Year," rather than just numbers.

## How to:
To convert a date into a string in Lua, you can use the `os.date()` function. This function takes in a date format string and a time value, and returns a string representing the date in the specified format. Here's an example code:

```Lua
local date = os.date("%B %d, %Y", 1567209600)
print(date)
```

Output: September 01, 2019

In the above code, we used the `%B %d, %Y` format, which will result in a string with the month, day, and full year. The second parameter `1567209600` is the time value, which is the number of seconds since January 1st, 1970. The `os.date()` function also has other format options available, such as `%m/%d/%y` for a short date format.

## Deep Dive:
Converting a date into a string is a common task in programming, as dates are often displayed to users in a more readable format. Before the `os.date()` function was introduced in Lua 5.1, programmers had to manually write functions to convert a date into a string. The `os.date()` function also has a built-in time zone adjustment feature, making it easier for programmers to handle different time zones.

An alternative to using `os.date()` is the `string.format()` function, which allows for more formatting options but requires more lines of code to achieve the same result. Additionally, there are third-party libraries available that offer advanced date and time manipulation in Lua.

## See Also:
- [Lua Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.9)
- [string.format() function](https://www.lua.org/manual/5.4/manual.html#pdf-string.format)
- [Third-party Lua libraries for date and time manipulation](https://luarocks.org/search?q=date)