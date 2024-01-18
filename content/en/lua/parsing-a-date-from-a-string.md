---
title:                "Parsing a date from a string"
html_title:           "Lua recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string is the process of extracting information from a string that represents a date, such as "Monday, June 1st 2020". Programmers do this in order to convert the string into a date object that can be manipulated and used in their code.

## How to:
To parse a date from a string in Lua, we can use the built-in os.date() function. This function takes two arguments: a format string and a time value. The format string specifies the desired date format and the time value is the string that we want to parse.

Example:
```
-- Parse a short date string
local date = os.date("%m/%d/%y", "06/01/20")
print(date) --> 06/01/20

-- Parse a full date string
local date = os.date("%A, %B %d %Y", "Monday, June 1st 2020")
print(date) --> Monday, June 1st 2020
```

## Deep Dive:
Historically, parsing dates from strings has been done manually by programmers, as there were no built-in functions for this task. However, modern languages like Lua have included built-in functions to make this process easier.

An alternative method for parsing dates in Lua is to use the string.gmatch() function. This function allows you to match patterns in a string and extract the matched values.

Example:
```
-- Extract the day and month from a string
local text = "Monday, June 1st 2020"
local day, month = string.gmatch(text, "%a+"), string.gmatch(text, "%a+")
print(day, month) --> Monday, June

-- You can then manipulate the extracted values to create a date object according to your needs.
```

When parsing dates from strings, it's important to consider the various date formats that may be used. You may need to use different format strings depending on the format of the string you are parsing.

## See Also:
- [Lua documentation for os.date() function](https://www.lua.org/manual/5.3/manual.html#pdf-os.date)
- [Lua documentation for string.gmatch() function](https://www.lua.org/manual/5.3/manual.html#6.4.1)
- [W3Schools tutorial on parsing dates in Lua](https://www.w3schools.com/lua/func_os_date.asp)