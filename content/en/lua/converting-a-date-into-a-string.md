---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Converting A Date Into A String in Lua: What, Why, and How

## What & Why?

Converting a date into a string changes a data type in Lua from a date format to a read-friendly string. Programmers do this to format dates for display, log records, or to enable certain operations.

## How to:
Here's how you do it in Lua:

```Lua
os.date("*t", os.time()) --{} representation

os.date("%Y-%m-%d-%H-%M-%S", os.time()) --String representation

os.date() --default representation
```
Running the above code will give you output like:

```Lua
os.date("*t", os.time()) --{ "year" = 2023, "month" = 3,  "day" = 22, "hour" = 7, "min" = 54, "sec" = 31, "wday" = 5, "yday" = 81, isdst = false }

os.date("%Y-%m-%d-%H-%M-%S", os.time()) --"2023-03-23-07-54-31"

os.date() -- "Thu Mar 23 07:54:31 2023"
```

## Deep Dive

Historically, date to string conversion is rooted in managing unformatted date data effectively. Before this, dates were not that programmatically friendly. 

Alternatives in Lua for this task are using custom libraries like `date.lua` or `Chronos`. But `os.date` is already built-in, making it a convenient choice.

When converting dates using `os.date`, tables or strings can be outputted. `os.date("*t", os.time())` returns a table, whereas `os.date("%Y-%m-%d-%H-%M-%S", os.time())` gives a string. Remember `os.date()` defaults to a string in the format "Thu Mar 23 07:54:31 2023". 

## See Also 

- Lua documentation on `os.date`: https://www.lua.org/pil/22.1.html
- Stack Overflow discussion on date conversion in Lua: https://stackoverflow.com/questions/36057993/how-to-convert-timestamp-to-date-in-lua
- Understanding `os.date` format specifiers: https://riptutorial.com/lua/example/4184/format-specifiers-in-os-date