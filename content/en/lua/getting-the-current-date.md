---
title:                "Getting the current date"
html_title:           "Lua recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Lua refers to retrieving the current date and time information from the system. Programmers often do this to keep track of events, schedule tasks, or timestamp data. 

## How to:

To get the current date in Lua, you can use the ```os.date()``` function. This function takes in a format string as a parameter and returns a string representing the current date and time in the specified format.

Here's an example code snippet to get the current date and time in the default format:
```
currentDate = os.date()
print(currentDate)
```
Output:
```
Wed Nov 24 15:53:55 2021
```

You can also specify a custom format string to get the date and time in a desired format. For example:
```
customDate = os.date("%m-%d-%Y %H:%M:%S")
print(customDate)
```
Output:
```
11-24-2021 15:53:55
```

## Deep Dive:

Historically, Lua did not have an inbuilt function to retrieve the current date and time information. However, it was added in version 5.4 as part of the ```os``` library. 

An alternative to using the ```os.date()``` function is the ```os.time()``` function, which returns the current system time as the number of seconds since January 1, 1970. This can be converted to a readable format using the ```os.date()``` function.

The default format string used by ```os.date()``` is ```"%a %b %d %H:%M:%S %Y"``` which stands for weekday, month, day, hour, minute, second, and year respectively. However, you can use a variety of format options to get the desired output. Consult the [Lua manual](https://www.lua.org/manual/5.4/manual.html#pdf-os.date) for more information on format options.

## See Also:

- [Lua manual - os library](https://www.lua.org/manual/5.4/manual.html#6.10)
- [Tutorialspoint - Lua os library](https://www.tutorialspoint.com/lua/lua_os_module.htm)
- [Lua.org forum - Thread on getting the current date in Lua](https://www.lua.org/pipermail/lua-l/2005-September/014588.html)