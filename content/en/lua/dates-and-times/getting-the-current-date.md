---
date: 2024-02-03 19:02:35.939026-07:00
description: "How to: Lua provides the `os.date` function to get the current date\
  \ and time. The function can be used without arguments to get a formatted string\
  \ or with\u2026"
lastmod: '2024-03-13T22:45:00.214961-06:00'
model: gpt-4-0125-preview
summary: Lua provides the `os.date` function to get the current date and time.
title: Getting the current date
weight: 29
---

## How to:
Lua provides the `os.date` function to get the current date and time. The function can be used without arguments to get a formatted string or with format specifiers to customize the output. Here's how to use it:

```lua
-- Getting the current date and time as a formatted string
print(os.date())  -- e.g., Thu Mar  3 14:02:03 2022

-- Customizing the output format
-- %Y for year, %m for month, %d for day, %H for hour, %M for minutes
print(os.date("%Y-%m-%d %H:%M"))  -- e.g., 2022-03-03 14:02
```

For more sophisticated date and time manipulation, Lua does not have built-in libraries as rich as some other programming languages. However, you can use third-party libraries such as `lua-date` (https://github.com/Tieske/date). This library offers more comprehensive functionalities for manipulating dates and times. Here's how you might use it:

First, ensure you have installed the `lua-date` library. You can typically install it using LuaRocks with the following command:

```bash
luarocks install lua-date
```

Then, you can use it in your Lua script like so:

```lua
local date = require("date")

-- Creating a date object for the current date and time
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- e.g., 2022-03-03 14:02:03
```

This example demonstrates the creation of a `date` object representing the current moment, which you can then format similarly to the `os.date` function but with added flexibility and options provided by the `lua-date` library.
