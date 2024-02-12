---
title:                "Converting a date into a string"
aliases: - /en/lua/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:46.946180-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a date into a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date to a string is about changing how date/time data is displayed. Programmers do this for readability, localization, or formatting consistency across applications.

## How to:
In Lua, we use `os.date` to format dates into strings. Here's a slice of code to chew on.

```lua
local now = os.time()
local formatted = os.date("%Y-%m-%d %H:%M:%S", now)
print(formatted)
-- Example output: 2023-04-01 15:24:37
```

Want a different flavor? Customize the string pattern.

```lua
local friendly_format = os.date("%B %d, %Y")
print(friendly_format)
-- Example output: April 01, 2023
```

## Deep Dive
Lua's `os.date` function is modeled after the POSIX `strftime` function. If you squint, you'll notice it's similar to C's `printf` family—same roots. 

Alternatives? Sure. You could wrestle with string concatenation and table indexing—manually grabbing date parts. But why sweat when `os.date` handles it?

Implementation details? The `os.date` function can behave in two ways: 
- Given a format string, it returns the formatted date.
- Omit the format, and it returns a table with date components.

Fun fact: Lua's time-related functions use the epoch as reference—the number of seconds since Jan 1, 1970. This quirk traces back to Unix time. 

## See Also
- Lua's reference manual on `os.date`: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
- strftime format specifiers to spice up `os.date`: http://strftime.org/
- A dive into Unix epoch time for the curious: https://en.wikipedia.org/wiki/Unix_time
