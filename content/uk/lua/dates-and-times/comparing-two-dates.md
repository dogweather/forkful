---
date: 2024-01-20 17:33:34.944699-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Lua doesn't have built-in date comparison functions. Instead, you compare\
  \ timestamps. Historically, timestamps represent seconds\u2026"
lastmod: '2024-04-05T22:51:02.562333-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Lua doesn't have built-in date comparison functions."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## How to: (Як це зробити:)
```Lua
-- Load the os.date table
local date1 = os.date("*t", os.time({year=2023, month=3, day=15}))
local date2 = os.date("*t", os.time({year=2023, month=4, day=15}))

-- Compare dates
if os.time(date1) < os.time(date2) then
  print("Date1 is earlier than Date2")
else
  print("Date2 is earlier than Date1")
end
```
Sample Output:
```
Date1 is earlier than Date2
```

## Deep Dive (Поглиблений Розбір:)
Lua doesn't have built-in date comparison functions. Instead, you compare timestamps.

Historically, timestamps represent seconds since the epoch (January 1, 1970). They make date comparison straightforward with simple arithmetic. Lua's `os.time()` turns a date table into a timestamp.

Utilities like `os.date("*t", ...)` build date tables. Alternatives in Lua include custom date libraries or parsing date strings manually. But be wary of timezones and daylight saving time.

Using native Lua functions ensures compatibility and simplicity. Timestamps are integers, so they're also efficient to compare.

## See Also (Дивіться також):
- Lua 5.4 Reference Manual: [os.date](http://www.lua.org/manual/5.4/manual.html#pdf-os.date) and [os.time](http://www.lua.org/manual/5.4/manual.html#pdf-os.time)
- GitHub: Lua Date Libraries (e.g., lua-date): [https://github.com/Tieske/date](https://github.com/Tieske/date)
