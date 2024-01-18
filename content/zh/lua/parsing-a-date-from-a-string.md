---
title:                "解析字符串中的日期。"
html_title:           "Lua: 解析字符串中的日期。"
simple_title:         "解析字符串中的日期。"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

"What & Why?": 
当我们从一个字符串中提取日期时，我们其实是在进行日期解析。程序员通常会这样做是为了把一个日期作为一个变量或者数据结构的一部分来使用，或者是为了改变日期的格式。

## How to:
日期解析在Lua中可以用不同的方法实现。下面是一个基本的实现示例，它会把一个字符串中表示的日期解析为三个数字，分别代表年、月和日的值。

```Lua
-- 输入一个日期字符串
local date_str = "2020-01-31"

-- 使用字符串的分割方法来获取年、月、日的值
local year = date_str:sub(1, 4)
local month = date_str:sub(6, 7)
local day = date_str:sub(9, 10)

print("Year: " .. year)
print("Month: " .. month)
print("Day: " .. day)
```

此代码的输出将是：

```
Year: 2020
Month: 01
Day: 31
```

## Deep Dive:
在计算机编程中，日期解析是一个常见的操作。在过去的几十年中，随着计算机的发展和广泛应用，人们需要对日期进行更精确的处理和转换。因此，各种编程语言都提供了日期解析的功能，包括Lua。

除了上面的示例代码，Lua也提供了相应的日期库来处理更复杂的日期操作，如日期的比较、格式化、时区转换等。其中比较流行的是[luatz](https://github.com/daurnimator/luatz)和[lua-date](https://github.com/Tieske/date).

## See Also:
- Lua官方文档 - [http://www.lua.org/manual/5.3/manual.html](http://www.lua.org/manual/5.3/manual.html)
- Lua日期库luatz - [https://github.com/daurnimator/luatz](https://github.com/daurnimator/luatz)
- Lua日期库lua-date - [https://github.com/Tieske/date](https://github.com/Tieske/date)