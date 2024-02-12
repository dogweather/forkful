---
title:                "从字符串解析日期"
aliases:
- /zh/lua/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:57.645115-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？
从字符串解析日期涉及将日期和时间的文本表示转换成可以在Lua程序中轻松操作、存储或比较的格式。程序员执行这项任务是为了便于执行调度、记录或任何时间计算，以及桥接人类可读的日期格式与计算机能高效处理的结构化数据类型之间的差距。

## 如何操作：
Lua没有内置支持日期和时间操作，超出了`os.date`和`os.time`函数提供的有限功能。然而，这些功能可以被用于基本解析，对于更复杂的需求，可以使用外部库`luadate`。

**使用`os.date`和`os.time`：**
```lua
-- 将人类可读的日期转换为时间戳，然后再转回来
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- 将时间戳转换回人类可读格式
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- 输出：2023-09-21 15:00:00
```

**使用`luadate`（第三方库）：**
要使用`luadate`，请确保通过LuaRocks或您选择的包管理器安装。`luadate`增加了广泛的日期和时间解析及操作能力。

```lua
local date = require('date')

-- 直接解析日期字符串
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- 输出：2023-09-21 15:00:00

-- 增加持续时间
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- 输出：2023-09-28 15:00:00
```

`luadate`库提供了一种更直观、更强大的处理日期方式，包括从字符串解析、格式化，以及对日期执行算术运算，这极大地简化了在Lua中处理时间数据的工作。
