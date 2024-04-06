---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:57.645115-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua\u6CA1\u6709\u5185\u7F6E\u652F\u6301\
  \u65E5\u671F\u548C\u65F6\u95F4\u64CD\u4F5C\uFF0C\u8D85\u51FA\u4E86`os.date`\u548C\
  `os.time`\u51FD\u6570\u63D0\u4F9B\u7684\u6709\u9650\u529F\u80FD\u3002\u7136\u800C\
  \uFF0C\u8FD9\u4E9B\u529F\u80FD\u53EF\u4EE5\u88AB\u7528\u4E8E\u57FA\u672C\u89E3\u6790\
  \uFF0C\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u9700\u6C42\uFF0C\u53EF\u4EE5\u4F7F\u7528\
  \u5916\u90E8\u5E93`luadate`\u3002 **\u4F7F\u7528`os.date`\u548C`os.time`\uFF1A**."
lastmod: '2024-04-05T22:38:47.079112-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua\u6CA1\u6709\u5185\u7F6E\u652F\u6301\u65E5\
  \u671F\u548C\u65F6\u95F4\u64CD\u4F5C\uFF0C\u8D85\u51FA\u4E86`os.date`\u548C`os.time`\u51FD\
  \u6570\u63D0\u4F9B\u7684\u6709\u9650\u529F\u80FD\u3002\u7136\u800C\uFF0C\u8FD9\u4E9B\
  \u529F\u80FD\u53EF\u4EE5\u88AB\u7528\u4E8E\u57FA\u672C\u89E3\u6790\uFF0C\u5BF9\u4E8E\
  \u66F4\u590D\u6742\u7684\u9700\u6C42\uFF0C\u53EF\u4EE5\u4F7F\u7528\u5916\u90E8\u5E93\
  `luadate`\u3002 **\u4F7F\u7528`os.date`\u548C`os.time`\uFF1A**."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

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
