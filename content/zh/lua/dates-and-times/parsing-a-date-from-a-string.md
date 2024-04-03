---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:57.645115-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u793A\u8F6C\u6362\u6210\u53EF\u4EE5\
  \u5728Lua\u7A0B\u5E8F\u4E2D\u8F7B\u677E\u64CD\u4F5C\u3001\u5B58\u50A8\u6216\u6BD4\
  \u8F83\u7684\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u9879\u4EFB\u52A1\
  \u662F\u4E3A\u4E86\u4FBF\u4E8E\u6267\u884C\u8C03\u5EA6\u3001\u8BB0\u5F55\u6216\u4EFB\
  \u4F55\u65F6\u95F4\u8BA1\u7B97\uFF0C\u4EE5\u53CA\u6865\u63A5\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u65E5\u671F\u683C\u5F0F\u4E0E\u8BA1\u7B97\u673A\u80FD\u9AD8\u6548\u5904\u7406\
  \u7684\u7ED3\u6784\u5316\u6570\u636E\u7C7B\u578B\u4E4B\u95F4\u7684\u5DEE\u8DDD\u3002"
lastmod: '2024-03-13T22:44:47.922165-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u793A\u8F6C\u6362\u6210\u53EF\u4EE5\
  \u5728Lua\u7A0B\u5E8F\u4E2D\u8F7B\u677E\u64CD\u4F5C\u3001\u5B58\u50A8\u6216\u6BD4\
  \u8F83\u7684\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u9879\u4EFB\u52A1\
  \u662F\u4E3A\u4E86\u4FBF\u4E8E\u6267\u884C\u8C03\u5EA6\u3001\u8BB0\u5F55\u6216\u4EFB\
  \u4F55\u65F6\u95F4\u8BA1\u7B97\uFF0C\u4EE5\u53CA\u6865\u63A5\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u65E5\u671F\u683C\u5F0F\u4E0E\u8BA1\u7B97\u673A\u80FD\u9AD8\u6548\u5904\u7406\
  \u7684\u7ED3\u6784\u5316\u6570\u636E\u7C7B\u578B\u4E4B\u95F4\u7684\u5DEE\u8DDD\u3002\
  ."
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
