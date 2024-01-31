---
title:                "获取当前日期"
date:                  2024-01-20T15:15:32.224743-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)

获取当前日期就是让程序知道现在是哪一天。程序员这么做可能是为了记录事件、标记数据或者仅仅是显示在界面上给用户看。

## How to: (如何做：)

```Lua
-- 获取当前日期和时间
local current_time = os.date("*t") -- 返回一个包含当前日期和时间的表
print("Year:", current_time.year)
print("Month:", current_time.month)
print("Day:", current_time.day)

-- 只获取当前年月日
local today = os.date("%Y-%m-%d") -- 格式化日期
print("Today is:", today)
```

示例输出：
```
Year: 2023
Month: 4
Day: 1
Today is: 2023-04-01
```

## Deep Dive (深入了解)

Lua使用`os.date`函数访问日期和时间。这个函数的设计受到了C语言标准库中`strftime`函数的影响。有两种方式来使用`os.date`：一种是格式化字符串，另一种是返回一个时间表。对于简单任务，如获取当前日期，格式化字符串足够用。但当你需要更多细节时，返回一个时间表会更有用。

您可以使用不同的格式化参数来获得各种日期和时间组件。例如，`%X`会返回当前时间，`%x`会返回当前日期。

替代方案包括操作系统提供的命令或者其他库如LuaRocks上的`luadate`。不过，对于Lua标准库来说，`os.date`简洁明了，通常是处理日期时间问题的首选方式。

实现上，`os.date`会调用操作系统底层的API来获取当前的系统时间。这意味着不同操作系统上，底层工作可能略有不同，但Lua为开发者提供了一个统一的接口。

## See Also (另请参阅)

- Lua 5.4参考手册中的os库：[http://www.lua.org/manual/5.4/manual.html#6.9](http://www.lua.org/manual/5.4/manual.html#6.9)
- strftime函数文档：[http://www.cplusplus.com/reference/ctime/strftime/](http://www.cplusplus.com/reference/ctime/strftime/)
