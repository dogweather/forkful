---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:08.139729-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u8BB8\
  \u591A\u5E94\u7528\u7A0B\u5E8F\u7684\u5173\u952E\u4EFB\u52A1\uFF0C\u5305\u62EC\u65E5\
  \u5FD7\u8BB0\u5F55\u3001\u4E8B\u4EF6\u65F6\u95F4\u6233\u6216\u4EFB\u52A1\u8C03\u5EA6\
  \u3002\u5728Lua\u4E2D\uFF0C\u8FD9\u4E00\u529F\u80FD\u5141\u8BB8\u7A0B\u5E8F\u5458\
  \u5728\u5176\u5E94\u7528\u7A0B\u5E8F\u4E2D\u65E0\u7F1D\u5904\u7406\u65E5\u671F\u548C\
  \u65F6\u95F4\u64CD\u4F5C\uFF0C\u786E\u4FDD\u4ED6\u4EEC\u7684\u8F6F\u4EF6\u53EF\u4EE5\
  \u6709\u6548\u5730\u4E0E\u5B9E\u65F6\u6570\u636E\u4EA4\u4E92\u3002"
lastmod: '2024-03-13T22:44:47.923505-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u8BB8\
  \u591A\u5E94\u7528\u7A0B\u5E8F\u7684\u5173\u952E\u4EFB\u52A1\uFF0C\u5305\u62EC\u65E5\
  \u5FD7\u8BB0\u5F55\u3001\u4E8B\u4EF6\u65F6\u95F4\u6233\u6216\u4EFB\u52A1\u8C03\u5EA6\
  \u3002\u5728Lua\u4E2D\uFF0C\u8FD9\u4E00\u529F\u80FD\u5141\u8BB8\u7A0B\u5E8F\u5458\
  \u5728\u5176\u5E94\u7528\u7A0B\u5E8F\u4E2D\u65E0\u7F1D\u5904\u7406\u65E5\u671F\u548C\
  \u65F6\u95F4\u64CD\u4F5C\uFF0C\u786E\u4FDD\u4ED6\u4EEC\u7684\u8F6F\u4EF6\u53EF\u4EE5\
  \u6709\u6548\u5730\u4E0E\u5B9E\u65F6\u6570\u636E\u4EA4\u4E92\u3002."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 什么与为什么？

在编程中获取当前日期是许多应用程序的关键任务，包括日志记录、事件时间戳或任务调度。在Lua中，这一功能允许程序员在其应用程序中无缝处理日期和时间操作，确保他们的软件可以有效地与实时数据交互。

## 如何操作：

Lua提供了`os.date`函数以获取当前的日期和时间。这个函数可以不带参数使用，以获取格式化的字符串，或者使用格式说明符来自定义输出。以下是如何使用它的方法：

```lua
-- 获取当前日期和时间作为格式化字符串
print(os.date())  -- 例如，Thu Mar  3 14:02:03 2022

-- 自定义输出格式
-- %Y代表年份，%m代表月份，%d代表日，%H代表小时，%M代表分钟
print(os.date("%Y-%m-%d %H:%M"))  -- 例如，2022-03-03 14:02
```

对于更复杂的日期和时间操作，Lua并没有像一些其他编程语言那样内置丰富的库。不过，你可以使用第三方库例如`lua-date` (https://github.com/Tieske/date)。这个库为操作日期和时间提供了更全面的功能。以下是你可能会如何使用它：

首先，确保你已经安装了`lua-date`库。你通常可以使用LuaRocks来安装它，用以下命令：

```bash
luarocks install lua-date
```

然后，你可以像这样在你的Lua脚本中使用它：

```lua
local date = require("date")

-- 创建一个代表当前日期和时间的日期对象
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- 例如，2022-03-03 14:02:03
```

这个示例演示了如何创建一个代表当前时刻的`date`对象，你可以类似于`os.date`函数进行格式化，但是`lua-date`库提供了额外的灵活性和选项。
