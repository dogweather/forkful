---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:08.139729-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua\u63D0\u4F9B\u4E86`os.date`\u51FD\u6570\
  \u4EE5\u83B7\u53D6\u5F53\u524D\u7684\u65E5\u671F\u548C\u65F6\u95F4\u3002\u8FD9\u4E2A\
  \u51FD\u6570\u53EF\u4EE5\u4E0D\u5E26\u53C2\u6570\u4F7F\u7528\uFF0C\u4EE5\u83B7\u53D6\
  \u683C\u5F0F\u5316\u7684\u5B57\u7B26\u4E32\uFF0C\u6216\u8005\u4F7F\u7528\u683C\u5F0F\
  \u8BF4\u660E\u7B26\u6765\u81EA\u5B9A\u4E49\u8F93\u51FA\u3002\u4EE5\u4E0B\u662F\u5982\
  \u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:47.080321-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua\u63D0\u4F9B\u4E86`os.date`\u51FD\u6570\
  \u4EE5\u83B7\u53D6\u5F53\u524D\u7684\u65E5\u671F\u548C\u65F6\u95F4\u3002\u8FD9\u4E2A\
  \u51FD\u6570\u53EF\u4EE5\u4E0D\u5E26\u53C2\u6570\u4F7F\u7528\uFF0C\u4EE5\u83B7\u53D6\
  \u683C\u5F0F\u5316\u7684\u5B57\u7B26\u4E32\uFF0C\u6216\u8005\u4F7F\u7528\u683C\u5F0F\
  \u8BF4\u660E\u7B26\u6765\u81EA\u5B9A\u4E49\u8F93\u51FA\u3002\u4EE5\u4E0B\u662F\u5982\
  \u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
