---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么?

日期转换为字符串使我们可以更好的处理和展示日期数据。程序员通常用这种方式来格式化日期，以便在用户界面或文件中显示。

## 怎么做:

Lua 支持通过 os 库的 date 函数以多种方式进行日期到字符串的转换。以下是一个示例：

```Lua
-- 当前日期和时间
local date_time = os.date("%Y-%m-%d %H:%M:%S")
print("当前日期和时间是: " .. date_time)

-- 仅日期
local date = os.date("%Y-%m-%d")
print("今天的日期是: " .. date)

-- 仅时间
local time = os.date("%H:%M:%S")
print("当前时间是: " .. time)
```

运行这段代码，你会看到如下输出：

```Lua
当前日期和时间是: 2022-07-06 18:30:15
今天的日期是: 2022-07-06
当前时间是: 18:30:15
```

## 深入探究

- **历史背景**: 在 Lua 成为一种独立的编程语言之前，日期转换为字符串的需求已经存在了很久。这种需求普遍存在于用于数据记录、报告或者用户界面中显示日期的系统。

- **替代方案**: 如果标准的日期和时间格式无法满足需求，我们可以使用自定义格式字符串。例如 "%A, %B %d, %Y" 会返回 "Wednesday, July 06, 2022" 这样的格式。

- **实现细节**:  os.date 函数会根据我们提供的格式字符串，返回相应的日期或时间。这个函数返回的日期时间都是基于系统时间，这就意味着它会受到系统设置的影响。

## 另请参见:

- [Lua os.date 官方文档](https://www.lua.org/manual/5.1/manual.html#pdf-os.date)
- [Lua 格式化字符串教程](https://eduonix.com/blog/everything-you-need-to-know-about-strings-in-lua/)