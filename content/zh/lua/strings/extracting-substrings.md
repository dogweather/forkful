---
date: 2024-01-20 17:46:09.099427-07:00
description: "How to \u5982\u4F55\u64CD\u4F5C\uFF1A \u63D0\u53D6\u5B50\u4E32\u529F\
  \u80FD\u4ECELua\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.204313-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to 如何操作：
```Lua
-- 基本用法
local text = "Hello, Lua!"
local substring = text:sub(8, 10)
print(substring) -- 输出 "Lua"

-- 使用负数索引
local back_substring = text:sub(-4, -2)
print(back_substring) -- 输出 "Lua"

-- 提取至末尾
local end_substring = text:sub(8)
print(end_substring) -- 输出 "Lua!"
```

## Deep Dive 深入剖析：
提取子串功能从Lua 1.0就存在了，使用`string.sub`函数。在Lua中，字符串索引从1开始，这和一些其他语言（如C或Java）不同。我们也可以使用负数索引，它表示倒数第几个字符。除此之外，我们可以使用模式匹配来提取复杂条件下的子串——使用`string.match`。要注意，子串操作在Lua中是安全的，即使索引超出范围也不会出错，但会返回空字符串或原字符串。

## See Also 相关链接：
- Lua官方文档关于字符串： https://www.lua.org/manual/5.4/manual.html#6.4
- Lua用户维基对字符串操作的讨论： http://lua-users.org/wiki/StringLibraryTutorial

记得，掌握字符串操作对编程非常重要，无论是简单的数据处理还是复杂的文本分析。
