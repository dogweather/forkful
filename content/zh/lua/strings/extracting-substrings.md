---
date: 2024-01-20 17:46:09.099427-07:00
description: "How to \u5982\u4F55\u64CD\u4F5C\uFF1A \u63D0\u53D6\u5B50\u4E32\u529F\
  \u80FD\u4ECELua\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.056565-06:00'
model: gpt-4-1106-preview
summary: "How to \u5982\u4F55\u64CD\u4F5C\uFF1A \u63D0\u53D6\u5B50\u4E32\u529F\u80FD\
  \u4ECELua 1.0\u5C31\u5B58\u5728\u4E86\uFF0C\u4F7F\u7528`string.sub`\u51FD\u6570\u3002\
  \u5728Lua\u4E2D\uFF0C\u5B57\u7B26\u4E32\u7D22\u5F15\u4ECE1\u5F00\u59CB\uFF0C\u8FD9\
  \u548C\u4E00\u4E9B\u5176\u4ED6\u8BED\u8A00\uFF08\u5982C\u6216Java\uFF09\u4E0D\u540C\
  \u3002\u6211\u4EEC\u4E5F\u53EF\u4EE5\u4F7F\u7528\u8D1F\u6570\u7D22\u5F15\uFF0C\u5B83\
  \u8868\u793A\u5012\u6570\u7B2C\u51E0\u4E2A\u5B57\u7B26\u3002\u9664\u6B64\u4E4B\u5916\
  \uFF0C\u6211\u4EEC\u53EF\u4EE5\u4F7F\u7528\u6A21\u5F0F\u5339\u914D\u6765\u63D0\u53D6\
  \u590D\u6742\u6761\u4EF6\u4E0B\u7684\u5B50\u4E32\u2014\u2014\u4F7F\u7528`string.match`\u3002\
  \u8981\u6CE8\u610F\uFF0C\u5B50\u4E32\u64CD\u4F5C\u5728Lua\u4E2D\u662F\u5B89\u5168\
  \u7684\uFF0C\u5373\u4F7F\u7D22\u5F15\u8D85\u51FA\u8303\u56F4\u4E5F\u4E0D\u4F1A\u51FA\
  \u9519\uFF0C\u4F46\u4F1A\u8FD4\u56DE\u7A7A\u5B57\u7B26\u4E32\u6216\u539F\u5B57\u7B26\
  \u4E32\u3002"
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
