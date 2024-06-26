---
date: 2024-01-20 17:58:21.760606-07:00
description: "How to: \u5982\u4F55\u8FDB\u884C \u7B80\u6D01\u4EE3\u7801\uFF0C\u5FEB\
  \u901F\u7406\u89E3\u3002`gsub` \u65B9\u6CD5\u5B9E\u73B0\u641C\u7D22\u66FF\u6362\uFF0C\
  \u62EC\u53F7\u5185\u4E09\u4E2A\u53C2\u6570\uFF1A\u67E5\u627E\u5185\u5BB9\u3001\u66FF\
  \u6362\u5185\u5BB9\u548C\u539F\u59CB\u6587\u672C\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.200733-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u8FDB\u884C \u7B80\u6D01\u4EE3\u7801\uFF0C\u5FEB\u901F\u7406\
  \u89E3\u3002`gsub` \u65B9\u6CD5\u5B9E\u73B0\u641C\u7D22\u66FF\u6362\uFF0C\u62EC\u53F7\
  \u5185\u4E09\u4E2A\u53C2\u6570\uFF1A\u67E5\u627E\u5185\u5BB9\u3001\u66FF\u6362\u5185\
  \u5BB9\u548C\u539F\u59CB\u6587\u672C\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to: 如何进行
```Lua
local text = "Hello World! Today is a great day."
local searchText = "great"
local replaceWith = "wonderful"

local result = text:gsub(searchText, replaceWith)

print(result)
-- Output: Hello World! Today is a wonderful day.
```
简洁代码，快速理解。`gsub` 方法实现搜索替换，括号内三个参数：查找内容、替换内容和原始文本。

## Deep Dive 深入了解
搜索替换从文本编辑早期就有了，像是`sed`工具在Unix系统上。Lua的`gsub`函数是内置的字符串处理功能中的一部分，高效且易用。你可以指定模式匹配(patter matching)复杂查找，或替换所有匹配项。举个例子，可以使用正则表达式来指定模式：

```Lua
local text = "Lua is fun, because Lua is cool and Lua is easy!"
local pattern = "(Lua) is"
local replaceWith = "%1 was"

local result = text:gsub(pattern, replaceWith)
print(result)
-- Output: Lua was fun, because Lua was cool and Lua was easy!
```

这里`%1`代表模式中第一个捕获（括号内的内容），在替换时使用。

## See Also 相关资源
- Lua用户手册: https://www.lua.org/manual/5.4/
- `Pattern Matching`教程: https://www.lua.org/pil/20.2.html
- Lua社区讨论: http://www.luafaq.org/

记得实践，多编程，才能深入学习Lua搜索替换的妙用。
