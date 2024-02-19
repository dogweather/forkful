---
aliases:
- /zh/lua/concatenating-strings/
date: 2024-01-20 17:35:18.818263-07:00
description: "\u5728Lua\u4E2D\uFF0C\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\
  \u4E24\u4E2A\u6216\u591A\u4E2A\u5B57\u7B26\u4E32\u5408\u6210\u4E00\u4E2A\u3002\u7A0B\
  \u5E8F\u5458\u62FC\u63A5\u5B57\u7B26\u4E32\u4E3A\u4E86\u521B\u5EFA\u52A8\u6001\u6D88\
  \u606F\uFF0C\u7EC4\u7EC7\u6570\u636E\uFF0C\u6216\u6784\u5EFA\u7A0B\u5E8F\u8FD0\u884C\
  \u65F6\u624D\u80FD\u786E\u5B9A\u7684\u5B57\u7B26\u4E32\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.246545
model: gpt-4-1106-preview
summary: "\u5728Lua\u4E2D\uFF0C\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u4E24\
  \u4E2A\u6216\u591A\u4E2A\u5B57\u7B26\u4E32\u5408\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\
  \u5458\u62FC\u63A5\u5B57\u7B26\u4E32\u4E3A\u4E86\u521B\u5EFA\u52A8\u6001\u6D88\u606F\
  \uFF0C\u7EC4\u7EC7\u6570\u636E\uFF0C\u6216\u6784\u5EFA\u7A0B\u5E8F\u8FD0\u884C\u65F6\
  \u624D\u80FD\u786E\u5B9A\u7684\u5B57\u7B26\u4E32\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
在Lua中，字符串拼接就是将两个或多个字符串合成一个。程序员拼接字符串为了创建动态消息，组织数据，或构建程序运行时才能确定的字符串。

## How to: (如何操作：)
```Lua
-- 使用 .. 来拼接字符串
local greeting = "你好，"
local name = "世界！"
local message = greeting .. name
print(message) -- 输出: 你好，世界！

-- 使用 string.format 来格式化字符串
local temperature = 25.5
local weatherMessage = string.format("今天的温度是: %.1f 摄氏度。", temperature)
print(weatherMessage) -- 输出: 今天的温度是: 25.5 摄氏度。
```

## Deep Dive (深入探讨)
Lua中的字符串拼接对性能有影响：大量拼接可能会导致内存过度使用和碎片化。在Lua的早期版本中，由于内部细节如字符串是不可变的，每次拼接操作都会创建新的字符串，这在连续大量拼接时非常低效。

替代方案包括使用table.concat函数，当处理大量字符串拼接时性能更好。Lua 5.1引入了字符串缓冲区，其通过降低内存分配次数和复制操作来优化性能。

实现细节：
```Lua
-- 使用 table.concat 来拼接字符串数组
local fruits = {"苹果", "香蕉", "橙子"}
local list = table.concat(fruits, ", ")
print(list) -- 输出: 苹果, 香蕉, 橙子
```
这种方法特别适合循环中的字符串拼接操作。

## See Also (另请参阅)
- [Lua 5.4参考手册](https://www.lua.org/manual/5.4/)
- [Programming in Lua (第四版)](https://www.lua.org/pil/contents.html)
