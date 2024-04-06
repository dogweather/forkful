---
date: 2024-01-20 17:38:58.658484-07:00
description: "How to: | \u600E\u4E48\u505A: \u5728Lua\u4E2D\uFF0C\u8F6C\u6362\u5B57\
  \u7B26\u4E32\u5230\u5C0F\u5199\u5F88\u76F4\u63A5\u3002\u4F7F\u7528`string.lower()`\u51FD\
  \u6570\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.054667-06:00'
model: gpt-4-1106-preview
summary: "| \u600E\u4E48\u505A: \u5728Lua\u4E2D\uFF0C\u8F6C\u6362\u5B57\u7B26\u4E32\
  \u5230\u5C0F\u5199\u5F88\u76F4\u63A5\u3002\u4F7F\u7528`string.lower()`\u51FD\u6570\
  \uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to: | 怎么做:
在Lua中，转换字符串到小写很直接。使用`string.lower()`函数：

```Lua
local originalString = "Hello World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- 输出: hello world!
```

## Deep Dive | 深入探讨:
字符串大小写转换功能在很多语言中都是个基础功能。Lua提供的`string.lower()`函数已经存在很长时间了，是字符串处理工具箱中的一部分。如果考虑支持国际化和特殊字符，那么可能需要更复杂的库，比如Lua的`utf8`库。Lua5.3及之后版本提供内置的`utf8`库，使得操作Unicode字符串变得容易。

```Lua
local utf8String = "ÂBçD"
print(utf8.lower(utf8String))  -- 输出: âbçd
```

要注意的是，`string.lower()`不会改变原字符串; 它会返回一个新的小写字符串。

## See Also | 另请参阅:
- Lua 5.4参考手册: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
