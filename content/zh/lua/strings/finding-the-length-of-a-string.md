---
date: 2024-01-20 17:47:41.646089-07:00
description: "\u5728Lua\u4E2D\uFF0C\u4E86\u89E3\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\
  \u5C31\u662F\u77E5\u9053\u5B83\u5305\u542B\u591A\u5C11\u4E2A\u5B57\u7B26\u3002\u8FD9\
  \u6837\u505A\u53EF\u4EE5\u5E2E\u6211\u4EEC\u5904\u7406\u6587\u672C\u6570\u636E\uFF0C\
  \u6BD4\u5982\u5224\u65AD\u8F93\u5165\u7684\u5408\u6CD5\u6027\u6216\u8005\u8FDB\u884C\
  \u5B57\u7B26\u4E32\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.946666
model: gpt-4-1106-preview
summary: "\u5728Lua\u4E2D\uFF0C\u4E86\u89E3\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u5C31\
  \u662F\u77E5\u9053\u5B83\u5305\u542B\u591A\u5C11\u4E2A\u5B57\u7B26\u3002\u8FD9\u6837\
  \u505A\u53EF\u4EE5\u5E2E\u6211\u4EEC\u5904\u7406\u6587\u672C\u6570\u636E\uFF0C\u6BD4\
  \u5982\u5224\u65AD\u8F93\u5165\u7684\u5408\u6CD5\u6027\u6216\u8005\u8FDB\u884C\u5B57\
  \u7B26\u4E32\u64CD\u4F5C\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在Lua中，了解字符串的长度就是知道它包含多少个字符。这样做可以帮我们处理文本数据，比如判断输入的合法性或者进行字符串操作。

## 如何操作：
```Lua
-- 使用#操作符
local str = "你好，世界！"
print(#str)  -- 输出字符串长度
```
输出：
```
15
```

```Lua
-- 使用string.len函数
local str = "Lua编程"
print(string.len(str))  -- 同样输出字符串长度
```
输出：
```
9
```

## 深入了解
在Lua里，字符串是不可变的值。得知字符串长度是常见的需求，通常用于循环、分割或者其他文本处理任务。原生Lua使用`#`操作符或`string.len()`函数来获取字符串长度。这种设计思路源于Lua的简单和效率原则。

历史背景方面，Lua自5.0版本引入了`#`操作符，之前只能通过`string.len()`函数得知长度。`#`提供了便捷的方式，但是不处理UTF-8字符串。在多语言编程中，使用`utf8.len()`来正确处理多字节字符，这在Lua 5.3中引入。

替代方案可能涉及自己编写函数来计算字符串长度，特别是处理特殊编码时。Lua中字符串的实现采用内部结构保存长度信息，这就是为什么使用`#`操作符和`string.len()`即快速又准确。

## 查看更多
- Lua 5.4参考手册: http://www.lua.org/manual/5.4/
- Lua字符串教程: https://www.lua.org/pil/21.1.html
- UTF-8支持: https://www.lua.org/manual/5.4/manual.html#6.5
