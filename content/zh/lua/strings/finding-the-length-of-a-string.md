---
title:                "获取字符串的长度"
aliases:
- /zh/lua/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:41.646089-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/finding-the-length-of-a-string.md"
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
