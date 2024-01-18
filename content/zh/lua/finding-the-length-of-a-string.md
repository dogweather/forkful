---
title:                "找到字符串的长度"
html_title:           "Lua: 找到字符串的长度"
simple_title:         "找到字符串的长度"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串长度及其作用
字符串长度指的是字符串中包含的字符的数量。程序员经常需要获取字符串长度以便在处理文本时使用。例如，可以使用字符串长度来确保输入的密码长度满足安全要求，或者将字符串截断为特定长度以适合显示。

## 如何获取字符串长度
```Lua
-- 使用 string.len() 函数获取字符串长度
local str = "Hello world!"
print(string.len(str))
-- 输出结果为 12
```

## 深入了解
字符串长度的概念可以追溯到早期的编程语言。在 Lua 中，字符串长度可以通过使用 string.len() 函数来获取，也可以通过遍历字符串的每个字符并计数来实现。此外，还有其他一些编程语言将字符串长度作为内置函数提供，如 Java 中的 .length() 方法。

## 相关资源
- [Lua 官方文档](https://www.lua.org/docs.html)
- [Lua 教程](https://www.w3schools.com/lua/)
- [Lua 字符串操作](https://www.tutorialspoint.com/lua/lua_strings.htm)