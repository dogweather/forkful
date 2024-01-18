---
title:                "插入字串"
html_title:           "Lua: 插入字串"
simple_title:         "插入字串"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Lua 编程：String 插值

## 什么是String插值？
String 插值是一种快速而方便的方法，可以在字符串中插入变量的值。这使得程序员能够在字符串中动态地添加变量值，而不是硬编码它们。这使得代码更有可读性，并且可以节省大量的时间和努力。

## 如何实现String插值？
使用字符串插值非常简单。只需在变量名称前加上一次 '%'，然后将变量添加到字符串中即可。让我们来看一个例子：
```Lua
local name = "John"
local age = 25
-- 使用字符串插值
print("%s is %d years old.", name, age)
```
输出：John is 25 years old.

## 深入挖掘
- 历史背景：String插值最初在Perl语言中实现，随后被引入到多种编程语言中。
- 替代方法：除了字符串插值，程序员也可以使用字符串连接符来构建一个包含变量值的字符串。
- 实现细节：在Lua中，字符串插值是通过调用`string.format()`函数来实现的，该函数使用类似于C语言中的`printf()`函数的语法。

## 相关资料
- [Lua官方文档](https://www.lua.org/docs.html)
- [Lua字符串插值的实现](https://github.com/lua/lua/blob/master/lstrlib.c)