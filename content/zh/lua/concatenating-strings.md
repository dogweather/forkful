---
title:                "串接字符串"
html_title:           "Lua: 串接字符串"
simple_title:         "串接字符串"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

# 什么是字符串连接以及为什么程序员要这样做？

字符串连接是指将几个独立的字符串组合成一个新的字符串的操作。程序员通常会使用字符串连接来生成更复杂的字符串，例如在输出文本信息时。这样做可以使代码更加简洁和易于阅读。

# 怎么做？

让我们来看一个简单的例子来说明如何使用Lua进行字符串连接：
```Lua
local str1 = "Hello"
local str2 = "World"
local str3 = str1 .. str2
print(str3)
```
输出结果：
```
HelloWorld
```
上面的代码中，我们首先创建了两个字符串变量"Hello"和"World"，然后使用".."(两个点)操作符将它们连接起来，并赋值给变量str3。最后，通过输出函数print()来打印结果。

# 深入了解

字符串连接在编程语言中已经很常见，它的起源可以追溯到早期的计算机编程语言，如BASIC和FORTRAN。在Lua中，字符串连接是通过使用.."操作符实现的，但是也可以使用字符串库中的函数来实现，如string.format()。

除了字符串连接，程序员还可以使用字符串插值来生成复杂的字符串，这种方法可以更加直观和易于理解。然而，对于一些特殊的需求，仍然需要使用字符串连接来构建所需的字符串。

# 相关资源

- [Lua官方文档](https://www.lua.org/)
- [Wikipedia：字符串连接](https://en.wikipedia.org/wiki/String_concatenation)