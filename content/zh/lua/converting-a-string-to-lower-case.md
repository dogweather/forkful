---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么&为什么？

在Lua编程中，将字符串转换为小写的过程是指将字符串中的所有大写英文字母转换为对应的小写字母。程序员之所以这样做，主要是为了实现字符串的统一格式，使其能够适应不区分大小写的字符比较。

## 如何实现：

在Lua中，我们使用 `string.lower()` 方法来将字符串转换为小写。

示例代码和输出：

```Lua
str = "Hello World!"
print(string.lower(str))
```

输出:

```Lua
hello world!
```

这段代码表示，我们首先定义了一个字符串"Hello World!"，然后使用`string.lower()`将其转换为小写，并输出到控制台。

## 深入：

1. 历史背景：在Lua的早期版本中（如Lua 5.0），`string.lower()`函数使用的是C库函数`tolower()`来进行转换。随着Lua版本的升级，该函数的实现算法进行了优化以提高性能。

2. 替代方案：除了`string.lower()`外，我们也可以使用`utf8.lower()`函数处理包含非ASCII字符的字符串。

3. 实现细节：`string.lower()`函数的实现并不局限于ASCII字符集。它可以处理任何遵循Lua字符集规则的字符串。

## 参阅：

这里有一些链接是与字符串转换相关的资源：

1. [Lua字符串函数](http://www.runoob.com/lua/lua-string.html) — runoob.com

2. [Lua`string`库](http://lua-users.org/wiki/StringLibraryTutorial) - lua-users.org

3. [在Lua中处理字符串](https://stackoverflow.com/questions/tagged/lua+string) - StackOverflow

希望这篇文章能帮助你理解和掌握Lua中将字符串转化为小写的方法和相关知识。