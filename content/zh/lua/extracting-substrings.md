---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
"提取子字符串"是指从原始字符串中提取一部分。程序员通常需要它来处理、存储或操作具有特殊意义的字符串部分。

## 如何做：
Lua提供了内置的`string.sub`函数来实现字符串截取。下面是代码示例和输出结果：

```Lua
s = "Hello, Mandarin readers!"
print(string.sub(s, 8, 15))

-- Output: Mandarin
```

在此处，我们抽取了`"Hello, Mandarin readers!"`字符串中的`"Mandarin"`字串。

## 深入了解

1. 历史背景：Lua语言自诞生以来就内置了`string.sub`这个函数，这是由于在字符串处理中，子字符串的提取是很常见的需求。

2. 替代方案：`string.find`函数也可以实现相似的功能，找到特定部分的位置，然后再配合`string.sub`进行提取。

```Lua
s = "Hello, Mandarin readers!"
start, finish = string.find(s, "Mandarin")
print(string.sub(s, start, finish))

-- Output: Mandarin
```

3. 实现细节：在Lua中，字符串的索引从1开始（这与许多其他编程语言中的从0开始的索引有所不同）。`string.sub(s, start, finish)`会提取从`s`中第`start`个字符到第`finish`个字符的子串。

## 另请参阅

1. [Lua 字符串操作](http://www.runoob.com/lua/lua-strings.html)，包括`gsub`，`upper`，`lower`等详细操作指南。
2. [Lua User wiki](http://lua-users.org/wiki/StringsTutorial)，涵盖了Lua的各种字符串操作和处理方式。