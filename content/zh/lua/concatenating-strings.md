---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

文章标题：Lua（当前版本）中的字符串连接

## 什么 & 为什么？
字符串连接就是把两个或更多的字符串进行组合。程序员常进行字符串连接操作，以便储存和操作更多的信息。

## 如何操作：
以下是字符串连接的代码示例和样例输出：
```Lua
str1 = "你好，"
str2 = "世界！"
print(str1 .. str2)
```
输出结果： 
```Lua
你好，世界！
```

## 深层了解
字符串连接来源于编程的早期时代，因为那时候的内存极其有限，所以只能用这个方法处理字符串。在Lua中，字符串连接的实施细节关键在于".."操作符，它使得连接操作更容易和直观。然而，你也可以使用string.format或者table.concat等的可替代方法，尤其在处理大量的字符串时，这些方法会提供更优秀的性能。

## 参考资料
1. [Lua字符串连接官方文档](http://www.lua.org/manual/5.2/manual.html#3.4.5)
2. [学习Lua字符串连接的更多知识](https://www.tutorialspoint.com/lua/lua_strings.htm)
3. [在Lua中更有效的字符串连接历史](https://programminghistorian.org/en/lessons/counting-frequencies)
请注意，所有链接的内容都是英文的。