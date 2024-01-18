---
title:                "将字符串转换为小写"
html_title:           "Lua: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 什么和为什么？
Lua是一种流行的编程语言，通常用于游戏开发和嵌入式系统。在Lua中，将字符串转换为小写是一种常见的操作，它允许程序员将字符串转换为其所有字母都是小写的格式。这样做有助于标准化字符串，并允许程序员更轻松地比较字符串的内容。

# 如何：
 ```Lua
 -- 示例1：将字符串转换为小写
 local str = "Hello World!"
 print(string.lower(str))  -- 输出：hello world!
 
 -- 示例2：使用for循环将字符串的每个字符转换为小写
 local str = "Hello World!"
 for i = 1, #str do  -- #操作符用于获取字符串的长度
   local char = str:sub(i, i)  -- 使用:sub()方法获取字符串的指定索引处的字符
   print(char:lower(), end="")  -- 使用:lower()方法转换字符为小写并输出，end参数用于在同一行打印
 end
 -- 输出：hello world!
 ``` 

# 深入探讨：
- 历史背景：字符串转换为小写的需求在早期计算机系统中很常见。在ASCII编码中，小写字母和大写字母的ASCII码是连续的，因此只需将大写字母的ASCII码加上32即可转换为小写字母的ASCII码。Lua也继承了这一特性。
- 其他替代方法：除了使用Lua内置的string.lower()方法，还可以使用string.gsub()方法将字符串内的大写字母替换为小写字母，或者使用string.lower()方法配合string.gsub()方法来实现更复杂的转换。
- 实现细节：Lua的string.lower()方法通过对字符串中的每个字符进行ASCII码加32操作来实现转换。在一些语言中，对于特定语言的字符，转换为小写可能需要更复杂的处理。

# 参考资料：
1. Lua官方文档：https://www.lua.org/docs.html
2. ASCII编码表：http://www.asciitable.com/
3. Lua string库：https://www.lua.org/pil/20.html