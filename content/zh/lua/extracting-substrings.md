---
title:                "从文本提取子字符串"
html_title:           "Lua: 从文本提取子字符串"
simple_title:         "从文本提取子字符串"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

# 什么是提取子字符串？为什么程序员要这么做？

提取子字符串是指从一个字符串中获取一个或多个子串的过程。程序员在处理字符串时需要提取子字符串，以便对其进行进一步的操作和分析，比如搜索、替换或者比较字符串。

# 如何实现提取子字符串？

### 提取单个子串：
```Lua
str = "Hello World!" --声明一个字符串变量
substr = string.sub(str, 7, 11) --提取子串，参数分别为原字符串、起始索引和结束索引
print(substr) --输出 "World"
```

### 提取多个子串：
```Lua
str = "Welcome to Lua Programming!" 
substr1, substr2 = string.match(str, "(Lua) (Programming)") --匹配并提取多个子串，参数分别为原字符串和匹配模式
print(substr1, substr2) --输出 "Lua Programming"
```

### 提取并替换子串：
```Lua
str = "apple,pear,banana,orange" 
substr = string.gsub(str, "apple", "strawberry") --替换所有匹配的子串，参数分别为原字符串、被匹配的子串和替换的子串
print(substr) --输出 "strawberry,pear,banana,orange"
```

# 深入了解
1. 提取子字符串的方法最早出现在Kernighan和Ritchie的C语言中，后来被许多其他语言借鉴和实现。
2. 除了使用Lua的内置函数外，还可以使用正则表达式、循环等其他方法来提取子字符串。
3. 在Lua中，字符串是不可变的，在提取子字符串后会生成一个新的字符串对象。

# 查看更多
- [Lua字符串函数](https://www.runoob.com/lua/lua-string.html)
- [正则表达式教程](https://www.runoob.com/regexp/regexp-tutorial.html)