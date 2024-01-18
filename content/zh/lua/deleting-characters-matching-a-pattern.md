---
title:                "匹配模式的删除字符"
html_title:           "Lua: 匹配模式的删除字符"
simple_title:         "匹配模式的删除字符"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么&为什么？ 
Lua是一种编程语言，它允许程序员通过匹配特定模式来删除字符串中的字符。程序员可能会这样做是为了简化文本处理，或者根据特定的需求来删除不需要的字符。 

## 如何： 
```Lua 
-- 删除字符串中所有大写字母 
str = "Hello World!" 
str = str:gsub("%u", "") 
print(str) 
``` 
输出：oorld! 

```Lua 
-- 删除字符串中的所有数字 
str = "abc123xyz" 
str = str:gsub("%d", "") 
print(str) 
``` 
输出：abcxyz

## 深入探讨： 
1. 历史背景：最早实现删除字符串中字符匹配功能的是Perl编程语言，随后许多其他语言也加入了这一功能。
2. 其他选择：除了使用gsub函数，也可以使用“find and replace”方法来实现删除字符匹配的功能。
3. 实现细节：gsub函数接受两个参数，第一个是要匹配的模式，第二个是替换的字符串。可以根据具体需要来设置模式，例如，"%u"代表大写字母，"%d"代表数字。 

## 另请参阅： 
- Lua官方文档：https://www.lua.org/docs.html
- Perl语言官方网站：https://www.perl.org/
- "Find and Replace"方法参考教程：https://www.w3schools.com/python/ref_string_replace.asp