---
title:                "将字符串转为大写"
html_title:           "Lua: 将字符串转为大写"
simple_title:         "将字符串转为大写"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

What & Why?:
在程序设计中，字符串大小写转换是指将字符串中的所有字符改变为大写或小写。程序员在编写代码时经常需要进行这样的转换，因为它可以给用户提供更好的可读性以及更方便的操作特定字符串的能力。

How to:
```Lua
-- 要将字符串变为大写，可以使用string.upper()函数。
print(string.upper("Hello World!"))  -- 输出: HELLO WORLD!

-- 要将字符串变为小写，可以使用string.lower()函数。
print(string.lower("Hello World!"))  -- 输出: hello world!
```

Deep Dive:
在早期编程语言中，字符串大小写转换通常需要使用额外的库或函数来实现。但是在Lua中，这些函数已经内置在string库中，因此编程变得更加方便快捷。

除了直接使用内置函数外，也可以通过使用循环来实现大小写转换。比如，可以用字符串的ASCII码来判断字符是否为大写或小写，并将其相应地改变。但是这种方法相比直接使用内置函数来说会更繁琐和低效。

另外，需要注意的是，在Lua中，字符串的大小写转换只针对英文字母有效，对于其它语言中的特殊字符或标点符号可能没有作用。

See Also:
- [Lua官方文档中关于string库的介绍](https://www.lua.org/manual/5.3/manual.html#6.4)
- [字符串大小写转换的更多实现方法](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [学习Lua编程的推荐书籍](https://www.amazon.com/dp/1935182849)