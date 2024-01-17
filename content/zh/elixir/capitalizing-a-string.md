---
title:                "字符串大写化"
html_title:           "Elixir: 字符串大写化"
simple_title:         "字符串大写化"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串大写是指将字符串中的所有字符都转换为大写形式。程序员这样做的原因是为了方便比较和匹配字符串，因为大写和小写字母是不同的。

## 如何：
```Elixir
String.upcase("hello world") 
IO.puts("HELLO WORLD")
```

结果:

```Elixir
"HELLO WORLD"
HELLO WORLD
```

## 深入了解：
字符串大写的概念在计算机编程中已经存在了很长时间。在早期计算机中，由于存储空间有限，只能使用大写字母来表示字符。但随着计算机技术的发展，我们现在可以使用小写字母来表示字符。不过，许多编程语言仍然支持字符串大写操作，因为它仍然是一种有效的方法来处理字符串。

## 参考：
了解更多关于字符串大写的知识：https://yoongkang.com/blog/string-case-conversions-in-elixir/