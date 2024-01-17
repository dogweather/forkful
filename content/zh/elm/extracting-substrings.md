---
title:                "提取子字符串"
html_title:           "Elm: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

提取子字符串是指从一个字符串中获取一部分内容。程序员通常会这么做，是为了从一个较长的字符串中提取出特定的信息，方便处理和操作。

## 如何进行：

```Elm
substring : Int -> Int -> String -> String
substring 2 4 "Hello world" -- 输出 "ll"
substring 3 6 "Elm is awesome!" -- 输出 "m i"
```

## 深入探讨：

提取子字符串的概念可以追溯到早期的计算机编程语言，如BASIC和C语言。除了使用内置函数外，还有其他方法可以进行子字符串提取，如使用正则表达式或手动遍历字符串。在Elm中，提取子字符串使用的是内置函数，效率高且简单易懂。

## 参考资料：

- [Elm官方文档](https://guide.elm-lang.org/effect_managers/0.18/String.html#substring)
- [字符串操作文档](https://guide.elm-lang.org/effects/managing-effects.html#string-substr)
- [正则表达式教程](https://www.zybuluo.com/jfpidong/note/78497)