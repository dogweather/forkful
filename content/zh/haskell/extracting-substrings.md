---
title:                "提取子串"
html_title:           "Haskell: 提取子串"
simple_title:         "提取子串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是子字符串，为什么程序员会用到它？

子字符串指的是从一个字符串中提取出一部分字符组成新的字符串。程序员经常使用子字符串来处理文本，比如搜索和替换特定的字词。这种技术也可以用来解析复杂的文本数据。

## 如何提取子字符串：

使用Haskell中的```take```和```drop```函数可以方便地提取子字符串。比如，如果我们有一个字符串"Hello world!"，我们可以使用```take```和```drop```函数来提取出前5个字符"Hello"和后6个字符"world!"。代码示例如下：

```Haskell
let str = "Hello world!"
take 5 str                 -- Output: "Hello"
drop 6 str                 -- Output: "world!"
```

## 深入探讨

提取子字符串的概念并不新鲜，在Haskell的前身Miranda语言中就有相关的函数。除了使用```take```和```drop```函数，还可以使用```substr```函数来提取子字符串，它接受一个起始位置和提取的长度作为参数。然而，```take```和```drop```函数更加灵活，可以通过结合使用来实现更复杂的提取操作。

## 查看更多信息

了解更多有关提取子字符串的信息，请参考以下资源：

- [Haskell字符串文档](https://www.haskell.org/onlinereport/standard-prelude.html#g:21)
- [Haskell字符串函数教程](https://wiki.haskell.org/String_functions)
- [Haskell字符串模式匹配教程](https://wiki.haskell.org/Pattern_matching)