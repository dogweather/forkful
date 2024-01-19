---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
字符串插值是用来在字符串中嵌入表达式的结果的方法。程序员之所以使用它，是因为它能让我们更方便地组装字符串。

## 如何做？
```Clojure
(def name "Lucy")
(def greeting (str "Hello, " name "!"))
```
输出：
```Clojure
"Hello, Lucy!"
```
在上面的代码中，我们可以看到怎么在字符串中插入变量。用`str`函数来组合字符串和变量。

## 深入探讨
字符串插值的历史可以追溯到 60 年代的编程语言。Clojure 选择了 `str` 函数的方式来实现字符串插值。这是一种简单而清晰的方式。除此之外，有些语言如 Python，JavaScript 等，会使用特殊的字符串插值语法。

然而，`str` 函数并不只用于字符串插值。它可以接收任意数量的参数，将它们转化为字符串后拼接在一起。它的工作方式如下：
1. 如果参数是 `nil`，那么转化为空字符串。
2. 如果参数是一个可打印的字符，那么转化为对应的字符串。
3. 其他的情况，转化为字符串。

## 扩展阅读
你可以阅读以下链接来进一步理解字符串插值：
[Clojure str 函数的文档](https://clojuredocs.org/clojure.core/str)
[Clojure 字符串和字符的文档](https://clojure.org/guides/weird_characters)
[字符串插值的 Wikipedia 词条](https://en.wikipedia.org/wiki/String_interpolation)