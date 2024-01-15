---
title:                "寻找字符串的长度"
html_title:           "Clojure: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要知道一个字符串的长度，例如在处理用户输入或者验证输入内容时。Clojure提供了简便的方法来获取字符串的长度，接下来我们将会介绍如何操作。

## 如何操作

在Clojure中，我们可以使用 `count` 函数来获取字符串的长度，该函数接受一个字符串作为参数，并返回字符串的长度。

```
Clojure
(count "Hello World") ;; 输出 11
```

## 深入了解

`count` 函数并不仅仅可以用于字符串，它也可以用于一个序列或者集合。如果传入一个序列，`count` 函数会返回序列的元素个数。如果传入一个集合，`count `函数会返回集合中的元素个数。

除了 `count` 函数，我们还可以使用 `(.length s)` 方法来获取字符串的长度，它会返回一个整数。 注意，`count` 函数和 `(.length s)` 方法都会返回字符串中的字符数，而不是字节数。

## 查看更多

可以参考Clojure官方文档 [字符串函数](https://clojuredocs.org/clojure.string/count) 来了解更多关于 `count` 函数的使用方法。

# 也可以查看这些链接：
- [Clojure官方文档](https://clojure.org/)
- [Clojure教程](https://www.tutorialspoint.com/clojure/)
- [Clojure中文网](https://clojure-cn.readthedocs.io/zh_CN/latest/index.html)