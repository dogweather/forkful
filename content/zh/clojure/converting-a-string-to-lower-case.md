---
title:    "Clojure: 将字符串转换为小写"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候需要将字符串转换为小写字母。这可以帮助我们比较字符串或将其格式化为标准大小写。Clojure提供了一种简单的方法来实现这一功能，让我们来学习如何使用它！

## 如何操作

首先，我们需要定义一个字符串变量，这可以通过使用Clojure的`def`关键字来完成。然后，我们可以使用`str/lower-case`函数将字符串转换为小写字母：

```Clojure
(def str "HELLO WORLD")
(str/lower-case str)
```

这将返回一个新的字符串`"hello world"`。我们也可以将这一过程简化为一行代码：

```Clojure
(str/lower-case "HELLO WORLD")
```

## 深入了解

`str/lower-case`函数实际上是Clojure语言的一个内置函数，它位于`clojure.string`命名空间中。它采用一个字符串作为参数，并返回小写字母的新字符串。这个函数可以处理不同的编码，例如Unicode字符串。如果你想要了解更多关于字符串转换为小写字母的更多技巧和技术，请阅读Clojure的官方文档。

## 参考文献

- [Clojure官方文档](https://clojuredocs.org/clojure.string/lower-case)
- [Clojure中文文档](https://www.gitbook.com/book/wizardforcel/clojure-by-example/details)
- [使用Clojure处理字符串](https://medium.com/@dennisideler/clojure-working-with-strings-11fc5c6e9e07)

## 参见

- [Koans.cn](https://www.koans.cn/markdown)
- [Clojure入门指南](https://clojure.org/guides/getting_started)