---
title:                "连接字符串"
html_title:           "Clojure: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

##为什么
您可能想知道为什么要学习如何连接字符串。简单来说，连接字符串是编程中常用的操作，它可以让您动态地将不同的字符串合并起来，创建出更复杂的字符串。比如您可以用它来构建一个完整的句子，或者将多个单词连在一起来创建一个单词游戏。

##如何进行
首先，让我们来看看如何在Clojure中连接字符串：

```Clojure
(str "Hello" " " "world")
```

这段代码将输出 "Hello world"。通过使用str函数，我们可以将多个字符串合并成一个，并在它们之间添加任意的分隔符。

如果您想连接字符串和其他类型的值，可以使用clojure.string/join函数，它可以接受任意数量的参数，并将它们连接成一个字符串。例如：

```Clojure
(require '[clojure.string :as string])

(string/join " " "Hello" "world" "!")
```

这段代码将输出 "Hello world !"。除了空格，您还可以在join函数中指定任意的分隔符。

##深入探讨
连接字符串的底层原理其实并不复杂，Clojure中的字符串其实就是Java中的字符串，而连接字符串的方式也和Java是一样的。因此，您也可以使用Java中的方法来进行字符串连接。例如：

```Clojure
(.concat "Hello" " world")
```

除了特定的字符串连接函数，Clojure也提供了一些更加灵活的方法来操作字符串。比如，在连接字符串之前，我们经常需要操作字符串的形式，比如去除空格、转换大小写等。Clojure提供了一系列函数来进行这些操作，比如：

```Clojure
(clojure.string/trim "  Hello world ")
```

这段代码将输出 "Hello world"，因为我们去除了字符串两边的空格。

除了这些基本的操作，Clojure还提供了许多其他方法来处理字符串，包括正则表达式、字符串替换等。如果您想进一步学习如何操作字符串，可以参考下面的链接。

##另请参阅
- [Clojure String函数文档](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure正则表达式文档](https://clojuredocs.org/clojure.core/re-pattern)