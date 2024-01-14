---
title:                "Clojure: 匹配模式的字符删除"
simple_title:         "匹配模式的字符删除"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么

Clojure是一种功能强大的函数式编程语言，它允许开发人员更快地编写可靠和高效的代码。删除与模式匹配的字符可以帮助你更好地处理数据，提高代码的可读性和可维护性。在本文中，我们将介绍如何在Clojure中删除与模式匹配的字符，并深入探讨其中的原理。

## 如何做

Clojure提供了许多内置的函数，可以帮助我们删除与模式匹配的字符。其中最常用的是`clojure.string/replace`函数。让我们来看看一个简单的例子：

```Clojure
(clojure.string/replace "Hello World!" #"o" "")
```

这段代码将会把字符串中所有的`o`字符替换为空字符串，最后输出`Hell Wrld!`。你也可以使用`clojure.string/replace-first`函数来只替换第一个匹配的字符。

如果你想删除多个字符，可以使用正则表达式来匹配这些字符，然后用空字符串来替换它们。举个例子，下面的代码将会删除字符串中的所有数字：

```Clojure
(clojure.string/replace "I have 2 apples and 3 oranges" #"\d" "")
```

最后的输出将会是`I have  apples and  oranges`。

除了`clojure.string`命名空间中的函数，你也可以使用`clojure.core`命名空间中的`replace`和`replace-first`函数来完成相同的任务。不过要注意，使用`replace`函数时，要将要替换的字符放在第一个参数的位置，要替换的内容放在第二个参数的位置。

```Clojure
(Replace "Hello World!" "o" "")
```

## 深入探讨

在Clojure中，字符串是不可变的，因此任何字符串的修改都会返回一个新的字符串。当我们使用`clojure.string/replace`函数时，实际上是在创建一个新的字符串对象来替换原来的字符串。这就是为什么，尽管我们没有指定要接收函数返回值的变量，原始的字符串仍然不会被修改的原因。

另外值得注意的是，`clojure.string/replace`函数还接受一个可选的第三个参数，用来指定要替换的次数。如果不指定，默认会替换所有匹配的字符。

Clojure中还有其他一些函数可以帮助我们处理字符串，如`clojure.string/split`和`clojure.string/join`。它们也可以结合使用来删除匹配的字符，但使用正则表达式的方法更为通用。

## 参考链接

- [Clojure官方文档](https://clojure.org/guides/string_api)
- [正则表达式入门教程](https://www.runoob.com/regexp/regexp-tutorial.html)
- [Clojure Cookbook: Manipulating strings](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02-data/2-01-manipulating-strings.asc)