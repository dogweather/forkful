---
title:    "Elm: 提取子字符串"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么
## 为什么要提取子字符串？

提取子字符串是在处理字符串时一个非常常见且有用的操作。它可以帮助我们轻松地从给定的字符串中提取出我们想要的部分，比如手机号码、邮件地址等等。在编程过程中，我们经常需要处理大量的文本数据，通过提取子字符串，可以让我们的程序更有效地处理这些数据。

# 怎么做
## 使用Elm提取子字符串的方法

在Elm中，我们可以使用`String.slice`函数来提取子字符串。它接受三个参数：目标字符串、起始索引和结束索引。起始索引表示需要提取的子字符串的起始位置，结束索引表示子字符串的结束位置。例如，如果我们想要从字符串"Hello World"中提取"World"这个单词，我们可以这样写：
```Elm
String.slice "Hello World" 6 11
```
这将返回一个新的字符串，内容为"World"。如果我们想要提取"Hello"这个单词，我们可以这样写：
```Elm
String.slice "Hello World" 0 5
```
这将返回"Hello"。

我们也可以使用`String.left`和`String.right`函数来提取字符串的前几个或后几个字符。例如，`String.left "Hello World" 5`将返回"Hello"，而`String.right "Hello World" 5`将返回"World"。

# 深入了解
## 关于提取子字符串的更多知识

除了上面提到的函数外，Elm还提供了许多其他函数来帮助我们提取子字符串。比如，我们可以使用`String.split`来按照指定的分隔符把字符串拆分成多个子字符串。我们也可以使用`String.trim`来去除字符串的前导或尾部空格。

另外，我们还可以使用正则表达式来提取子字符串。Elm提供了`Regex`模块来处理正则表达式，我们可以使用`Regex.contains`和`Regex.find`等函数来按照指定的模式提取字符串中的部分内容。

# 参考链接
## 了解更多关于提取子字符串的内容

- [Elm官方文档 - String模块](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm官方文档 - Regex模块](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Elm语言中文文档 - 字符串操作](https://www.elmchina.org/docs/syntax/strings.html#slice)