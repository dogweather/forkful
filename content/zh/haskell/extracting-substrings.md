---
title:                "Haskell: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，有时候我们需要从一个字符串中提取出特定的部分来使用，这就是提取子字符串的作用。它可以帮助我们更方便地处理字符串，并且可以节省我们的代码量。

## 如何做
我们可以使用Haskell中的`take`和`drop`函数来提取子字符串。例如，我们有一个字符串`"Hello World"`，我们想要提取出其中的"World"这个子字符串，我们可以这样写代码：

```Haskell
take 5 (drop 6 "Hello World")
```

这段代码的输出将会是`"World"`。首先我们使用`drop`函数来去除前面的6个字符，然后再使用`take`函数来提取后面的5个字符，这样就得到了我们想要的子字符串。

## 深入了解
除了使用`take`和`drop`函数外，Haskell还提供了其他一些函数来帮助我们提取子字符串。比如`takeWhile`函数可以根据给定的条件提取出满足条件的字符，`takeEnd`函数可以从字符串的末尾开始提取子字符串等等。

此外，在处理字符串时，我们还可以使用正则表达式来提取特定的子字符串。Haskell中有一些库可以帮助我们使用正则表达式，比如`text`和`regex-base`。

## 参考链接
- [Haskell的字符串操作函数文档](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)
- [使用正则表达式提取子字符串的示例](https://www.schoolofhaskell.com/user/adinapoli/extracting-substrings/validation-using-regular-expressions)
- [Haskell中处理字符串的库和函数](https://hackage.haskell.org/packages/search?terms=string)