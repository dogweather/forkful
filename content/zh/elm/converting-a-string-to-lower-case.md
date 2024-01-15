---
title:                "将字符串转换为小写"
html_title:           "Elm: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写

字符串是编程中常见的数据类型，在某些情况下我们需要将字符串转换为小写字母。这可能是为了方便比较字符串，或者为了保持字符串的统一性。无论出于什么原因，将字符串转换为小写都是一项非常实用的技能。

## 如何实现字符串转换为小写

首先，我们需要使用 Elm 编程语言。然后，按照以下步骤实现字符串转换为小写。

第一步，将要转换的字符串赋值给一个变量，比如 ```str```
```
str = "STRING IN CAPITAL LETTERS"
```

第二步，使用 Elm 的内置函数 ```String.toLower``` 将字符串转换为小写。
```
lowerStr = String.toLower str
```

第三步，使用 ```Debug.log``` 函数来打印转换后的结果，方便我们查看输出。
```
lowerStr = String.toLower str
  |> Debug.log "Lowercase string"
```

输出结果为：
```
Lowercase string: string in capital letters
```

## 深入了解字符串转换为小写

在 Elm 中，字符串是不可变的数据类型。这意味着一旦创建了一个字符串，它就不会发生改变。所以当我们调用 ```String.toLower``` 函数时，实际上它会返回一个新的字符串，而不是直接修改原字符串。这是 Elm 语言的一大特点，它可以避免一些潜在的 bug。

另外，与其他编程语言不同的是，Elm 中的字符串默认是 Unicode 字符集，因此在进行大小写转换时，它会考虑到 Unicode 字符的特殊情况。

# 查看更多

想要了解更多关于 Elm 的内容，可以参考下面的链接：

- [官方网站](https://elm-lang.org/)：进入官方网站可以找到 Elm 的详细文档和教程。
- [Elm 中文网](http://www.elmchina.org/)：一个致力于推广 Elm 语言的中文网站，有大量的中文资料和社区讨论。
- [Elm 中文文档](https://github.com/elm-tools/elm-doc-zh)：一个由社区翻译的 Elm 中文文档，可以帮助你更方便地学习 Elm。

欢迎大家多多交流和探讨，让我们共同学习 Elm 语言！