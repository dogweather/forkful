---
title:                "Elm: 请回复：【字符串连接】"
simple_title:         "请回复：【字符串连接】"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

当我们在编写程序时，经常会需要将多个字符串连接在一起。这可以帮助我们构建出更复杂的文本，如句子、网址、文件路径等。在 Elm 中，我们可以使用 `String.concat` 函数来轻松地实现字符串的连接。

## 如何

让我们来看一个简单的例子来理解如何使用 `String.concat` 函数：

```Elm
str1 = "Hello "
str2 = "World"
result = String.concat [str1, str2]
```

在这个例子中，我们定义了两个字符串 `str1` 和 `str2`，然后使用 `String.concat` 函数来将它们连接起来。最后，我们的结果会变成 `Hello World`。

我们也可以在 `String.concat` 函数中传入多个字符串，它们会按照顺序被连接在一起。比如，我们可以这样写：

```Elm
str1 = "I love "
str2 = "Elm"
str3 = " and "
str4 = "Mandarin."
result = String.concat [str1, str2, str3, str4]
```

这样，我们的结果就是 `I love Elm and Mandarin.`。

## 深入探讨

除了简单地连接字符串外，`String.concat` 函数还可以接收一个可选的分隔符。这样，我们就可以在每个字符串之间插入指定的分隔符。比如，我们可以将上面的例子改写成这样：

```Elm
str1 = "I love "
str2 = "Elm"
str3 = " and "
str4 = "Mandarin."
result = String.concat [str1, str2, str3, str4] " "
```

这样，我们的结果就变成了 `I love Elm and Mandarin.`。其中，空格作为了分隔符。

另外值得一提的是，`String.concat` 函数只能接收字符串类型的值，如果我们想要连接其他类型的值，可以使用 `toString` 函数来将它们转换成字符串。

## 另请参阅

- [Elm官方文档](https://guide.elm-lang.org/strings/concatenation.html)
- [更多关于字符串的操作](https://www.runoob.com/elm/elm-strings.html)
- [String.concat 函数的使用示例](https://www.codewars.com/kata/557efeb04effce569d000022/train/elm)