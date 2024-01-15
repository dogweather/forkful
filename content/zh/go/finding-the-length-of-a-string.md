---
title:                "寻找字符串的长度"
html_title:           "Go: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要知道一个字符串的长度。比如，在使用字符串作为密码时，我们需要确保它的长度符合安全要求。而在Go语言中，我们可以非常简单地找到字符串的长度，让我们来看看如何做到。

## 如何操作

对于大多数编程语言来说，找到一个字符串的长度通常是一个很简单的操作，但Go语言中有一点不同。我们需要使用内置的`len()`函数来找到字符串的长度。

```Go
str := "Hello World"
len := len(str)

fmt.Println(len) // Output: 11
```

如上所示，我们首先定义一个字符串变量，并将其赋值为"Hello World"。然后，我们使用`len()`函数计算出字符串的长度，并将结果赋值给一个新的变量len。最后，我们使用`fmt.Println()`函数来打印出字符串的长度。

如果我们想要找到UTF-8编码的字符串的长度，我们可以使用`utf8.RuneCountInString()`函数。

```Go
str := "你好，世界"
len := utf8.RuneCountInString(str)

fmt.Println(len) // Output: 5
```

## 深入了解

在Go语言中，字符串实际上是由字节组成的数组，所以使用`len()`函数来找到字符串的长度其实是在找到这个数组的长度。Go语言中的字符串使用UTF-8编码，所以一个汉字会被编码为3个字节。

如果我们需要对字符串进行切片操作，可以使用`str[start:end]`语法，其中start和end分别表示切片的开始位置和结束位置。如果我们不知道字符串的长度，也可以使用`len()`函数来计算出字符串的长度，并将其作为end的值。

## 参考链接

- [Go官方文档-Strings](https://golang.org/pkg/strings)
- [Go官方文档-Unicode标准和UTF-8编码](https://golang.org/ref/spec#Rune_literals)
- [Go官方文档-Unicode包](https://golang.org/pkg/unicode/)
- [Go官方文档-UTF-8包](https://golang.org/pkg/unicode/utf8/)