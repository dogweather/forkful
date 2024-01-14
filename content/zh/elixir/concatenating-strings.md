---
title:                "Elixir: 拼接字符串"
simple_title:         "拼接字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要将多个字符串连接起来，构造出新的字符串。这种操作叫做字符串拼接，它能够让我们更灵活地处理文本数据，是编程中常用的基础技术之一。

## 如何做

要在Elixir中进行字符串拼接，我们可以使用`<>`运算符。下面是一个简单的例子：

```Elixir
str1 = "Hello"
str2 = "World"
str1 <> str2   #返回"HelloWorld"
```

我们还可以在字符串中插入变量，使用`#{}`的形式。比如：

```Elixir
name = "Tom"
"Hello, #{name}!"   #返回"Hello, Tom!"
```

除了`<>`运算符，Elixir还提供了一个`String.concat/2`函数来进行字符串拼接。这个函数接受一个字符串列表作为参数，将它们依次连接起来生成一个新的字符串。例如：

```Elixir
String.concat(["Hello", " ", "World"])  #返回"Hello World"
```

如果我们需要在字符串中重复多次同一个字符，可以使用`String.duplicate/2`函数。它接受一个字符和重复次数作为参数，生成包含重复字符的新字符串。比如：

```Elixir
String.duplicate("-", 10)  #返回"----------"
```

除了上述方法，我们还可以使用`String.to_string/1`函数将其他类型的值转换为字符串，然后进行拼接。例如：

```Elixir
num = 123
"Number: " <> String.to_string(num)   #返回"Number: 123"
```

## 深入探讨

在Elixir中，字符串实际上是Unicode字符序列。因此，在拼接字符串时，需要注意保持正确的字符编码，避免出现乱码。此外，Elixir还提供了一些内置的字符串处理函数，比如`String.length/1`用于获取字符串的长度，`String.reverse/1`用于翻转字符串等。

另外值得一提的是，Elixir中的字符串是不可变的，即一旦创建，就不能被修改。每次进行字符串拼接时，都会产生一个新的字符串，并不会改变原有的字符串。这样的设计有利于提高程序的稳定性和性能。

## 看看更多

想要了解更多关于Elixir中字符串处理的知识，可以参考下面的资源：

- [Elixir字符串操作文档](https://hexdocs.pm/elixir/String.html)
- [Elixir字符串拼接方式探讨](https://hackernoon.com/string-concatenation-in-elixir-33521d9e4c2e)
- [学习Elixir的最佳实践](https://programminghistorian.org/lessons/best-practices-for-programming-in-elixir) 

感谢阅读！祝你在Elixir世界里玩得开心！