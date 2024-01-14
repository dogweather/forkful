---
title:    "Gleam: 大写一个字符串"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

首先，欢迎来到这篇Gleam编程博客！今天我们要介绍一个简单但又非常有用的编程技巧——如何将字符串中的字母大写。为什么要这样做呢？有时候我们需要统一格式化用户输入的数据，或者给一些提示信息增加一点视觉效果，使其更加易读。那么，让我们来看看如何使用Gleam来实现这个功能吧！

## 如何实现

首先，让我们定义一个名为"str"的字符串变量，里面包含了几个单词。现在，我们想要将这个字符串中的所有字母都变成大写，这个非常简单！只需要使用Gleam的内置函数capitalize即可。

```Gleam
let str = "hello world"
str = capitalize(str)
```

如此简单，我们已经成功将"hello world"变为"HELLO WORLD"。另外，如果我们只想要将字符串中的第一个单词首字母大写，可以使用函数title_case。

```Gleam
let str = "hello world"
str = title_case(str)
```

此时，输出将变为"Hello World"。这些内置的Gleam函数让我们在字符串操作上更加灵活和高效。

## 深入了解

除了上面提到的两个函数，Gleam还有其他的字符串操作函数，比如concatenation（连接）、split（拆分）、substr（截取）等等。这些函数都可以在官方文档中找到详细的说明。除此之外，在Gleam社区中也有许多开源的字符串库，可以在需要更复杂操作时使用。总之，学习并掌握这些字符串操作的技巧将使你的Gleam编程更加高效和精确。

## 参考链接

- 官方文档：https://gleam.run/stdlib/gleam/string.html
- Gleam社区：https://github.com/gleam-lang/gleam/lib/string