---
title:    "Clojure: 字符串连接"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

在编程中，字符串是一种非常常见的数据类型。它们由各种字符组成，可以表示文本或其他数据。但有时，我们需要将多个字符串拼接在一起，形成一个更长的字符串。这种操作称为“字符串连接”，它在很多编程语言中都是重要的基础知识。在Clojure中，拼接字符串也是一项常见的操作，让我们来看看为什么要这样做。

## 怎样做

在Clojure中，字符串连接操作可以通过`str`函数来实现。我们来看一个简单的例子，将两个字符串“Hello”和“World”拼接在一起，代码如下：

```Clojure 
(str "Hello" "World")
```

运行以上代码，我们会得到以下输出：

```
"HelloWorld"
```

正如你所见，`str`函数会将所有传入的参数拼接在一起，并返回一个新的字符串。如果我们传入更多的字符串，它们也会被按顺序拼接在一起。比如，我们传入三个字符串“Welcome”，“to”，“Clojure”，会得到以下输出：

```
"WelcometoClojure"
```

另外，`str`函数也可以接受除了字符串之外的其他数据类型作为参数。在这种情况下，它会自动将这些数据转换为字符串，然后拼接在一起。比如，我们传入一个整数和一个布尔值作为参数，会得到以下输出：

```
"42true"
```

## 深入了解

在Clojure中，`str`函数其实是一个宏，它会将所有传入的参数转换为字符串后，再调用`StringBuilder`来进行拼接。这也就是为什么在之前的例子中，我们拼接的字符串不会自动加上空格的原因。

除了`str`函数，Clojure还提供了另外两个函数来做字符串的拼接：`join`和`str-join`。其中，`join`函数可以在每个参数之间加上指定的分隔符，比如空格、逗号等。而`str-join`函数则可以在每个参数之间加上指定的前缀和后缀，比如圆括号、引号等。你可以自行尝试这两个函数来探索它们的用法。

## 参考链接

- [Clojure官方文档 - 字符串操作](https://clojure.org/reference/data_structures#Strings)
- [Clojure Cheatsheet - 字符串操作](https://clojure.org/api/cheatsheet#Strings)
- [狂神说Clojure - 字符串和集合操作](https://www.bilibili.com/video/BV1sK4y1B7y6?from=search&seid=922083896067130936)

## 请参考

- [学习Clojure的起步指南](https://zhuanlan.zhihu.com/p/111619005)
- [Clojure学习资源整理](https://zhuanlan.zhihu.com/p/110529895)