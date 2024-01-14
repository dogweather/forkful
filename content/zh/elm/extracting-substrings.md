---
title:    "Elm: 提取子字符串"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 为什么

提取子字符串是一项非常有用的编程技巧，它可以帮助我们在字符串中定位和提取特定的信息，从而简化我们的编程工作。无论是处理用户输入，还是解析数据，提取子字符串都是必不可少的能力。

## 如何

提取子字符串的简单方法是使用 Elm的内建函数`slice`。它接受一个起始索引和结束索引作为参数，并返回在这两个索引之间的子字符串。

下面是一个例子，提取字符串中指定位置的字符：

```Elm
str = "Hello World!"
slice 2 3 str
```

上面的代码输出的是`"l"`，因为它提取了起始索引为2（第三个字符）和结束索引为3（第四个字符）之间的子字符串。

我们也可以使用`String.left`和`String.right`函数提取字符串的前几个或后几个字符：

```Elm
str = "Hello World!"
String.left 5 str
```

上面的代码输出的是`"Hello"`，它提取了`str`字符串的前5个字符。

## 深入探究

除了使用内建函数外，我们还可以使用`String.at`函数来提取特定位置的字符，它接受一个索引作为参数，并返回该位置的字符。

```Elm
str = "Hello World!"
String.at 6 str
```

上面的代码输出的是`"o"`，因为它提取了索引为6的字符。

我们还可以通过对字符串进行分割来提取子字符串，使用`String.split`函数可以将字符串分割成一个字符串列表。我们可以通过索引来获取列表中特定位置的子字符串。

```Elm
str = "Hello World!"
String.split " " str
```

上面的代码输出的是`["Hello", "World!"]`，它将字符串按空格分割成两个子字符串，并存储在一个列表中。我们可以使用索引来提取特定的子字符串。

## 参考资料

- [Elm官方文档](https://guide.elm-lang.org/)
- [提取子字符串的不同方法](https://www.tutorialspoint.com/how-to-extract-a-substring-in-elm-programming-language)