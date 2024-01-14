---
title:                "Gleam: 字符串的大写转换"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要对字符串进行大写处理。这可能是为了格式化输出或者进行字符串比较。不管是什么原因，使用Gleam编程语言可以轻松地实现字符串大写功能。

## 如何操作

使用Gleam编程语言，您可以通过内置的`String.to_uppercase`函数来将字符串大写。下面是一个简单的示例：

```Gleam
let my_string = "hello world"
let uppercase_string = String.to_uppercase(my_string)

// 输出：HELLO WORLD
```

如果您想要更多的控制，可以使用`String.fold`函数来自定义大写规则。下面是一个将每个字母转换为大写的示例：

```Gleam
let my_string = "hello world"
let uppercase_string = String.fold(my_string, fn(c) {
  Char.to_uppercase(c)
})

// 输出：HELLO WORLD
```

## 深入了解

在Gleam中，字符串是不可变的数据类型。这意味着无法直接对字符串进行修改，而是需要创建一个新的字符串。因此，在`String.to_uppercase`函数中，实际上是创建了一个全新的字符串对象，并将原始字符串中的所有字符按照大写规则进行转换。同时，使用`String.fold`函数可以更加灵活地控制大写的转换过程。

## 参考资料

- [Gleam官方文档](https://gleam.run)
- [Gleam源代码仓库](https://github.com/gleam-lang/gleam)
- [Gleam社区论坛](https://discord.gg/gleam-lang)

## 参见

- [Gleam编程指南](https://www.gleam.run/book/getting-started.html)
- [使用Gleam实现基础数据结构](https://devblog.featherweight.dev/posts/data-structures-with-gleam/)