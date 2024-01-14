---
title:    "Gleam: 连接字符串"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 为什么嵌入变量？

当你需要在程序中动态地构建字符串时，嵌入变量就非常有用。它可以帮助你简化代码，节省时间，并提高代码的可读性。

## 如何使用？

在Gleam中，字符串拼接使用`<>`操作符来实现。让我们以一个简单的例子来说明它的用法：

```gleam
let name = "小明"
let greeting = "你好，" <> name <> "！"
io.println(greeting)
```

以上代码将输出：“你好，小明！”。你可以在任何地方使用`<>`来连接字符串和变量。记住，两边都需要有空格。

## 深入了解

在Gleam中，字符串是不可变的，这意味着每次拼接字符串时都会创建一个新的字符串。如果你需要频繁地修改字符串，则建议使用列表来处理字符串，然后再通过`String.join`函数来拼接它们。

此外，如果你需要拼接大量的字符串，最好使用`String.Builder`来构建字符串。它的效率更高，可以减少内存占用。

# 参考链接
- Gleam官方文档：https://gleam.run/book/
- 字符串操作：https://gleam.run/books/stdlib/strings.html
- String模块：https://gleam.run/books/stdlib/String.html
- String.Builder模块：https://gleam.run/books/stdlib/String.Builder.html

# 参见
- "为什么使用字符串插值？"
- "Gleam中的其它字符串操作技巧"