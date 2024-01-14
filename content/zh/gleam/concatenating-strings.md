---
title:                "Gleam: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
为什么会有拼接字符串的需求？拼接字符串是一种常见的编程技术，它可以将多个短字符串连接成一个长字符串，并且在某些情况下可以提高代码的可读性和可维护性。

## 如何操作
在Gleam中，我们可以使用 `<>` 符号来拼接字符串。以下是一个简单的例子：

```Gleam
let name = "小明"
IO.print("你好" <> name)
```

输出结果为： 你好小明

我们也可以使用 `String.concat()` 函数来拼接多个字符串。例如：

```Gleam
let numbers = [1, 2, 3]
IO.print("这些数字加起来是" <> String.concat(numbers))
```

输出结果为： 这些数字加起来是123

如果需要在拼接的过程中添加一些分隔符，可以使用 `String.join()` 函数。例如：

```Gleam
let fruits = ["苹果", "香蕉", "橘子"]
IO.print("今天吃了" <> String.join(", ", fruits))
```

输出结果为： 今天吃了苹果, 香蕉, 橘子

## 深入了解
拼接字符串的过程实际上是将多个字符串在内存中连接起来，因此在处理大量字符串时，要注意内存的消耗。另外，Gleam还提供了 `String.concat_list()` 函数来更高效地拼接字符串列表。

另外，拼接字符串也可以用来构建动态的SQL语句，在Gleam中可以使用 `String.concat()` 函数来拼接变量和固定的SQL语句，从而动态生成SQL语句。

## 请参阅
- [Gleam字符串文档](https://gleam.run/documentation/stdlib/string/)
- [使用Gleam构建Web应用程序教程](https://gleam.run/tutorials/web-development/using-gleam-to-build-web-applications/)