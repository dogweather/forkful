---
title:                "Gleam: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么：为什么要将字符串首字母大写？

在编程中，我们经常需要对字符串进行操作，而首字母大写是一种常见的操作。它可以让字符串更加美观和易读，也可以用于格式化输出数据。接下来，我们将介绍如何在Gleam中实现字符串首字母大写的方法。

## 如何操作：示例代码和输出结果

首先，我们需要定义一个字符串变量，并赋值为需要进行首字母大写的字符串。

```Gleam
let name = "gleam programming"
```

接下来，我们使用Gleam的 `String.capitalize` 函数来实现首字母大写。

```Gleam
let capitalized_name = String.capitalize(name)
```

现在，我们可以输出 `capitalized_name` 变量的值，查看结果。

```Gleam
IO.print(capitalized_name)
```

输出结果为：

```
Gleam programming
```

## 深入了解：关于字符串首字母大写的更多信息

在Gleam中，`String.capitalize` 函数实现了将字符串第一个字符转换为大写，并保持其他字符不变的功能。此外，Gleam还提供了 `String.uppercase_first` 和 `String.uppercase` 函数来实现将字符串所有字符转换为大写。

另外，如果想要实现字符串中每个单词的首字母大写，可以使用Gleam的 `String.map` 函数来遍历并操作每个单词。

## 参考链接

- [Gleam官方网站](https://gleam.run/)
- [Gleam文档](https://gleam.run/documentation/)
- [关于字符串的更多操作方法](https://gleam.run/documentation/stdlib/string.html)