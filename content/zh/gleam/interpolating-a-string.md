---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
插值字符串是将变量或表达式的值插入字符串中的过程。程序员之所以执行此操作，主要是为了更方便地创建和修改复杂字符串。

## 如何执行：
我们在Gleam中看一个插值字符串的例子。假设我们定义了一个变量`name`，我们想在一个问候语句中使用它。

```Gleam
let name = "world"

let sentence = "Hello, " ++ name
```

输出将是：

```Gleam
"Hello, world"
```

## 深度解析
虽然字符串插值听起来像是现代编程概念，但它实际上追溯到了20世纪60年代，那时在Unix Shell脚本和C语言中就已经出现了。在过去，很多语言使用复杂的格式化功能来拼接字符串，这往往比使用字符串插值要复杂。

在Gleam中，我们通过`++`操作符完成字符串插值。它与许多其他语言（如Python的f-string或JavaScript的模板字面量）的字符串插值不同，Gleam采用的方式更接近于古老的拼接。

运行时，Gleam会先计算`++`两边的值，然后创建一个新的字符串，将两者进行连接。

## 另请参见
要了解Gleam中的字符串和字符串插值的更多信息，请查阅以下资源：

[Gleam官方文档](https://gleam.run/book/tour/strings.html)

[Gleam GitHub](https://github.com/gleam-lang/gleam)