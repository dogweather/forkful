---
title:                "字符串的插值"
html_title:           "Gleam: 字符串的插值"
simple_title:         "字符串的插值"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串内插是一种将变量值插入到字符串中的方式。程序员们经常使用它来减少重复，并使代码更易读。

## 如何：

```Gleam
let name = "John"
let greeting = "Hello, {name}"
IO.print(greeting)
```

输出：

```
Hello, John
```

## 深入了解：

**历史背景：** 字符串内插最早出现在像Ruby和Python这样的动态语言中。它们的设计灵感来自于Shell和Perl脚本语言。

**其他选择：** 除了字符串内插，程序员还可以使用字符串拼接（使用`++`操作符）或格式化字符串（使用`format!`宏）来实现相同的目的。然而，字符串内插通常被认为是最简洁和最易读的方法。

**实现细节：** 在编译时，字符串内插会将使用`{}`包围的变量替换为其对应的值，并将其与静态字符串连接。这种方式可以减少运行时的开销，并提高程序的性能。

## 另请参阅：

- [Gleam官方文档](https://gleam.run/)
- [Github上Gleam的源代码](https://github.com/gleam-lang/gleam)