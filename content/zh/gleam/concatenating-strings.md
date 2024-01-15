---
title:                "连接字符串"
html_title:           "Gleam: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，合并字符串是一项非常常见的任务。通过将多个字符串连接在一起，我们可以创建更复杂的文本内容。比如，我们可以将名字和地址合并起来创建一个完整的个人信息，或是将多个文本段落连接在一起形成一篇长文。在Gleam中，合并字符串非常简单，下面就来看看如何做到。

## 如何操作

要在Gleam中合并字符串，你需要使用操作符 `++`。这个操作符可以将两个字符串合并在一起，形成一个新的字符串。

```Gleam
let name = "张三"
let address = "北京市东城区"
let info = name ++ "的住址是" ++ address
```
输出：张三的住址是北京市东城区

在以上例子中，我们将名字和地址合并在一起，创建了一个新的信息。除了字符串，我们也可以使用变量和数字来进行合并，只要使用 `++` 操作符就可以了。

## 深入探讨

在Gleam中，字符串实际上是一个字符列表的集合。当我们使用 `++` 操作符时，其实是在对字符列表进行连接操作，最终得到一个新的字符列表。因此，我们可以使用其他字符列表相关的操作符来处理字符串，比如 `|>` 操作符来转换字符列表，在合并字符串时也可以使用。

## 参考链接

- [Gleam官方文档](https://gleam.run/documentation)
- [学习Gleam代码样例](https://github.com/gleam-lang/gleam/tree/master/examples)
- [Gleam编程社区论坛](https://forum.gleam.run/)