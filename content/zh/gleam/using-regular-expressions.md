---
title:                "使用正则表达式"
html_title:           "Gleam: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 什么 & 为什么要用？

正则表达式是一种强大的文本匹配工具，可以帮助程序员更有效地处理和操作文本数据。程序员使用正则表达式的原因通常是为了节省时间和精力，在处理大量文本数据时能够更快速地完成任务。

## 如何：

```Gleam
import gleam/strings/regex
"example1" |> Regex.match?("regex")
// => false
"example2" |> Regex.match?("ex")
// => true
```

```Gleam
import gleam/strings/regex
let pattern = Regex.compile("*example")
let matches = pattern |> Regex.matches("some example")
// => ["some example"]
```

## 深入探讨：

正则表达式最初是在20世纪50年代由美国数学家Stephen Kleene发明的，随着计算机的发展，它变得越来越流行。除了Gleam，许多编程语言都内置了对正则表达式的支持，例如Java，Python和JavaScript。对于某些任务，正则表达式可能会比其他方法（如循环）更高效。然而，过于复杂的正则表达式可能会导致性能下降或产生错误的匹配。

## 参考链接：

- [正则表达式维基百科页面](https://zh.wikipedia.org/zh-hans/正则表达式)
- [Gleam官方文档中关于正则表达式的说明](https://gleam.run/stdlib/gleam_Std.Regex.html)
- [Java官方文档中关于使用正则表达式的教程](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)