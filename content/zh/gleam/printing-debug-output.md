---
title:                "打印调试输出"
date:                  2024-01-20T17:52:36.903405-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? / 什么以及为什么？

打印调试输出就是让程序在运行时显示信息，帮助开发者理解发生了什么。程序员这么做是为了找出代码中的错误和性能瓶颈。

## How to: / 如何操作：

```gleam
import gleam/io

pub fn main() {
  let my_data = "Gleam shines bright!"
  io.debug(my_data) // Prints debug information to the console
}
```

输出样例：

```plaintext
DEBUG: Gleam shines bright!
```

## Deep Dive / 深入探究

历史上，打印调试输出是了解程序内部运行的简便方法。它比起使用调试器来得直接，但没有调试器那么强大。Gleam的`io.debug`函数可以打印任何可以转变为字符串的值。其他语言也有类似的方法，比如Python的`print`或者JavaScript的`console.log`。在Gleam中，`debug`输出的信息前会自动添加`DEBUG:` 前缀，这有助于在日志中区分调试信息。

## See Also / 另见

- Programming languages comparison on debug techniques: [https://en.wikipedia.org/wiki/Debugging](https://en.wikipedia.org/wiki/Debugging)
- "Effective Debugging" - book by Diomidis Spinellis: [https://www.spinellis.gr/](https://www.spinellis.gr/)