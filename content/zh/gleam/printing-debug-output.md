---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
打印调试输出是用于解决问题和优化代码的工具，它可以显示您的程序在运行过程中的详细信息，提供有助于修复错误或提升性能的数据。

## 怎么做：

在Gleam中打印调试输出是有点不同的，因为Gleam是一种静态类型语言，并且更强调不改变状态。但是您可以使用 `println` 函数，完整代码应该像这样：

```Gleam
import gleam/io.{println}

pub fn main(args: List(String)) {
   let _ = println("Hello, Gleam!")
}
```

当运行这个程序时，它将在控制台输出 "Hello, Gleam!"。

## 深入研究：

打印调试输出已经存在很长时间，其中一些早期程序员在没有先进的调试工具可用时，只能依赖它解决问题。在许多动态类型的语言中，例如JavaScript，可以直接打印任何变量。但在静态类型的语言中，如Gleam，这可能更复杂一点，因为需要知道需要打印的数据的具体类型。除了Gleam中的 `println` 函数之外，您还可以使用诸如 `log` 或 `debug` 等库来提供更多的日志和报告选项。

## 另请参阅：

如果您希望深入了解Gleam中如何打印调试输出的更多详细信息，可以参阅下面的链接：

[Gleam官方文档](https://gleam.run/book/tour/basics.html)

[Gleam中的日志库](https://hex.pm/packages/log)

这些资源可以为您提供丰富的信息并让你更好的理解如何在Gleam中实施调试输出。