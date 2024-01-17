---
title:                "写入标准错误"
html_title:           "Elixir: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

请注意：无需多余的词句，该篇文章将使用非正式的语气和简洁的写作风格，旨在向 Mandarin 读者介绍使用 Elixir 编程语言时如何向标准错误输出写入内容。

## 什么是标准错误输出与为何要使用它？
标准错误输出是指将程序中的错误信息输出到控制台的过程。程序员使用标准错误输出来追踪和调试错误，这有助于提高程序的稳定性和可靠性。

## 如何实现：
在 Elixir 编程中，我们可以使用 `IO.write(:stderr, "错误信息")` 来向标准错误输出写入内容。下面是一个简单的例子，可以看到程序的输出结果中包含了我们写入的错误信息。

```Elixir
IO.write(:stderr, "这是一个错误信息")
```

输出结果：
这是一个错误信息

## 深入了解：
历史背景：标准错误输出最初是在 Unix 操作系统中使用的，后来被许多其他编程语言和平台所采用。

替代方案：除了使用 Elixir 内置的 `IO.write(:stderr, "错误信息")` 方法，程序员也可以使用其他第三方库来实现向标准错误输出写入内容。

实现细节：在 Elixir 中，标准错误输出是通过 `:stderr` 原子值来表示的，程序会将该值传递给 `IO.write` 函数来进行输出。

## 相关资源：
更多关于 Elixir 标准错误输出的信息，请参阅官方文档：https://hexdocs.pm/elixir/IO.html#write/3

另外，如果您想了解 Elixir 提供的其他代码输出选项，请查看官方文档中的“标准 IO”部分：https://hexdocs.pm/elixir/IO.html#module-stdio