---
title:                "开始一个新项目"
html_title:           "Gleam: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么是新项目？为什么程序员要启动

启动一个新项目意味着开始一个新的软件开发项目。程序员们经常这样做，因为他们需要构建新的软件，或者对现有的软件进行改进。

## 如何启动

```Gleam
import io
import gleam/string

fn main() {
  let output = string.repeat("Gleam", 3)
  io.println(output)
}

// 输出：GleamGleamGleam
```

可以使用 `gleam/string` 模块中的 `string.repeat` 函数来重复打印字符串。这样可以使输出更有趣。

## 深入探讨

Gleam 是一种现代的函数式编程语言，它的设计初衷是提高代码的可读性和可维护性。它受到了许多其他编程语言的影响，比如 Erlang、JavaScript 和 Haskell。除了 Gleam，还有其他的一些编程语言也可以启动新项目，比如 Rust、Go 和 Elixir。

## 参考链接

- 官方文档：https://gleam.run/
- Rust 官方文档：https://www.rust-lang.org/
- Go 官方文档：https://golang.org/
- Elixir 官方文档：https://elixir-lang.org/