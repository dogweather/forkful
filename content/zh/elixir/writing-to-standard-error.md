---
title:                "写入标准错误"
date:                  2024-01-19
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"

category:             "Elixir"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
写入标准错误(stderr)用于输出错误信息。程序员这样做可以把程序的错误信息和正常输出分开，方便调试和日志记录。

## How to: (如何操作：)
在Elixir里，使用 `IO` 模块中的 `IO.puts/2` 和 `IO.warn/2` 函数，可以把信息直接写入标准错误：

```elixir
# 写入标准错误示例
IO.puts(:stderr, "这是一个错误信息。")

# 使用 IO.warn，更符合警告信息的场合
IO.warn("这是一个警告。")
```

运行后的输出（假设没有正常输出）:
```
这是一个错误信息。
这是一个警告。
```

## Deep Dive (深入探索)
早期计算机使用stderr作为终端输出错误信息，便于分离标准输出(stdout)和错误输出，后来成为多种编程语言的标准。
在Elixir中，`IO.puts/2` 和 `IO.warn/2` 是写标准错误的主要方法。但你也可以用底层功能，比如 :erlang's `:io.format/2`。
实现上，Elixir通过Erlang的BEAM虚拟机与操作系统交互，管理输出流。

## See Also (延伸阅读)
- Elixir `IO` 模块官方文档: [https://hexdocs.pm/elixir/IO.html](https://hexdocs.pm/elixir/IO.html)
- Erlang `:io` 模块官方文档: [http://erlang.org/doc/man/io.html](http://erlang.org/doc/man/io.html)
- 了解Unix标准流的历史：[https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
