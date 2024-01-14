---
title:                "Elixir: 向标准错误输出（Computational Error）的文章"
simple_title:         "向标准错误输出（Computational Error）的文章"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

为什么要将错误信息写入标准错误流中呢？毕竟，我们可以将错误信息直接打印出来。但事实上，将错误信息写入标准错误流中能够更有效地捕捉异常，并帮助我们更好地调试代码。因此，在编写Elixir代码时，写入标准错误流是一个非常有用的技巧。

## 如何

要将错误信息写入标准错误流中，我们可以使用Elixir的 `IO.write/2` 函数。该函数接受两个参数，第一个参数为要写入的文本，第二个参数为流。我们可以使用 `:stderr` 来表示标准错误流。下面是一个示例代码：

```Elixir
IO.write("This is an error message", :stderr)
```
运行上述代码后，我们就可以在控制台看到标准错误流中打印出了错误信息。

## 深入探讨

除了使用 `IO.write/2` 函数，我们还可以使用 Elixir 的 `Kernel.raise/3` 函数来将错误信息写入标准错误流。该函数会将参数中的异常信息打印到标准错误流中，并将其作为实际异常抛出。下面是一个示例代码：

```Elixir
defmodule MyModule do
  def raise_error do
    raise "This is an error message"
  end
end

MyModule.raise_error()
```

运行上述代码后，我们会在控制台看到如下输出：

```
** (RuntimeError) This is an error message
    (elixir 1.11.3) lib/exception.ex:106: Exception.raise/3
    (elixir 1.11.3) lib/kernel/parallel_compiler.ex:345: Kernel.ParallelCompiler.spawn_compilers/2
    (elixir 1.11.3) lib/kernel/parallel_compiler.ex:28: Kernel.ParallelCompiler.start/1
```

可以看到，错误信息被成功写入标准错误流中。

## 参考链接

- [Elixir文档-IO](https://hexdocs.pm/elixir/IO.html)
- [Elixir文档-Kernel](https://hexdocs.pm/elixir/Kernel.html)
- [Elixir错误处理](https://elixir-lang.org/getting-started/error-handling.html)