---
title:                "Elixir: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么要打印调试输出？

打印调试输出是在开发过程中常用的有效工具。它可以帮助我们理解代码的执行流程、发现潜在的错误，并且更容易进行调试和修复。

## 如何实现

```Elixir
IO.inspect("Hello, World!") 
```

以上代码将在控制台上打印出 `"Hello, World!"`，这样我们就可以看到程序在运行时的实际结果。我们还可以使用 `IO.inspect/2` 来打印出变量的值、对象的属性等，以便更好地理解代码的执行过程。

```Elixir
iex> x = [1, 2, 3]
[1, 2, 3]
iex> IO.inspect(x, label: "My list") 
My list: [1, 2, 3] 
[1, 2, 3]
```

在以上示例中，我们使用 `IO.inspect/2` 来打印出变量 `x` 的值，并在输出前加上了标签 `"My list"`。

## 深入了解

实际上，Elixir 中的调试输出还有更多的用法。我们可以通过 `IO.inspect/3` 来设置选项，如 `color: [background: :red]` 来更改输出的颜色，或者使用 `inspect()` 宏来自定义对象的输出格式。此外，我们还可以使用 `:erlang.trace/3` 来跟踪程序的执行过程，更深入地了解代码的运行情况。

## 查看相关文章

[《Elixir入门指南》](https://zhuanlan.zhihu.com/p/27237143)

[《Elixir开发常用工具》](https://zhuanlan.zhihu.com/p/47753509)

[官方文档：IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)

[官方文档：inspect() 宏](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#inspect/2)

[官方文档：:erlang.trace/3](https://erlang.org/doc/man/erlang.html#trace-3)

# 查看更多链接

[《Elixir中的调试技巧和陷阱》](https://www.barbarianmeetscoding.com/blog/elixir-debugging-tips-and-tricks-and-the-pitfalls-you-could-encounter-along-the-way/)

[《Elixir中有用的调试技巧》](https://blog.appsignal.com/2018/08/28/useful-elixir-debugging-tips.html)

[《如何使用IO.inspect加快Elixir代码的开发与调试》](https://www.jianshu.com/p/893466b4cced)