---
title:    "Elixir: 写入标准错误"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 为什么要向标准错误写入？

Elixir是一种功能强大的编程语言，它允许开发者写出高效、可重用的代码。在Elixir中，我们可以使用标准错误来记录程序中的错误和异常。除了帮助我们调试代码，写入标准错误还能让我们更好地理解程序的执行过程和运行时发生的问题。让我们来看看如何使用Elixir写入标准错误。

## 如何操作

要写入标准错误，我们可以使用`IO.write_stderr/1`函数，它接受一个字符串作为参数。让我们来看一个简单的例子：

```
Elixir IO.write_stderr("Oops, something went wrong!")
```

该代码将会在标准错误中输出字符串"Oops, something went wrong!"。如果运行时发生了错误，我们也可以使用`Kernel.inspect/2`函数来输出错误信息，如下所示：

```
Elixir IO.write_stderr(Kernel.inspect(error, message))
```

这样，我们可以在标准错误中获取更具体的信息，帮助我们更快地解决问题。

## 深入了解

在Elixir中，我们可以使用`IO.stream(:stderr)`来获取标准错误的流，这个流可以帮助我们更深入地理解程序的执行过程。我们也可以使用`IO.write/2`函数将字符串写入标准错误流，如下所示：

```
Elixir IO.write(IO.stream(:stderr), "Hello, world!")
```

通过使用标准错误流，我们可以将更多的信息记录下来，并在必要时作为调试工具。同时，使用`IO.stream(:stderr)`也可以让我们将标准错误重定向到其他的地方，比如日志文件。

# 参考资料

- [Elixir官方文档](https://elixir-lang.org/getting-started/io-and-the-factory.html#standard-error)
- [Elixir程序员指南](https://elixir-lang.org/getting-started/io-and-the-factory.html#standard-error)
- [Elixir论坛讨论](https://elixirforum.com/t/stderr-readability-in-iex/2212)