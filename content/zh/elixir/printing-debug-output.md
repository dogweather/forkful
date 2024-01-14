---
title:    "Elixir: 打印调试输出"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Why

为什么会打印调试输出？

当我们在编写Elixir代码时，难免会遇到一些错误或Bug。此时，打印调试输出是一种简单直接的方法，在代码中插入一些提示信息，有助于我们找出问题所在并进行调试。它可以帮助我们更加深入地了解程序的运行情况，从而提高代码质量。

# How To

使用打印调试输出非常简单，只需在代码中添加以下语句：

```Elixir
IO.puts("调试信息")
```

我们可以在任何位置插入这样的语句，以便在程序运行时打印出相关的信息。例如，我们可以在函数内部打印变量的值，以便检查其取值是否正确。

```Elixir
num = 10
IO.puts("num的值为#{num}")
```

以上代码会输出：`num的值为10`，从而帮助我们确认变量num的值是否正确。

# Deep Dive

除了简单地打印信息，我们还可以使用Elixir提供的`inspect`函数来打印复杂的数据结构，如列表、哈希表等。

```Elixir
list = [1, 2, 3]
IO.puts(inspect(list)) 
```

以上代码会输出：`[1, 2, 3]`，从而帮助我们更好地理解复杂数据结构的内容。

另外，我们还可以使用`Logger`模块来打印调试信息，它比普通的`IO.puts`函数提供了更多的控制选项，如日志级别、输出格式等。详情请参考[Elixir官方文档](https://hexdocs.pm/logger/Logger.html)。

# See Also

- [Elixir官方文档](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir中文社区](https://elixir-cn.com/)
- [Elixir入门指南](https://learnxinyminutes.com/docs/zh-cn/elixir-cn/)