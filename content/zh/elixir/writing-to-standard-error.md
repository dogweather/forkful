---
title:    "Elixir: 写入标准错误"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#为什么

从事标准错误书写的原因可能是为了更好地进行调试和错误处理。通过将错误信息打印到标准错误流中，我们可以更方便地查看和分析程序的错误信息，从而更容易地修复代码中的错误。

##如何使用

在Elixir中，我们可以使用`IO.puts/2`函数向标准错误流（stderr）打印信息。下面是一个简单的例子：

```Elixir
IO.puts(:stderr, "这是一个错误信息")
```

运行该代码，我们可以在控制台中看到以下输出：

```bash
这是一个错误信息
```

##深入讲解

除了使用`IO.puts/2`函数，我们还可以使用`IO.write/2`函数向标准错误流打印信息。不同之处在于，`puts`函数会在输出信息的结尾添加一个换行符，而`write`函数不会。下面是一个使用`write`函数的例子：

```Elixir
IO.write(:stderr, "这是第一行错误信息")
IO.write(:stderr, "这是第二行错误信息")
```

运行该代码，我们可以得到以下输出：

```bash
这是第一行错误信息这是第二行错误信息
```

需要注意的是，`write`函数不会自动添加换行符，我们需要自己在需要添加换行符的地方加上`\n`符号。

#查看也可以

- [Elixir的标准库文档：IO模块](https://hexdocs.pm/elixir/IO.html)
- [Elixir编程指南：错误处理](https://elixirschool.com/zh-hans/lessons/basics/errors/)
- [了解Elixir的 STDERR 的用法](https://tipsofthecloud.blogspot.com/2019/11/understanding-elixir-stderr.html)