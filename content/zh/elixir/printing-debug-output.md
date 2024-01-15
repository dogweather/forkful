---
title:                "打印调试输出"
html_title:           "Elixir: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么

在编写代码时，调试输出是一个重要的工具。它可以帮助我们在程序中找到错误并进行修复，提高代码的可读性和调试效率。

## 如何做

### Elixir中输出调试信息

要在Elixir中输出调试信息，我们可以使用 `IO.inspect/2` 函数。让我们来看一个例子：

```Elixir
iex> IO.inspect("Hello World", label: "My Message")
My Message: "Hello World"
"Hello World"
```

在上面的例子中，我们使用了 `IO.inspect/2` 函数来输出字符串 "Hello World"。我们还可以使用 `label` 选项来为我们的调试信息添加标签，让它们更容易被识别。

### 调试当前代码

有时，我们想要查看当前代码执行的结果，以便更好地理解程序的执行流程。这时，我们可以使用 `IO.inspect/1` 函数。让我们来看一个示例：

```Elixir
iex> IO.inspect("Elixir")
"Elixir"
"Elixir"
```

在上面的例子中，我们没有使用 `label` 选项，因此只是简单地输出了 "Elixir"。

### 添加条件语句

有时候，我们只想在特定的条件下输出调试信息。在这种情况下，我们可以使用 `IO.inspect/4` 函数，并传递一个条件语句作为第四个参数。让我们来看一个例子：

```Elixir
iex> x = 10
10

iex> IO.inspect(x, label: "Value of x", unless: x < 5)
Value of x: 10
10

iex> IO.inspect(x, label: "Value of x", only: x < 5)
:ok
```

在上面的例子中，我们使用了 `unless` 和 `only` 选项来控制调试信息的输出。如果条件为假，`unless` 选项将会导致信息被输出，而 `only` 选项则会导致信息不被输出。

## 深入了解

`IO.inspect/2` 函数在Elixir中是如何工作的呢？它的内部实现其实是调用了 `IO.puts/2` 函数，在打印信息的同时，也会将该信息返回作为函数的结果。这就是为什么我们在调用 `IO.inspect/2` 的时候，同时也会输出结果的原因。

除了 `IO.inspect/2` 函数之外，Elixir还提供了一些其他的调试工具，比如 `Logger` 模块和 `IO.warn/2` 函数。它们都可以帮助我们更有效地调试我们的程序。

## 参考链接

- [Elixir官方文档](https://elixir-lang.org/getting-started/debugging.html)
- [Elixir论坛讨论](https://elixirforum.com/t/console-io-inspect-inspect-expressions-pipable-inspect-custom-exception-data-inspect-and-access-macro-inspect-docs/808/5)

## 参见

- [使用Debug模块调试Elixir应用](https://bluepenguin.yupoo.com/albums/36544818?uid=1)
- [Elixir语言特性简介](https://www.zhihu.com/question/432201138/answer/1643055604)