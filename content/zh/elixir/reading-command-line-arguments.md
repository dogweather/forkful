---
title:    "Elixir: 读取命令行参数"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么要阅读命令行参数

阅读命令行参数是编程中一个基础的技能，它能帮助我们从命令行获取输入，并根据不同的参数执行不同的操作。通过阅读命令行参数，我们能够更有效地控制程序的行为，使代码更灵活。

## 如何阅读命令行参数

在Elixir中，我们可以使用IO模块的`argv`函数来获取命令行参数。下面是一个例子：

```Elixir
args = IO.argv
```

使用`argv`函数会将命令行参数存储在一个列表中，并赋值给变量`args`。我们可以使用`length`函数来获取传递的参数数量：

```Elixir
length(args)
```

同样，我们也可以使用`nth`函数来获取特定位置的参数：

```Elixir
arg = List.nth(args, 0)
```

当我们运行上面的代码，并在命令行输入`elixir example.exs foo bar`，则变量`arg`的值就会变为`foo`。

## 深入了解阅读命令行参数

在Elixir中，我们也可以使用模式匹配来处理命令行参数。例如，我们可以使用下划线来忽略不需要的参数，只关心特定的参数：

```Elixir
[_, _, first_param, _] = IO.argv
```

这样，变量`first_param`的值就会是第三个传递的参数。

在实际的项目中，我们也可以结合使用`OptionParser`模块来更方便地处理命令行参数。该模块可以帮助我们定义不同的参数，并根据用户输入的不同参数执行相应的操作。有关更多信息，请查看Elixir的官方文档。

## 参考链接

- [Elixir官方文档](https://elixir-lang.org/getting-started/)

## 查看也可以

- [如何使用Elixir编写命令行工具](https://dev.to/marv1n/how-to-write-command-line-tools-in-elixir-1emi)
- [深入理解Elixir的模式匹配](https://www.javascriptjanuary.com/blog/gentle-intro-to-pattern-matching-in-elixir)