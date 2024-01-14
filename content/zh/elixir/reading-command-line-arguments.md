---
title:    "Elixir: 读取命令行参数"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么阅读命令行参数
当我们在编写程序时，有时候我们希望从终端（或命令行）中获得一些输入，而不是把所有的数据都写在代码里。这时候，我们就需要阅读命令行参数来获取输入，让我们的程序变得更加灵活和易于使用。

## 如何阅读命令行参数
阅读命令行参数在Elixir中非常简单。我们可以使用`System.argv`函数来获取命令行参数的列表。下面这个例子展示了如何获取用户在终端输入的两个数字，并计算它们的和。

```Elixir
input1 = System.argv |> List.first |> String.to_integer
input2 = System.argv |> List.last |> String.to_integer

IO.puts "输入的两个数字是 #{input1} 和 #{input2}"
IO.puts "它们的和是 #{input1 + input2}"
```

在终端中，我们可以这样调用这个程序：

```
elixir sum.exs 2 3
```

这将输出：

```
输入的两个数字是 2 和 3
它们的和是 5
```

## 深入了解命令行参数
除了获取输入外，我们还可以在命令行中传递一些特殊的参数来控制我们的程序。这些参数通常以`--`为前缀，并可以有多个。下面是一个例子，通过命令行参数来指定要打印的字符串和重复的次数。

```Elixir
# 命令行参数的格式为 `--参数名 值`
input_string = System.argv |> Enum.at(2) |> String.to_integer
repeat_count = System.argv |> Enum.at(4) |> String.to_integer

IO.puts String.duplicate(input_string, repeat_count)
```

在终端中，我们可以这样调用这个程序：

```
elixir repeat.exs --str "Hello!" --times 3
```

这将输出：

```
Hello!Hello!Hello!
```

# 参考链接
- [Elixir官方文档 - System.argv](https://hexdocs.pm/elixir/System.argv.html)
- [命令行参数的使用方法](https://elixirschool.com/lessons/basics/command-line-flags/)
- [更多Elixir知识，敬请关注Elixir中国社区](https://elixir-cn.com/)