---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

命令行参数是程序执行时由用户提供的外部输入。程序员读取命令行参数来调整程序行为，并使程序更具灵活性。

## 如何做：

Elixir 中用 `System.argv/0` 函数读取命令行参数。这个函数返回字符串列表，列表中的每个元素都是一个参数。以下是简单示例：

```elixir
defmodule MyScript do
  def main do
    System.argv()
    |> Enum.at(0)
    |> IO.puts()
  end
end
```

运行这段程序，输出会显示我们传的第一个命令行参数。

```bash
elixir my_script.exs Hello
> Hello
```

## 深入探索

### 历史背景

命令行参数可以追溯到最早的计算机系统，当时用户通过终端交互式地向程序提供输入。

### 替代方案

Erlang 的 `init:get_arguments/0` 函数可获取命令行参数。不过，Elixir 提供的 `System.argv/0` 是个更友好的封装。

### 实现细节

Elixir 读取命令行参数是通过调用 Erlang 运行时系统的相应函数实现的，这些系统级别的函数会在虚拟机启动时保存所有命令行参数。

## 参考资料

- [Elixir 官方文档 - System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Wikipedia 上的命令行参数](https://zh.wikipedia.org/wiki/%E5%91%BD%E4%BB%A4%E8%A1%8C%E5%8F%82%E6%95%B0)