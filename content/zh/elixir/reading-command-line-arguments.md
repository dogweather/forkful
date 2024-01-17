---
title:                "读取命令行参数"
html_title:           "Elixir: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 最新版本的Elixir编程文章：从命令行读取参数

## 什么是命令行参数？为何程序员要这样做？
命令行参数是指在程序运行时，通过命令行（或终端）输入的信息。这些参数可以帮助程序执行不同的任务或配置不同的设置。程序员通常读取命令行参数，是为了更方便地控制程序的行为，同时也可以减少代码量。

## 如何读取命令行参数？
让我们来看一个简单的例子，假设我们要在命令行中输入一个数字，并将其加1后输出。下面是如何使用Elixir读取命令行参数的示例代码：

```Elixir
defmodule AddOne do
  def main do
    args = System.argv
    num = Enum.at(args, 1) |> String.to_integer
    IO.puts num + 1
  end
end

AddOne.main()
```

现在，在命令行中输入：```elixir add_one.exs 5```（这里假设你的文件名是add_one.exs），你会得到输出：6。我们首先使用System.argv函数将所有命令行参数存储在变量args中。然后，通过使用Enum.at函数和String.to_integer函数，我们提取第二个参数（索引从0开始），即输入的数字，然后将其加1并输出。

## 深入了解
读取命令行参数的概念已经存在很久了，它最早出现在命令行界面（CLI）中。随着图形用户界面（GUI）的流行，很多程序都不再使用命令行参数，但是在一些特定场景下，仍然有它们的用武之地。如果你想要了解其他读取命令行参数的方法，你可以参考[这篇文章](https://thoughtbot.com/blog/the-beauty-of-the-humble-command-line-argument)。

## 参考链接
- [Elixir官方文档](https://elixir-lang.org/getting-started/cmdline.html)
- [如何在Elixir中处理命令行参数](https://dev.to/oschlueter/handle-command-line-arguments-in-elixir-4j6n)