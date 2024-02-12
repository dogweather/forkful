---
title:                "读取命令行参数"
aliases:
- /zh/elixir/reading-command-line-arguments.md
date:                  2024-01-20T17:55:43.582423-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
命令行参数读取就是从用户输入获取信息的过程。程序员这么做是为了使程序更灵活，能根据不同的参数执行不同的任务。

## How to: (如何操作：)
在Elixir中，你可以使用`System.argv`来读取命令行参数。例子如下：

```elixir
# my_script.exs
defmodule MyScript do
  def main(args) do
    IO.inspect(args)
  end
end

MyScript.main(System.argv())
```

如果运行 `elixir my_script.exs arg1 arg2 arg3`，输出会是：

```
["arg1", "arg2", "arg3"]
```

## Deep Dive (深入了解)
Elixir语言派生自Erlang，这是一种为高可用性和可伸缩性而设计的语言。命令行参数的读取沿用了Erlang强大的系统工具。除`System.argv`外，你还可以使用`OptionParser`来解析带有特定标志的参数，它提供了更强的功能和灵活性。在实现时，Elixir在启动时填充`System.argv`，你可以随时修改这个列表，以影响程序后续的动作。

## See Also (另请参阅)
- Elixir官方文档关于`System.argv`: https://hexdocs.pm/elixir/System.html#argv/0
- 关于`OptionParser`的信息: https://hexdocs.pm/elixir/OptionParser.html
