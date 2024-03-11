---
date: 2024-01-20 17:52:12.812894-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u8BB0\u5F55\u6216\u663E\u793A\
  \u7A0B\u5E8F\u8FD0\u884C\u4E2D\u7684\u53D8\u91CF\u503C\u3001\u72B6\u6001\u6216\u9519\
  \u8BEF\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5FEB\
  \u901F\u627E\u5230\u5E76\u89E3\u51B3\u4EE3\u7801\u4E2D\u7684\u95EE\u9898\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.125434-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u8BB0\u5F55\u6216\u663E\u793A\
  \u7A0B\u5E8F\u8FD0\u884C\u4E2D\u7684\u53D8\u91CF\u503C\u3001\u72B6\u6001\u6216\u9519\
  \u8BEF\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5FEB\
  \u901F\u627E\u5230\u5E76\u89E3\u51B3\u4EE3\u7801\u4E2D\u7684\u95EE\u9898\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
打印调试输出是记录或显示程序运行中的变量值、状态或错误信息。程序员这样做是为了快速找到并解决代码中的问题。

## How to: (怎么做：)
在Elixir中，你可以用`IO.inspect`来打印调试信息，还能看到变量的值。下面是个简单例子：

```elixir
defmodule Greeter do
  def hello(name) do
    IO.inspect(name, label: "Debug name")
    "Hello, #{name}!"
  end
end

Greeter.hello("World")
```

输出会是这样：
```
Debug name: "World"
"Hello, World!"
```

## Deep Dive (深入了解)
`IO.inspect`不仅是为了调试。它最早在Elixir里出现是为了让开发者能够轻松检查任何表达式的值而不改变表达式的行为。除了`IO.inspect`，你还可以使用`IO.puts` 或 `Logger`模块。`Logger`更适合生产环境，因为它有不同的日志等级和更多配置选项。

在实现细节方面，`IO.inspect`是同步的，这意味着程序会等待输出完成后才继续运行。这个方法通常不会影响原始数据，可以放心在任何地方用来检查数据。

## See Also (另请参阅)
- Elixir官方文档 `IO.inspect`: https://hexdocs.pm/elixir/IO.html#inspect/2
- Elixir官方文档 `Logger`: https://hexdocs.pm/logger/Logger.html
- 更多关于Elixir中`IO`模块的信息: https://hexdocs.pm/elixir/IO.html
- 关于打印输出和调试的讨论: https://elixirforum.com
