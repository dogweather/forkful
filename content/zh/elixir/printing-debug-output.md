---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 是什么和为什么?

打印调试输出就是在程序运行过程中输出一些信息，以便于开发者更清楚地了解程序运行状态。程序员采用这种方式来检测和排错。

## 怎么操作:

在 Elixir 中打印调试信息的常用方法是使用 `IO.inspect` 函数。这个函数会将输入内容进行格式化，并输出到终端以供查看。

```Elixir
defmodule Printer do
  def print_data(data) do
    IO.inspect(data)
  end
end

data = %{name: "John", age: 30}
Printer.print_data(data)
```

运行以上代码将会在终端打印出如下输出:

```Elixir
%{name: "John", age: 30}
```

## 深入理解:

### 历史背景：
`IO.inspect` 方法起源于 Erlang's io:format，为了提供便捷的调试信息打印功能，Elixir 将其进行了二次封装。

### 替代方案：
也可以通过 Logger 模块来打印调试信息，该模块除了支持信息打印，还支持日志级别（如：错误，警告，信息等）的设定。

```Elixir
require Logger
Logger.debug("debug message")
```

### 实现细节：
`IO.inspect` 实际上是调用了 `Inspect` 模块，该模块为 Elixir 对象提供了标准格式输出的能力。

## 参考：

- [Elixir Docs - IO](https://hexdocs.pm/elixir/IO.html)
- [Elixir School - Debugging](https://elixirschool.com/en/lessons/specifics/debugging/)
- [Elixir Lang - Logger](https://hexdocs.pm/logger/Logger.html)