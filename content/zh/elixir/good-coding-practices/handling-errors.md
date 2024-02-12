---
title:                "处理错误"
aliases:
- /zh/elixir/handling-errors.md
date:                  2024-01-26T00:52:01.153093-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/handling-errors.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

错误处理意味着编写能够应对事情出现偏差时的代码。程序员这样做是为了防止崩溃，并确保他们的程序在“墨菲定律”出击时能够优雅地恢复。

## 如何操作：

在Elixir中，我们通常使用模式匹配和`case`语句来处理不同的结果，包括错误。

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "不能除以零。"}
      _ -> {:ok, a / b}
    end
  end
end

# 成功的除法运算
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 是 #{result}")

# 尝试除以零
{:error, reason} = Example.divide(10, 0)
IO.puts("错误：#{reason}")
```

示例输出：
```
10 / 2 是 5.0
错误：不能除以零。
```

当您运行这段Elixir代码时，根据您的输入，您将获得成功的除法运算结果或错误消息。这里没有崩溃！

## 深入了解

早期，错误处理通常是关于检查返回值的。然而，由于Elixir的函数式根源，我们拥有了模式匹配和标签元组，比如`{:ok, value}`或`{:error, reason}`，这些方法更加优雅。

Elixir中还有其他处理错误的方法：

- **Elixir的`try`和`rescue`**，它们类似于命令式语言中的传统`try-catch`，但由于Elixir偏好明确性，它们使用得较少。
- **Supervisors和GenServers**，Elixir的OTP框架的一部分，它们更多关于容错。它们监控您的代码进程，如果出现问题准备重启它。

在实现上，Elixir借鉴了Erlang的稳健性。它将错误视为又一种消息类型，可以用所有的模式匹配和函数式的优点来处理。

## 另请参阅

想要进一步了解Elixir中的错误处理，请查看：

- Elixir官方指南关于[错误处理](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)。
- 了解更多关于[进程和OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)。
- Elixir论坛一直是提问的好地方：[https://elixirforum.com](https://elixirforum.com)。
