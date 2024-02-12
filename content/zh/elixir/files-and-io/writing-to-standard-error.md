---
title:                "写入标准错误"
aliases:
- /zh/elixir/writing-to-standard-error.md
date:                  2024-02-03T19:33:03.194705-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Elixir 中写入标准错误 (stderr) 是一种将错误消息和诊断信息与主输出 (stdout) 分开的方法。程序员使用 stderr 来调试和处理错误，而不会干扰程序的主要输出，这使得识别和解决问题变得更加容易。

## 如何操作：

在 Elixir 中，您可以使用 `IO` 模块函数，例如 `IO.puts/2` 和 `IO.warn/2`，来将消息写入标准错误：

```elixir
# 向 stderr 写入一个简单的消息
IO.puts(:stderr, "错误：出了些问题！")

# 使用 IO.warn，更适合警告/错误的语义
IO.warn("警告：您即将超出限制！")
```

`IO.puts/2` 在终端中的示例输出：
```
错误：出了些问题！
```

对于 `IO.warn/2`，输出会是类似的，但 `IO.warn/2` 是专门为警告设计的，可能在未来的 Elixir 版本中包括额外的格式化或行为。

**使用第三方库**

虽然 Elixir 的标准库通常足以处理标准错误输出，但对于更复杂的应用程序或配置不同的日志级别和输出，您可能会发现像 `Logger` 这样的库很有用。

使用 `Logger` 输出错误消息的示例：

```elixir
require Logger

# 配置 Logger 输出到 stderr
Logger.configure_backend(:console, device: :stderr)

# 写入一个错误消息
Logger.error("错误：无法连接到数据库。")
```

此设置将 `Logger` 的输出特别定向到 stderr，这对于将错误日志与标准日志消息分开非常有用。
