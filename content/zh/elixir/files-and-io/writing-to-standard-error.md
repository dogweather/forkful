---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:03.194705-07:00
description: "\u5728 Elixir \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF (stderr) \u662F\
  \u4E00\u79CD\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u4E0E\u4E3B\
  \u8F93\u51FA (stdout) \u5206\u5F00\u7684\u65B9\u6CD5\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528 stderr \u6765\u8C03\u8BD5\u548C\u5904\u7406\u9519\u8BEF\uFF0C\u800C\u4E0D\
  \u4F1A\u5E72\u6270\u7A0B\u5E8F\u7684\u4E3B\u8981\u8F93\u51FA\uFF0C\u8FD9\u4F7F\u5F97\
  \u8BC6\u522B\u548C\u89E3\u51B3\u95EE\u9898\u53D8\u5F97\u66F4\u52A0\u5BB9\u6613\u3002"
lastmod: '2024-02-25T18:49:45.005299-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Elixir \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF (stderr) \u662F\
  \u4E00\u79CD\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u4E0E\u4E3B\
  \u8F93\u51FA (stdout) \u5206\u5F00\u7684\u65B9\u6CD5\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528 stderr \u6765\u8C03\u8BD5\u548C\u5904\u7406\u9519\u8BEF\uFF0C\u800C\u4E0D\
  \u4F1A\u5E72\u6270\u7A0B\u5E8F\u7684\u4E3B\u8981\u8F93\u51FA\uFF0C\u8FD9\u4F7F\u5F97\
  \u8BC6\u522B\u548C\u89E3\u51B3\u95EE\u9898\u53D8\u5F97\u66F4\u52A0\u5BB9\u6613\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
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
