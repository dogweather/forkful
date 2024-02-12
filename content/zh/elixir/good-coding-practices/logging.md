---
title:                "日志记录"
aliases:
- /zh/elixir/logging/
date:                  2024-01-26T01:02:35.692046-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/logging.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
在软件开发中，日志记录是指在程序运行时记录发生的事件的技术，通常记录到文件或外部系统中。程序员这么做是为了获取软件行为的洞察、排除故障，并且维护操作历史记录，这对于调试和监控应用程序的健康至关重要。

## 如何操作：
在Elixir中，记录信息的主要方式是通过内置的`Logger`模块。以下是使用它的方法：

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("开始重要进程，参数为：#{param}")

    # 模拟正在完成的工作
    :timer.sleep(1000)

    Logger.debug("进程完成。")
  rescue
    error -> Logger.error("发生错误：#{inspect(error)}")
  end
end

# 要查看日志，你只需调用该函数：
MyApplication.do_something_important("MyParam")
```

这个简单的片段展示了如何以不同的级别记录日志（`info`，`debug`和`error`）。当你运行它时，除非你将Logger级别配置为`:debug`，否则你不会看到调试消息。默认情况下，Elixir的Logger会过滤掉级别低于`info`的日志消息。

在`:info`级别下的示例输出可能如下所示：
```
14:32:40.123 [info]  开始重要进程，参数为：MyParam
14:32:41.126 [error] 发生错误：％RuntimeError{message: "运行时错误"}
```

## 深入探讨：
Elixir的`Logger`是内置工具，自该语言初期就是其部分组成。它受其他BEAM语言如Erlang的日志系统影响。Logger提供了不同级别的日志记录 - `:debug`，`:info`，`:warn`和`:error` - 并且它是可插拔的，允许不同的后端挂接以处理日志消息。

对于更复杂的场景，一个内置Logger的替代品是使用如`Logstash`或`Sentry`这样的Elixir日志库，它们能提供额外的特性，如错误追踪和更直观格式的聚合。对于本地开发，Elixir开发人员常依赖内置Logger的功能，因为它简单且与BEAM VM集成。

在底层，Logger模块提供了异步和同步日志记录。默认的是异步日志记录，它不会在记录消息时阻塞你的应用程序执行。这确保了日志记录不会对性能产生负面影响。然而，在你需要保证消息按发送顺序记录时，可以启用同步日志记录。

可以在Elixir应用程序的`config/config.exs`文件中调整Logger配置，你可以设置日志级别、格式、元数据等。始终记得为不同的环境调整你的日志级别和输出；你不会希望冗长的调试日志充斥你的生产系统。

## 参见：
- 官方Elixir Logger文档：https://hexdocs.pm/logger/Logger.html
- 关于Elixir日志记录最佳实践的博客文章：https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Hex上的Sentry for Elixir：https://hex.pm/packages/sentry
- Elixir School对Logger的课程：https://elixirschool.com/en/lessons/specifics/debugging/#logging
