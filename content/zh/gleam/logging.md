---
title:                "日志记录"
date:                  2024-01-26T01:03:44.443674-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/logging.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
日志记录本质上是我们记录程序中发生情况的方式。就像拥有一个小黑匣子；当事情出错时（相信我，它一定会的），日志对于弄清楚发生了什么、诊断问题和优化性能来说是无价的。

## 如何进行：
在Gleam中，你通常需要引入一个日志库——并没有一个专门的日志机制。假设我们使用一个假想的`gleam_logger`库。以下是你可以集成它的方法：

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("应用程序正在启动！")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("计算成功", value)
    Error(err) -> 
      gleam_logger.error("计算失败", err)
  }
}
```

你的日志中的预期输出看起来应该像这样：

```
INFO: 应用程序正在启动！
DEBUG: 计算成功 42
ERROR: 计算失败 原因：除以零
```

## 深入研究
记录日志的艺术从编程的早期就已存在。系统操作员会实际从计算机中获取日志——确保一切顺利运行。快进到现在，记录日志已经数字化，成为软件开发的核心部分。

虽然Gleam作为一个相对年轻的语言，目标是Erlang生态系统，它没有内置的日志框架，但你可以利用成熟的Erlang记录日志设施或其他社区提供的库。每个都有不同的功能和权衡：有些可能提供结构化的日志记录，其他的可能更适合简单文本输出。

现在，实现一个日志设施的问题：它简单吗？乍一看，是的。但是剥开它的层次，你需要处理并发、I/O瓶颈、日志轮转、格式标准化（想想结构化日志记录的JSON）、级别过滤，甚至可能是分布式跟踪。另外，在函数式范式中，你通常希望副作用（如日志记录）以可预测和可控的方式处理。

## 另请参阅
以下是你可以找到更多关于Gleam及其周围生态系统中日志的详细信息的地方：
- [Erlang的:logger文件](http://erlang.org/doc/apps/kernel/logger_chapter.html)：由于Gleam编译为Erlang，这是直接适用的。
- [Gleam的标准库文档](https://hexdocs.pm/gleam_stdlib/)：可能会添加任何日志工具的更新。
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam)：一个精选的资源列表，可能会包括随着时间的推移而提供的日志库。
