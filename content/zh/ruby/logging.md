---
title:                "日志记录"
aliases:
- zh/ruby/logging.md
date:                  2024-01-26T01:08:32.295712-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/logging.md"
---

{{< edit_this_page >}}

## 什么是日志记录以及为什么要日志记录？
在编程中，日志记录就像是给你的应用程序保留一本日记。它是系统化地记录事件、消息和数据点，让你洞察应用程序的行为以及它是如何运作的。开发人员之所以进行日志记录，是因为它对调试、监控应用程序运行状况至关重要，同时可以在问题真正变成麻烦之前，获取潜在问题的线索。

## 如何进行日志记录：
Ruby内置了一个非常容易使用的日志记录模块 `Logger`。以下是一个快速入门的例子：

```ruby
require 'logger'

# 创建一个输出到STDOUT的Logger
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# 示例日志信息
logger.info("这是一条信息消息")
logger.warn("这是一条警告消息")
logger.error("这是一条错误消息")
```

运行上面的脚本将输出类似于以下内容：

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : 这是一条信息消息
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : 这是一条警告消息
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : 这是一条错误消息
```

你可以配置日志格式和级别以过滤掉不必要的噪音，也可以将日志定向到不同的输出，如文件或者甚至是外部日志服务。

## 深入了解
在编程中，日志记录就像一项古老的传统。从历史上看，日志通常是简单的文本文件，使用像 `grep` 这样的工具手动解析。但这个概念已经演变成了一个拥有强大日志框架和服务的整个生态系统，比如在Linux上的Log4j、Syslog，或者在云时代的Sematext和Loggly。

Ruby的 `Logger` 是一个简单的起点，但是如果你需要更强大的功能和灵活性，你可能会考虑使用像Lograge或Semantic Logger这样的替代库。这些库与Ruby应用程序搭配地很好，提供更精细的对日志格式的控制，包括结构化日志（JSON格式）、更好的性能，并且可以与其他服务无缝集成。

尽管每个Ruby日志库都有其独特的实现方式，但它们归根结底都围绕着一个你向其发送消息的日志实例的概念。日志处理器根据设置的级别——DEBUG, INFO, WARN, ERROR, FATAL, 和 UNKNOWN——来处理这些消息，并决定对它们如何处理：打印出来、保存到文件、通过网络发送等等。

## 另请参阅
如需深入了解Ruby内建的日志模块，请查看官方文档：

如果你对更高级的日志记录感兴趣或想探索第三方gems，请查看：
- [Lograge](https://github.com/roidrage/lograge)

对于一般的日志记录实践和哲学（不特定于Ruby），以下这些文章是不朽的阅读材料：
- [Google的网站可靠性工程书 - 第16章：处理过载](https://sre.google/sre-book/handling-overload/#log-messages)
- [12因素应用程序 - 日志](https://12factor.net/logs)
