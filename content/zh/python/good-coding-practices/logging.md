---
date: 2024-01-26 01:08:22.226339-07:00
description: "\u65E5\u5FD7\u8BB0\u5F55\u662F\u5728\u7A0B\u5E8F\u8FD0\u884C\u65F6\u8BB0\
  \u5F55\u5E94\u7528\u7A0B\u5E8F\u4E8B\u4EF6\u7684\u8FC7\u7A0B\uFF0C\u63D0\u4F9B\u4E00\
  \u4E2A\u9762\u5305\u5C51\u8FFD\u8E2A\u8DEF\u5F84\uFF0C\u7528\u4E8E\u6B7B\u540E\u5206\
  \u6790\uFF08post-mortem analysis\uFF09\u548C\u5B9E\u65F6\u76D1\u63A7\u3002\u7A0B\
  \u5E8F\u5458\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\u662F\u56E0\u4E3A\u5B83\u6709\u52A9\
  \u4E8E\u8C03\u8BD5\u95EE\u9898\u3001\u76D1\u63A7\u6027\u80FD\uFF0C\u5E76\u8FFD\u8E2A\
  \u7528\u6237\u884C\u4E3A\u4EE5\u4FBF\u5B89\u5168\u548C\u5206\u6790\u3002"
lastmod: '2024-03-11T00:14:21.029715-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u5FD7\u8BB0\u5F55\u662F\u5728\u7A0B\u5E8F\u8FD0\u884C\u65F6\u8BB0\
  \u5F55\u5E94\u7528\u7A0B\u5E8F\u4E8B\u4EF6\u7684\u8FC7\u7A0B\uFF0C\u63D0\u4F9B\u4E00\
  \u4E2A\u9762\u5305\u5C51\u8FFD\u8E2A\u8DEF\u5F84\uFF0C\u7528\u4E8E\u6B7B\u540E\u5206\
  \u6790\uFF08post-mortem analysis\uFF09\u548C\u5B9E\u65F6\u76D1\u63A7\u3002\u7A0B\
  \u5E8F\u5458\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\u662F\u56E0\u4E3A\u5B83\u6709\u52A9\
  \u4E8E\u8C03\u8BD5\u95EE\u9898\u3001\u76D1\u63A7\u6027\u80FD\uFF0C\u5E76\u8FFD\u8E2A\
  \u7528\u6237\u884C\u4E3A\u4EE5\u4FBF\u5B89\u5168\u548C\u5206\u6790\u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
---

{{< edit_this_page >}}

## 什么和为什么？
日志记录是在程序运行时记录应用程序事件的过程，提供一个面包屑追踪路径，用于死后分析（post-mortem analysis）和实时监控。程序员进行日志记录是因为它有助于调试问题、监控性能，并追踪用户行为以便安全和分析。

## 如何操作：
Python 自带了一个用于日志记录的内置模块。这是一个基础设置：
```Python
import logging

# 基础配置日志记录
logging.basicConfig(level=logging.INFO)

# 记录日志消息
logging.debug('这是一条调试消息')
logging.info('关于你的程序刚刚做了什么的信息')
logging.warning('一条警告消息')
logging.error('发生了一个错误')
logging.critical('程序无法恢复！')
```
当你运行这段代码时，你会看到以下输出（由于默认级别是WARNING，所以不会显示调试和信息消息）：
```
WARNING:root:一条警告消息
ERROR:root:发生了一个错误
CRITICAL:root:程序无法恢复！
```
你也可以设置日志记录到文件，而不是控制台：
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
现在你的日志将被定向到 'app.log' 文件。

## 深入了解
日志记录自编程早期以来就一直存在，系统日志是最古老的持久存储形式之一，除了实际保存数据的文件外。撇开历史不谈，日志记录的主要概念基本上保持不变，尽管工具已经发展。

Python 的 `logging` 模块非常强大和灵活。它允许程序员设置不同的日志级别（DEBUG, INFO, WARNING, ERROR, CRITICAL），这有助于分类和过滤日志。它具有分层日志系统，意味着你可以在记录器之间建立父子关系并将消息向上传播。

其他选择包括像 Loguru 或 structlog 这样的第三方库，它们提供了比内置日志模块更多的增强功能和更简单的接口。它们可以提供更美观的输出、更好的结构化数据序列化，以及更直观的处理日志配置的方式。

关于实现，设置日志记录时重要的是在应用程序开始时只做一次。建议在模块级使用 `logging.getLogger(__name__)` 来遵循 Python 日志记录的最佳实践。

日志记录在正常情况下不应严重影响应用程序的性能。然而，应注意记录的内容：过度冗长的日志记录，特别是在 DEBUG 级别，可能会减慢应用程序的速度，并迅速填满日志文件存储。

## 另请参见
想了解更多关于 Python 日志模块的信息，请查看官方的 Python 日志烹饪书（logging cookbook），其中有一些很好的示例和最佳实践：https://docs.python.org/3/howto/logging-cookbook.html

想深入了解结构化日志记录以及它如何帮助使日志更富信息量且便于分析，Loguru 的文档做得很好：https://loguru.readthedocs.io

另外，也可以看看 12 因素应用方法论，特别是关于日志的部分，以获取现代应用日志记录的观点：https://12factor.net/logs
