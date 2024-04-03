---
date: 2024-01-26 01:01:29.254834-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u501F\u9274\u4E86Java\u7684\u65E5\
  \u5FD7\u8BB0\u5F55\u8BBE\u65BD\uFF0C\u4F46\u60A8\u53EF\u4EE5\u7528\u66F4\u7B26\u5408\
  Clojure\u4E60\u60EF\u7684\u65B9\u5F0F\u5229\u7528\u5B83\u4EEC\u3002\u8BA9\u6211\u4EEC\
  \u770B\u4E00\u4E0B\u60A8\u5982\u4F55\u4F7F\u7528`clojure.tools.logging`\uFF0C\u5B83\
  \u63D0\u4F9B\u4E86\u5BF9\u51E0\u79CD\u65E5\u5FD7\u6846\u67B6\u7684\u7B80\u5355\u62BD\
  \u8C61\uFF1A\u2026"
lastmod: '2024-03-13T22:44:47.309589-06:00'
model: gpt-4-1106-preview
summary: "Clojure\u501F\u9274\u4E86Java\u7684\u65E5\u5FD7\u8BB0\u5F55\u8BBE\u65BD\uFF0C\
  \u4F46\u60A8\u53EF\u4EE5\u7528\u66F4\u7B26\u5408Clojure\u4E60\u60EF\u7684\u65B9\u5F0F\
  \u5229\u7528\u5B83\u4EEC\u3002\u8BA9\u6211\u4EEC\u770B\u4E00\u4E0B\u60A8\u5982\u4F55\
  \u4F7F\u7528`clojure.tools.logging`\uFF0C\u5B83\u63D0\u4F9B\u4E86\u5BF9\u51E0\u79CD\
  \u65E5\u5FD7\u6846\u67B6\u7684\u7B80\u5355\u62BD\u8C61\uFF1A\n\n\u9996\u5148\uFF0C\
  \u5728\u60A8\u7684`project.clj`\u4E2D\u6DFB\u52A0`clojure.tools.logging`\u548C\u4E00\
  \u4E2A\u65E5\u5FD7\u5B9E\u73B0\uFF08\u4F8B\u5982`log4j`\uFF09\u7684\u4F9D\u8D56\u9879\
  \uFF1A."
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 如何操作：
Clojure借鉴了Java的日志记录设施，但您可以用更符合Clojure习惯的方式利用它们。让我们看一下您如何使用`clojure.tools.logging`，它提供了对几种日志框架的简单抽象：

首先，在您的`project.clj`中添加`clojure.tools.logging`和一个日志实现（例如`log4j`）的依赖项：

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

现在，让我们记录一些消息：

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "开始进行强度计算...")
  (Thread/sleep 3000) ; 模拟一个长时间的计算
  (log/info "计算完成。答案是42。")
  42)

(compute-answer-to-everything)
```
默认情况下，输出不会显示`DEBUG`消息，因为日志级别通常设置为`INFO`：

```
INFO  [your-namespace] - 计算完成。答案是42。
```

如果需要，您可以在`log4j.properties`文件中配置日志级别和附加功能，以获得更详细的输出。

## 深入探索
Clojure的`clojure.tools.logging`已经存在一段时间，并且作为Clojure代码与Java日志世界之间的桥梁。历史上，Java已经经历了几次迭代和多个日志库，例如Java内置的日志API、`log4j`、`slf4j`和`logback`。

在Clojure中，虽然你可以直接使用Java的日志框架，但`clojure.tools.logging`能够检测类路径中的任何日志框架，并将任务委托给它，这样你就不必紧密耦合到具体的实现。这有助于保持你的Clojure代码的可移植性和模块性。

Clojure生态系统内的`clojure.tools.logging`替代品包括如`timbre`这样的库，它是一个纯Clojure的日志库，支持日志轮转、过滤和出箱即用的异步日志记录等功能。

在Clojure这样的多线程环境中，实现日志记录的细节至关重要。在这里，不变性和副作用管理提供了明显的优势。日志记录作为一种副作用，应谨慎处理，以避免性能瓶颈并确保线程安全，而这是大多数Java日志框架已经解决的问题。

最后，考虑结构化日志记录，其中日志以结构化数据（如JSON）的形式编写。这对于后期分析和处理特别有用，特别是在处理大规模分布式系统时。

## 另请参见
如果你渴望了解更多，请考虑查阅这些资源：

- Clojure Tools Logging 文档：https://github.com/clojure/tools.logging
- Timbre，一个Clojure日志库：https://github.com/ptaoussanis/timbre
- 在Clojure中配置Log4J：http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logback手册，用于高级设置：http://logback.qos.ch/manual/
- Clojure中结构化日志记录指南：https://corfield.org/blog/2020/04/28/structured-logging/
