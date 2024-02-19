---
aliases:
- /zh/clojure/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:30.855845-07:00
description: "\u5728 Clojure \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\
  \u521B\u5EFA\u6216\u4FEE\u6539\u6587\u4EF6\u4EE5\u5C06\u6570\u636E\u4FDD\u5B58\u5728\
  \u60A8\u7684\u5E94\u7528\u7A0B\u5E8F\u4E4B\u5916\uFF0C\u4F7F\u6570\u636E\u6301\u4E45\
  \u5316\u3001\u914D\u7F6E\u3001\u8BB0\u5F55\u65E5\u5FD7\u6216\u8FDB\u884C\u8FDB\u7A0B\
  \u95F4\u901A\u4FE1\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u9879\u4EFB\u52A1\u662F\
  \u4E3A\u4E86\u5C06\u5E94\u7528\u7A0B\u5E8F\u72B6\u6001\u3001\u914D\u7F6E\u5916\u5316\
  \uFF0C\u6216\u5728\u7A0B\u5E8F\u7684\u4E0D\u540C\u90E8\u5206\u6216\u4E0D\u540C\u7A0B\
  \u5E8F\u4E4B\u95F4\u5171\u4EAB\u4FE1\u606F\u3002"
lastmod: 2024-02-18 23:08:58.844522
model: gpt-4-0125-preview
summary: "\u5728 Clojure \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u521B\
  \u5EFA\u6216\u4FEE\u6539\u6587\u4EF6\u4EE5\u5C06\u6570\u636E\u4FDD\u5B58\u5728\u60A8\
  \u7684\u5E94\u7528\u7A0B\u5E8F\u4E4B\u5916\uFF0C\u4F7F\u6570\u636E\u6301\u4E45\u5316\
  \u3001\u914D\u7F6E\u3001\u8BB0\u5F55\u65E5\u5FD7\u6216\u8FDB\u884C\u8FDB\u7A0B\u95F4\
  \u901A\u4FE1\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u8FD9\u9879\u4EFB\u52A1\u662F\u4E3A\
  \u4E86\u5C06\u5E94\u7528\u7A0B\u5E8F\u72B6\u6001\u3001\u914D\u7F6E\u5916\u5316\uFF0C\
  \u6216\u5728\u7A0B\u5E8F\u7684\u4E0D\u540C\u90E8\u5206\u6216\u4E0D\u540C\u7A0B\u5E8F\
  \u4E4B\u95F4\u5171\u4EAB\u4FE1\u606F\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 写作及其原因

在 Clojure 中写入文本文件涉及创建或修改文件以将数据保存在您的应用程序之外，使数据持久化、配置、记录日志或进行进程间通信。程序员执行这项任务是为了将应用程序状态、配置外化，或在程序的不同部分或不同程序之间共享信息。

## 如何操作：

### 使用 Clojure 内置函数向文件写入文本

`spit` 函数是在 Clojure 中向文件写入文本的最简单方法。它需要两个参数：文件路径和要写入的字符串。如果文件不存在，`spit` 会创建它。如果文件已存在，`spit` 则会覆盖它。

```clojure
(spit "example.txt" "Hello, world!")
```

要向现有文件追加文本，您可以使用带有 `:append` 选项的 `spit` 函数。

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

运行这些代码片段后，"example.txt" 将包含：

```
Hello, world!
Let's add this new line.
```

### 使用第三方库

虽然 Clojure 的内置功能通常足够使用，但社区为更复杂或特定的任务开发了强大的库。对于文件 I/O 来说，一个受欢迎的库是 `clojure.java.io`，它提供了更类似 Java 的文件处理方法。

使用 `clojure.java.io` 写入文件前，你首先需要导入它：

```clojure
(require '[clojure.java.io :as io])
```

然后，您可以使用 `writer` 函数获取一个 writer 对象，并使用 `spit` 函数（或其他如 `print`、`println` 的函数）来写入文件：

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "This is written using clojure.java.io"))
```

这将创建（或如果已存在则覆盖）"example_with_io.txt" 并写入文本：

```
This is written using clojure.java.io
```

请记住：`with-open` 确保文件在写入后正确关闭，避免潜在的资源泄漏。
