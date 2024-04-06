---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:30.855845-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A `spit` \u51FD\u6570\u662F\u5728 Clojure\
  \ \u4E2D\u5411\u6587\u4EF6\u5199\u5165\u6587\u672C\u7684\u6700\u7B80\u5355\u65B9\
  \u6CD5\u3002\u5B83\u9700\u8981\u4E24\u4E2A\u53C2\u6570\uFF1A\u6587\u4EF6\u8DEF\u5F84\
  \u548C\u8981\u5199\u5165\u7684\u5B57\u7B26\u4E32\u3002\u5982\u679C\u6587\u4EF6\u4E0D\
  \u5B58\u5728\uFF0C`spit` \u4F1A\u521B\u5EFA\u5B83\u3002\u5982\u679C\u6587\u4EF6\u5DF2\
  \u5B58\u5728\uFF0C`spit` \u5219\u4F1A\u8986\u76D6\u5B83\u3002"
lastmod: '2024-04-05T21:53:47.674996-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

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
