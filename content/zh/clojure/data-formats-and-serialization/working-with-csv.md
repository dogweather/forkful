---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:10.061534-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u7684\u6807\u51C6\u5E93\u4E2D\
  \u6CA1\u6709\u5185\u7F6E CSV \u89E3\u6790\u529F\u80FD\uFF0C\u4F46\u4F60\u53EF\u4EE5\
  \u4F7F\u7528 `clojure.data.csv` \u5E93\u6765\u5B9E\u73B0\u8FD9\u4E00\u76EE\u7684\
  \u3002\u9996\u5148\uFF0C\u5C06\u8FD9\u4E2A\u5E93\u6DFB\u52A0\u5230\u4F60\u7684\u9879\
  \u76EE\u4F9D\u8D56\u4E2D\u3002 \u5728\u4F60\u7684 `project.clj`\uFF0C\u6DFB\u52A0\
  \u4EE5\u4E0B\u4F9D\u8D56\uFF1A."
lastmod: '2024-04-05T22:38:46.510762-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u7684\u6807\u51C6\u5E93\u4E2D\u6CA1\
  \u6709\u5185\u7F6E CSV \u89E3\u6790\u529F\u80FD\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\
  \u7528 `clojure.data.csv` \u5E93\u6765\u5B9E\u73B0\u8FD9\u4E00\u76EE\u7684\u3002\
  \u9996\u5148\uFF0C\u5C06\u8FD9\u4E2A\u5E93\u6DFB\u52A0\u5230\u4F60\u7684\u9879\u76EE\
  \u4F9D\u8D56\u4E2D\u3002 \u5728\u4F60\u7684 `project.clj`\uFF0C\u6DFB\u52A0\u4EE5\
  \u4E0B\u4F9D\u8D56\uFF1A."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 如何操作：


### 读取 CSV 文件
Clojure 的标准库中没有内置 CSV 解析功能，但你可以使用 `clojure.data.csv` 库来实现这一目的。首先，将这个库添加到你的项目依赖中。

在你的 `project.clj`，添加以下依赖：
```clojure
[clojure.data.csv "1.0.0"]
```
要读取 CSV 文件并打印每行：
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
这将输出 CSV 的每一行作为一个 Clojure 向量。

### 写入 CSV 文件
要将数据写入 CSV 文件，你可以使用相同的 `clojure.data.csv` 库：
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
这将创建或覆盖 `outputfile.csv`，并用指定的数据填充它。

### 使用第三方库：`clojure.data.csv`
虽然对于在 Clojure 中处理 CSV，`clojure.data.csv` 可以说是最直接的库，但对于更复杂的任务，例如处理带有特殊字符或非传统分隔符的 CSV，你可能需要在生态系统内探索额外的选项，或甚至考虑使用 Apache Commons CSV 等库进行 Java 互操作。然而，对于 Clojure 中的大多数标准 CSV 处理任务，`clojure.data.csv` 提供了一套简单有效的工具。
