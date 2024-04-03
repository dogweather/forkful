---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:10.061534-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A #."
lastmod: '2024-03-13T22:44:47.329547-06:00'
model: gpt-4-0125-preview
summary: '#.'
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
