---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:10.061534-07:00
description: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u6D89\u53CA\u89E3\u6790\u548C\u751F\u6210\u7ED3\u6784\u4E3A\u884C\u548C\u5217\u7684\
  \u6587\u672C\u6570\u636E\uFF0C\u7C7B\u4F3C\u4E8E\u7535\u5B50\u8868\u683C\u6570\u636E\
  \u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\u5E94\u7528\u7A0B\u5E8F\u3001\u6570\u636E\
  \u5E93\u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u4EE5\u53CA\u6570\u636E\u8F6C\u6362\
  \u4EFB\u52A1\u6765\u8BF4\u662F\u81F3\u5173\u91CD\u8981\u7684\uFF0C\u56E0\u4E3A CSV\
  \ \u4F5C\u4E3A\u4E00\u79CD\u8F7B\u91CF\u7EA7\u3001\u4E92\u64CD\u4F5C\u683C\u5F0F\
  \u88AB\u5E7F\u6CDB\u91C7\u7528\u3002"
lastmod: '2024-03-13T22:44:47.329547-06:00'
model: gpt-4-0125-preview
summary: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u6D89\
  \u53CA\u89E3\u6790\u548C\u751F\u6210\u7ED3\u6784\u4E3A\u884C\u548C\u5217\u7684\u6587\
  \u672C\u6570\u636E\uFF0C\u7C7B\u4F3C\u4E8E\u7535\u5B50\u8868\u683C\u6570\u636E\u3002\
  \u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\u5E94\u7528\u7A0B\u5E8F\u3001\u6570\u636E\u5E93\
  \u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u4EE5\u53CA\u6570\u636E\u8F6C\u6362\u4EFB\
  \u52A1\u6765\u8BF4\u662F\u81F3\u5173\u91CD\u8981\u7684\uFF0C\u56E0\u4E3A CSV \u4F5C\
  \u4E3A\u4E00\u79CD\u8F7B\u91CF\u7EA7\u3001\u4E92\u64CD\u4F5C\u683C\u5F0F\u88AB\u5E7F\
  \u6CDB\u91C7\u7528\u3002."
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
