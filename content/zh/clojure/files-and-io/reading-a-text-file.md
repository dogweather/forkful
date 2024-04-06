---
date: 2024-01-20 17:54:08.377122-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C\uFF1A \u65E9\u671FClojure\u7248\u672C\
  \u8BFB\u6587\u4EF6\u4E0D\u90A3\u4E48\u76F4\u89C2\uFF0C\u51FD\u6570\u5E93\u4E5F\u6CA1\
  \u6709\u73B0\u5728\u8FD9\u4E48\u4E30\u5BCC\u3002`slurp` \u662F\u4E2A\u5FEB\u6377\
  \u51FD\u6570\uFF0C\u76F4\u63A5\u8BFB\u53D6\u6574\u4E2A\u6587\u4EF6\u5185\u5BB9\u4E3A\
  \u4E00\u4E2A\u5B57\u7B26\u4E32\u3002\u5982\u679C\u6587\u4EF6\u5927\uFF0C\u53EF\u80FD\
  \u4E0D\u592A\u5B9E\u7528\uFF0C\u56E0\u4E3A\u4F1A\u6D88\u8017\u5927\u91CF\u5185\u5B58\
  \u3002 \u9010\u884C\u8BFB\u53D6\u4F7F\u7528 `line-seq`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.673598-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u65E9\u671FClojure\u7248\u672C\u8BFB\u6587\
  \u4EF6\u4E0D\u90A3\u4E48\u76F4\u89C2\uFF0C\u51FD\u6570\u5E93\u4E5F\u6CA1\u6709\u73B0\
  \u5728\u8FD9\u4E48\u4E30\u5BCC\u3002`slurp` \u662F\u4E2A\u5FEB\u6377\u51FD\u6570\
  \uFF0C\u76F4\u63A5\u8BFB\u53D6\u6574\u4E2A\u6587\u4EF6\u5185\u5BB9\u4E3A\u4E00\u4E2A\
  \u5B57\u7B26\u4E32\u3002\u5982\u679C\u6587\u4EF6\u5927\uFF0C\u53EF\u80FD\u4E0D\u592A\
  \u5B9E\u7528\uFF0C\u56E0\u4E3A\u4F1A\u6D88\u8017\u5927\u91CF\u5185\u5B58."
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## How to: 如何操作：
```Clojure
;; 读取整个文件
(slurp "example.txt")
```
```Clojure
;; 逐行读取
(with-open [rdr (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```
输出：
```
第一行内容
第二行内容
...
```

## Deep Dive 深入了解
早期Clojure版本读文件不那么直观，函数库也没有现在这么丰富。`slurp` 是个快捷函数，直接读取整个文件内容为一个字符串。如果文件大，可能不太实用，因为会消耗大量内存。

逐行读取使用 `line-seq` 函数，在一个打开的文件阅读器上操作，这样可以更节省内存，特别对大文件有好处。此外，有其他库提供更多功能和效率，如 `clojure.data.csv` 可以用于处理 CSV 文件。

另一个关注点是错误处理：`with-open` 宏会确保文件阅读器正常关闭，即使在读取过程中发生错误。

## See Also 另请参阅
- Clojure官方文档：[https://clojure.org/](https://clojure.org/)
- `clojure.java.io` 库：[https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- 处理 CSV：[https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
