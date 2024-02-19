---
aliases:
- /zh/clojure/reading-a-text-file/
date: 2024-01-20 17:54:08.377122-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u5C06\u6587\u4EF6\u5185\
  \u5BB9\u8F7D\u5165\u7A0B\u5E8F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\
  \u7528\u4E8E\u6570\u636E\u5904\u7406\u3001\u914D\u7F6E\u8BFB\u53D6\u6216\u8005\u4EC5\
  \u4EC5\u662F\u663E\u793A\u6587\u4EF6\u5185\u5BB9\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.843680
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u5C06\u6587\u4EF6\u5185\
  \u5BB9\u8F7D\u5165\u7A0B\u5E8F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\
  \u7528\u4E8E\u6570\u636E\u5904\u7406\u3001\u914D\u7F6E\u8BFB\u53D6\u6216\u8005\u4EC5\
  \u4EC5\u662F\u663E\u793A\u6587\u4EF6\u5185\u5BB9\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
读取文本文件就是将文件内容载入程序。程序员这么做主要用于数据处理、配置读取或者仅仅是显示文件内容。

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
