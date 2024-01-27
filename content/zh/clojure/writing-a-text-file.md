---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为何?)
写文本文件是将字符串保存到可存取的文件中的行为。程序员这么做是为了数据持久化、配置、日志记录以及信息共享。

## How to: (如何做：)
Clojure 使用 `spit` 和 `slurp` 函数来写入和读取文件。 

写文件简例：
```Clojure
(spit "example.txt" "这是写入的文本内容。")
```
读文件验证写入：
```Clojure
(slurp "example.txt") ; => "这是写入的文本内容。"
```
追加文本到文件：
```Clojure
(spit "example.txt" " 这是追加的文本。" :append true)
```
读文件验证追加：
```Clojure
(slurp "example.txt") ; => "这是写入的文本内容。 这是追加的文本。"
```

## Deep Dive (深入了解)
Clojure 写文件方法简洁。`spit` 函数的历史可追溯到其他Lisp变体，是为了易用而设计。备选方法包括低级的 Java I/O 操作，但`spit`提供了更简洁的接口。`spit` 隐藏了复杂的细节，比如文件流的打开和关闭。性能上，对于大文件或高频写操作，应考虑使用更底层的Java方法，或者NIO库以提高效率。

## See Also (另请参阅)
- Clojure官网对`spit`函数的描述: [clojure.core/spit](https://clojuredocs.org/clojure.core/spit)
- 阅读文本文件的`slurp`函数: [clojure.core/slurp](https://clojuredocs.org/clojure.core/slurp)
- Java I/O操作指南：[Oracle Java Documentation](https://docs.oracle.com/javase/tutorial/essential/io/)
