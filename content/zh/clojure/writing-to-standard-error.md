---
title:                "写入标准错误"
date:                  2024-01-19
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
标准错误是个特殊的输出流用来报告程序的错误消息。程序员用它分开正常的输出和错误信息，简化调试和日志记录。

## 如何做：
```Clojure
;; 输出到标准错误
(defn write-to-stderr [msg]
  (.write System/err (.getBytes msg)))

(write-to-stderr "出错了！\n")
```

输出样例：
```
出错了！
```

## 深入探索
标准错误（stderr）起源于Unix操作系统，它是三个主要的预定义流之一，其他两个是标准输入（stdin）和标准输出（stdout）。使用标准错误而不是标准输出可以让用户或其他程序区分正常消息和错误消息。Clojure中没有直接的语法来写入标准错误，但可以通过Java的系统类`System/err`来实现。其他语言如Python和Ruby有内建的stderr写入方法。

## 相关资源
- [Clojure Docs](https://clojuredocs.org/)
- [Java Platform SE 8 - System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
