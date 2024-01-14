---
title:                "Clojure: 读取文本文件"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

阅读文本文件是编程中的一个基本操作，可以帮助我们获取和处理文本数据。在Clojure中，我们可以使用简单的方法来读取文本文件，并从中提取需要的信息。

## 如何

首先，我们需要使用`clojure.java.io`命名空间来访问Clojure中的IO库。然后，使用`slurp`函数来读取文本文件，并将其内容放入一个字符串变量中。

```
Clojure
(require '[clojure.java.io :as io])

(def text (slurp "sample.txt"))
```

接下来，我们可以使用字符串操作函数来处理文本数据。例如，我们可以使用`split-lines`函数来拆分字符串为每行的列表，或者使用`substring`函数来提取特定的文本段落。

```
Clojure
(def lines (split-lines text))
(def paragraph (substring text 10 50))
```

最后，我们可以使用`println`函数来打印处理后的文本数据。

```
Clojure
(println "第一行：" (first lines))
(println "文章段落：" paragraph)
```

## 深入了解

如果我们想要更深入地处理文本数据，我们可以使用Clojure中的正则表达式来匹配和替换文本。Clojure中有一个`re-seq`函数可以方便地进行正则表达式搜索，并返回匹配的结果列表。

```
Clojure
(def links (re-seq #"https?://[^\s]+" text))
(println "所有链接：" links)
```

## 参考链接

- `clojure.java.io`命名空间：https://clojure.github.io/clojure/clojure.java.io-api.html
- 字符串操作函数：https://clojure.github.io/clojure/clojure.string-api.html
- 正则表达式函数：https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-find
- `re-seq`函数：https://clojuredocs.org/clojure.core/re-seq