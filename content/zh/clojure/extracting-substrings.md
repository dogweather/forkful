---
title:                "提取子字符串"
html_title:           "Clojure: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

Clojure是一种高效的编程语言，它采用了函数式编程的思想，让程序员能够以一种简洁而强大的方式来处理字符串。提取子字符串是一种常见的字符串操作，它可以帮助程序员快速地获取所需的信息，节省时间和精力。

## 如何操作

你可以使用`subs`函数来提取子字符串，它接受三个参数：字符串、开始索引和结束索引。例如，如果我们想要提取字符串`Clojure`的子字符串`oj`，我们可以这样编写代码：

```Clojure
(subs "Clojure" 2 4)
```

运行以上代码，会得到输出`oj`。

你还可以使用`clojure.string`命名空间中的函数来操作字符串。例如，如果我们想要提取一个字符串中的某一个单词，我们可以使用`split`函数来拆分字符串并选择相应的元素，如下所示：

```Clojure
(require '[clojure.string :as str])
(str/split "Hello world" #" ")[1]
```

运行以上代码，会得到输出`world`。

## 深入了解

除了以上提到的方法，Clojure还提供了更多操作字符串的函数，例如`take`、`drop`等。此外，考虑到字符串中可能存在中文、日语等多字节字符，Clojure也提供了相应的函数来处理这些情况，如`subs-updated`、`split-lines`等。

了解更多关于Clojure中字符串操作的函数，请参考官方文档：https://clojure.org/reference/strings。

## 参考链接

- https://clojure.org/reference/strings
- https://www.learn-clojure.com/strings/
- https://repl.it/@learn_clojure/BriefTenseInductives
- https://github.com/jafingerhut/clojure-string-cheatsheet
- https://clojuredocs.org/clojure.core/subs