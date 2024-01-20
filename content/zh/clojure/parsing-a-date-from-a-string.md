---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

解析日期（Parsing a date）是从字符串中提取日期信息的过程。程序员进行解析，因为他们需要把从文本、网络等接口获取的字符串格式的日期，转化为他们可以在软件中直接使用和操作的日期格式。

## 如何操作：

在 Clojure 语言中，我们可以使用 Java Interop 及其 `java.text.SimpleDateFormat` 类来解析日期字符串。
下面是代码示例：

```clojure
(ns date-parsing.core
  (:import [java.text SimpleDateFormat]
           [java.util Date]))

(defn parse-date [date-string]
  (let [format (SimpleDateFormat. "MM/dd/yyyy")]
    (.parse format date-string)))

(def date-example "12/31/2020")

(println (parse-date date-example)) ; #inst "2020-12-31T00:00:00.000-00:00"
```
这个 Clojure 代码有效地从一个字符串 "12/31/2020" 中解析出日期。

## 深入研究：

1.历史背景: 在早期的程序设计中，日期通常用字符串来存储和表示。这导致了很多问题，比如日期格式的不一致和解析错误。随着时间的推移，程序员开始使用特定的日期数据类型来存储日期，并解析字符串中的日期。

2.替代方法: 在 Clojure 中，除了使用 Java Interop，也可以使用 clj-time 库进行日期解析。clj-time 是在 Joda-Time 库基础上构建的，并提供了强大的日期和时间处理功能。

3.实现细节：解析日期时需要定义日期的格式，例如 "MM/dd/yyyy"。这是因为不同地区使用的日期格式可能不同，而 `SimpleDateFormat` 需要知道怎样从字符串中解析日期。

## 参考其他：

1. [Java Interop 官方文档] (https://clojure.org/reference/java_interop)

2. [clj-time GitHub 仓库] (https://github.com/clj-time/clj-time)

这两个链接提供了有关在 Clojure 中处理日期和时间以及与 Java 交互的更多信息。