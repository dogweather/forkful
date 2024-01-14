---
title:                "Clojure: 输入“将日期转换成字符串”"
simple_title:         "输入“将日期转换成字符串”"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候需要将日期转换为字符串，这种转换可以让我们更方便地使用和处理日期数据。

## 如何实现

```Clojure
(require '[clojure.java-time :as t])

(def date (t/date-time-numbers 2021 9 20))

;; 使用format函数将日期转换为字符串，输出格式为yyyy-MM-dd
(str (t/format date "yyyy-MM-dd"))
;; => "2021-09-20"

;; 也可以指定具体的格式，比如yyyy/MM/dd HH:mm:ss
(str (t/format date "yyyy/MM/dd HH:mm:ss"))
;; => "2021/09/20 00:00:00"

```

## 深入了解

在Clojure中，日期和时间数据被称为Instant，它是以毫秒为单位的时间戳。我们可以通过使用clojure.java-time这个官方库来处理Instant类型的数据，并进行日期和时间的格式化和转换。关于该库的更多信息可以查看官方文档。

另外，当我们将日期转换为字符串时，可以使用不同的格式来满足不同的需求。比如如果需要将日期作为文件名，可以使用yyyyMMdd的格式，如果需要展示给用户阅读，可以使用常见的yyyy-MM-dd格式。

## 参考链接

[clojure.java-time官方文档](https://clojure.github.io/java-time/)
[Clojure函数库之日期处理](https://zhuanlan.zhihu.com/p/39893522)