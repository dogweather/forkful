---
title:                "将日期转换为字符串"
date:                  2024-01-20T17:36:03.315927-07:00
model:                 gpt-4-1106-preview
simple_title:         "将日期转换为字符串"

category:             "Clojure"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
什么以及为什么？: 把日期转换成字符串就是把表示日期的数据变成文本格式。程序员常这么做来显示、存储或者在网络中分享日期信息。

## How to:
如何操作:
```Clojure
;; 引入 Clojure 日期时间库
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

;; 创建一个日期时间对象
(def my-date (t/now))

;; 定义日期时间格式
(def formatter (f/formatters :basic-date-time))

;; 把日期转换成字符串
(def date-str (f/unparse formatter my-date))

;; 打印结果
(println date-str)
```
```
;; 示例输出
"20230404T101015Z"
```

## Deep Dive
深入研究: 早期在Java平台上处理时间和日期使用的是`java.util.Date`，但是其设计有限制，比如线程安全问题和可读性差。`clj-time`库基于Joda-Time，给Clojure提供了更好的日期时间API。除了`clj-time`，Clojure开发者现在也可以使用`java.time`库，它在Java 8引入，设计更加现代。日期转换成字符串时，格式必须明确，可以用标准格式也可以自定义。

## See Also
参考链接:
- clj-time GitHub: https://github.com/clj-time/clj-time
- Joda-Time: https://www.joda.org/joda-time/
- Clojure official documentation: https://clojure.org/guides/deps_and_cli
- Java Time (java.time): https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
