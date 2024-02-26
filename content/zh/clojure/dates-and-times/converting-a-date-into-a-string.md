---
date: 2024-01-20 17:36:03.315927-07:00
description: "\u4EC0\u4E48\u4EE5\u53CA\u4E3A\u4EC0\u4E48\uFF1F: \u628A\u65E5\u671F\
  \u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u628A\u8868\u793A\u65E5\u671F\u7684\
  \u6570\u636E\u53D8\u6210\u6587\u672C\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u5E38\u8FD9\
  \u4E48\u505A\u6765\u663E\u793A\u3001\u5B58\u50A8\u6216\u8005\u5728\u7F51\u7EDC\u4E2D\
  \u5206\u4EAB\u65E5\u671F\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:44.949556-07:00'
model: gpt-4-1106-preview
summary: "\u4EC0\u4E48\u4EE5\u53CA\u4E3A\u4EC0\u4E48\uFF1F: \u628A\u65E5\u671F\u8F6C\
  \u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u628A\u8868\u793A\u65E5\u671F\u7684\u6570\
  \u636E\u53D8\u6210\u6587\u672C\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u5E38\u8FD9\u4E48\
  \u505A\u6765\u663E\u793A\u3001\u5B58\u50A8\u6216\u8005\u5728\u7F51\u7EDC\u4E2D\u5206\
  \u4EAB\u65E5\u671F\u4FE1\u606F\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
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
