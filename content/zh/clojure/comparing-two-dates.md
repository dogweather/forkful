---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么和为什么？
两个日期的比较就是为了决定哪个日期在日历上更早，或者是两个日期是否完全相同。程序员之所以要做这个，是因为他们需要对时间点进行排序，或者确定特定事件是否已经发生。

## 如何做：
在Clojure中，我们可以使用内建的java.util.Date库来比较两个日期。以下是一些基本的示例：

```clojure
(import 'java.util.Date)

(def date1 (Date. 120 0 1)) ;; 2020-01-01
(def date2 (Date. 120 0 2)) ;; 2020-01-02

(defn date-comparison []
  (if (.before date1 date2)
    (println "Date1 is before Date2")
    (println "Date1 is not before Date2")))

(date-comparison)
```
输出如下:
```clojure
Date1 is before Date2
```
## 深入探讨:
Clojure的java.util.Date库基于Java的同名类。最初的Java Date类在1995年发布，尽管在之后的版本中很多方法已经过时，但在Clojure中仍然可以使用。

另一个选择是使用Java 8新增的java.time库，这个库使用更现代的日期时间API，提供了更丰富和灵活的日期时间操作方法。

在比较日期时，你通常会使用`.before` 或 `.after`方法，这两个方法都返回一个布尔值，表示一个日期是否在另一个日期之前或之后。你也可以使用`.compareTo`方法，这个方法会返回一个整数，表示两个日期的顺序（-1: 第一个日期在前，1: 第二个日期在前，0:两个日期相等）。

## 参阅：
1. [Clojure官方文档](https://clojure.org/)
2. [Java Date Official Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
3. [Java Time Official Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)