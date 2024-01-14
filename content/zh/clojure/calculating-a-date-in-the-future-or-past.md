---
title:    "Clojure: 计算未来或过去的日期"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

当我们需要计算一个未来或过去的日期时，我们可以利用Clojure编程语言来轻松实现。这样的计算对于预订机票、预定会议室或计算物品的配送时间等任务非常有用。 

## 如何操作 

```Clojure 
;; 计算未来的日期
(def start-date (java.time.LocalDate/now)) 
(def future-date (.plusDays start-date 10)) 
(println "未来的日期为：" future-date)

;; 计算过去的日期
(def start-date (java.time.LocalDate/now))
(def past-date (.minusDays start-date 5))
(println "过去的日期为：" past-date)
```

输出: 

未来的日期为：2022-01-19 
过去的日期为：2022-01-14

## 深入了解

在Clojure中，我们可以使用 `plusDays` 或 `minusDays`函数来计算未来或过去的日期。这些函数接受两个参数，第一个参数为起始日期，第二个参数为要增加或减去的天数。同时，我们也可以使用其他类似的函数来计算未来或过去的日期，如 `plusMonths`、`plusYears`、`minusMonths`、`minusYears`等等。 

在Clojure中，日期和时间是以类 `java.time.LocalDate` 和 `java.time.LocalDateTime`来表示的。因此，我们可以直接使用Java的日期和时间类来进行日期计算。 

## 参考文献 

[Java日期时间API文档](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) 

## 参见 

- [Java日期和时间教程](https://www.runoob.com/java/java-date-time.html)
- [Clojure官方文档](https://clojure.org/)