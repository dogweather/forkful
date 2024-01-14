---
title:                "Clojure: 比较两个日期"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

为什么: 比较两个日期是一个在编程中常见的任务，它可以帮助我们更好地理解时间和日期的概念，同时也可以用来解决一些实际问题。

如何做: 在Clojure中比较两个日期可以使用内置的 `compare` 函数。首先定义两个日期变量，然后使用 `compare` 函数比较它们的大小，输出结果为 -1，0，或1，分别表示第一个日期小于、等于，或大于第二个日期。

```Clojure
(def date1 (java.util.Date. 2021 3 12))
(def date2 (java.util.Date. 2021 3 19))

(compare date1 date2)
```

输出结果为 -1，表示 date1 在 date2 之前。

深入探讨: 在Clojure中，日期通常以 `java.util.Date` 类型表示，这是一个内置的 Java 类。它保存了日期和时间的信息，包括年、月、日、时、分、秒等。我们可以使用 `java.util.Calendar` 类来操作日期，它提供了比 `java.util.Date` 更多的方法，例如获取指定日期的下一个月的日期。

值得注意的是，日期之间的比较并不仅限于使用 `compare` 函数。我们还可以使用 `before?` 和 `after?` 函数来判断一个日期是否在另一个日期之前或之后。

另外，如果我们需要更加灵活地处理日期，可以使用 `java.time` 类库，它提供了更多的功能和更简洁的语法。例如，我们可以使用 `java.time.LocalDate` 类来表示一个日期，使用 `compare` 函数来比较两个日期的大小。

See Also: 
- [Clojure 官方文档](https://clojure.org/)
- [Java 日期和时间文档](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html)
- [Clojure 核心函数文档](https://clojure.org/api/api)