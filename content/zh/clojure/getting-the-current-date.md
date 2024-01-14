---
title:                "Clojure: 获取当前日期"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么：获取当前日期的重要性

在进行编程时，我们经常需要使用日期作为程序的一部分。这可以是为了显示当前日期，或者计算某个特定日期与当前日期之间的差距。获取当前日期对于准确地记录时间和日期非常重要。

## 如何：Clojure中获取当前日期的方法

在Clojure中，获取当前日期的最简单方法是使用系统库中的`(java.util.Date.)`函数。这会返回一个表示当前日期和时间的对象。我们可以将其存储在一个变量中，然后在需要的情况下格式化成我们想要的日期格式。

```Clojure
(def current-date (java.util.Date.))
```

如果我们想要获得一个特定的日期（例如2021年1月1日），我们可以使用`(java.util.Date. 2021 1 1)`的方式来设置日期参数。同样，我们可以将其存储在一个变量中以便后续使用。

```Clojure
(def specific-date (java.util.Date. 2021 1 1))
```

如果我们想要将日期格式化成字符串，我们可以使用Java库中的SimpleDateFormat函数，它可以让我们根据我们想要的格式将日期对象转换为字符串。在下面的代码中，我们将日期格式化为"dd.MM.yyyy"的格式。

```Clojure
(def formatted-date (.format (java.text.SimpleDateFormat. "dd.MM.yyyy") current-date))
```

```Clojure
(def formatted-specific-date (.format (java.text.SimpleDateFormat. "dd.MM.yyyy") specific-date))
```

这会得到"12.12.2021"和"01.01.2021"两个字符串，分别表示当前日期和特定日期。

## 深入：关于获取当前日期的更多信息

获取当前日期看似简单，但实际上涉及到许多概念。在计算机中，时间通常以从某个特定时间（称为“纪元”）开始经过的秒数来表示。而日期则是基于一定的日历规则来计算的。了解这些概念可以帮助我们更好地处理日期和时间。

另外，在计算机系统中，日期和时间并非总是准确的，可能会受到时区、夏令时和计算机时钟本身的影响。因此，在处理日期和时间时，需要小心处理各种边界情况。

## 参考资料

- [Java SimpleDateFormat文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Clojure官方网站](https://clojure.org/)
- [关于日期和时间的计算机概念](https://www.epochconverter.com/)