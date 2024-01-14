---
title:                "Clojure: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期？

获取当前日期在Clojure编程中是非常常见的操作。通过获取当前日期，我们可以在程序中实现时间相关的功能，比如记录数据更新的时间，或者定时执行某些任务等。 

## 如何实现？

在Clojure中，有多种方法可以获取当前日期。下面将通过代码示例来介绍其中的两种方法。

### 方法一：使用 `clj-time` 库

首先，我们需要导入 `clj-time` 库：
```Clojure
(ns my-namespace
  (:require [clj-time.core :refer [now]]))
```

然后，就可以使用 `now` 函数来获取当前日期对象：
```Clojure
(def current-date (now))
```

最后，我们可以使用 `clj-time` 提供的各种函数来对日期对象进行操作，比如获取年、月、日等信息，或者进行日期的加减运算。

### 方法二：使用 `java.time` 库

`java.time` 是 Java 8 中引入的日期和时间 API，我们可以通过Clojure与Java的互操作性来使用它。

首先，需要导入 `java.time.LocalDate` 类：
```Clojure
(import java.time.LocalDate)
```

然后，我们可以直接调用 `LocalDate` 的静态方法来获取当前日期对象：
```Clojure
(def current-date (LocalDate/now))
```

最后，我们可以使用 `LocalDate` 提供的各种方法来对日期对象进行操作，比如获取年、月、日等信息，或者进行日期的加减运算。

## 深入探讨

无论是使用 `clj-time` 还是 `java.time`，都是通过调用Java类的静态方法来获取当前日期。在Java中，可以通过 `Clock` 类来获取当前时间，而这也是 `clj-time` 库的内部实现。另外，值得一提的是，在Clojure中，我们也可以自己实现一个 `now` 函数来获取当前日期，比如通过调用 `System/currentTimeMillis` 方法来返回当前毫秒数，再通过 `java.time.Instant` 类来将毫秒数转换为日期对象。不管使用哪种方法，最终都是在获取系统时间并做适当的处理来得到当前日期。

# 参考链接

- [clj-time 文档](https://cljdoc.org/d/clj-time/clj-time/0.14.4/doc/readme)
- [java.time 文档](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)