---
title:                "获取当前日期"
html_title:           "Clojure: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在日常编程中，获取当前日期是一项必不可少的任务。它可以帮助我们跟踪事件，计算持续时间等等。Clojure语言提供了简单而强大的方法来获取当前日期，让我们来看看如何实现吧！

## 如何

首先，让我们导入Clojure中的日期库：

```Clojure
(require '[clojure.java-time :as jtime])
```

接下来，我们可以使用`now`函数来获取当前日期和时间：

```Clojure
(jtime/now)
```

这将返回包含当前日期和时间的`Instant`对象。我们可以通过调用`toLocalDate`来获取当前日期：

```Clojure
(jtime/toLocalDate (jtime/now))
```

该函数将返回一个`LocalDate`对象，其中包含当前日期的年份，月份和日数。我们还可以使用`toLocalDateTime`函数来获取当前日期和时间的更多细节：

```Clojure
(jtime/toLocalDateTime (jtime/now))
```

这将返回一个`LocalDateTime`对象，其中包含当前日期的年份，月份，日数，小时，分钟和秒数。现在，让我们来看看如何格式化当前日期的输出。

要以特定的格式输出当前日期，我们可以使用`format`函数，并指定所需的格式。例如，要以`yyyy-MM-dd`格式输出当前日期，我们可以这样做：

```Clojure
(jtime/format (jtime/now) "yyyy-MM-dd")
```

这将返回一个字符串，其中包含当前日期的年份，月份和日数，使用连字符分隔。除此之外，您还可以指定其他日期格式，例如`MM/dd/yyyy`，`dd MMM yyyy`等等。现在，让我们来深入探讨一下如何获取当前日期的更多信息。

## 深入了解

在获取当前日期时，Clojure使用的是Java类库中的日期和时间类。这些类提供了各种方法来处理和管理日期，例如计算持续时间，比较日期，转换日期等等。您可以通过查看Java类库文档来了解更多关于这些类和其相关方法的信息。另外，Clojure还提供了许多其他日期操作的函数，例如`plus-days`，`minus-days`等等，使您能够方便地对日期进行修改和计算。现在，您已经了解了如何获取当前日期，让我们来看看一些相关的链接吧。

## 参考资料

- Java 日期和时间类库文档: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Clojure 日期与时间函数文档: https://clojure.github.io/java-time/
- Clojurians论坛: https://ask.clojure.org/
- Clojure标准库: https://clojuredocs.org/