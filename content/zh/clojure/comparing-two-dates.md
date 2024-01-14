---
title:    "Clojure: 比较两个日期"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期

比较两个日期是编程中经常会遇到的问题，它可以帮助我们判断时间先后顺序，做出相应的逻辑处理。比如在开发一个日程管理应用时，我们就需要比较日期来确定某个事件是在今天、明天还是过去的日期。在Clojure中有很多方法可以比较日期，接下来我们就来看一下如何实现。

## 如何比较日期

我们可以使用Clojure的`<`, `<=`, `>`, `>=`等比较运算符来比较两个日期。

```Clojure
(def today (java.util.Date.)) ;; 获取当前日期
(def tomorrow (java.util.Date. (DateTime/plusDays 1))) ;; 获取明天的日期

;; 判断日期是否相等
(= today tomorrow) ;; false

;; 判断日期是否小于
(< today tomorrow) ;; true

;; 判断日期是否大于等于
(>= tomorrow today) ;; true
```

我们也可以使用Clojure提供的`clj-time`库来更方便地比较日期。该库已经被Clojure社区广泛使用，并且提供了许多实用的日期操作函数。

```Clojure
(require '[clj-time.core :as t])

(def today (t/today)) ;; 获取当前日期
(def tomorrow (t/tomorrow)) ;; 获取明天的日期

;; 判断日期是否相等
(t/equal? today tomorrow) ;; false

;; 判断日期是否小于
(t/before? today tomorrow) ;; true

;; 判断日期是否大于等于
(t/after? tomorrow today) ;; true
```

## 深入了解比较两个日期

在Clojure中，日期被表示为`java.util.Date`类型的对象。这些对象是可变的，因此我们在比较日期时需要注意，避免修改原始日期对象。另外，Clojure并不直接支持日期的加减运算，我们需要通过`clj-time`库中的函数来实现。

此外，Clojure还提供了另一个日期处理库`java-time`，它被认为是`java.util.Date`的替代品，提供了更多功能和更符合现代标准的API。因此，在进行日期比较时，我们也可以考虑使用`java-time`库来替代`java.util.Date`。

## 参考链接

- [Clojure官方文档](https://clojure.org/)
- [clj-time库文档](https://github.com/clj-time/clj-time)
- [java-time库文档](https://github.com/jsa-aerial/java-time)