---
title:    "Clojure: 获取当前日期。"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

获取当前日期，对于编程来说是一个常见的需求。它可以用于记录程序运行的时间，或者作为特定功能的触发条件。

## 如何操作

使用Clojure编程语言，我们可以通过调用内置函数`now`来获取当前日期。下面是一个简单的示例代码：

```Clojure
(import '(java.time LocalDate))

(def today (LocalDate/now))
(println today)
```

运行上述代码，我们将得到如下输出：

`2021-07-08`

## 深入了解

值得注意的是，`now`函数会返回一个`LocalDateTime`对象，它包含了日期和时间的信息。如果只需要日期信息，我们可以使用`today`函数来获取当前日期的`LocalDate`对象。除了`now`和`today`函数，Clojure还提供了许多其他的日期相关函数，如`plus-days`和`minus-months`，它们可以帮助我们进行日期的加减操作。

## 参考资料

- [Clojure日期与时间库官方文档](https://clojure.org/api/cheatsheet)
- [Java.time文档](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)

# 请参考

- [Clojure中文网站](https://clojure.org.cn/)
- [Clojure语言学习资源推荐](https://clojure.org.cn/learning/resources.html)
- [Clojure社区论坛](https://clojure.org.cn/forum/)