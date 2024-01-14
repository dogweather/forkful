---
title:    "Clojure: 获取当前日期"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么使用Clojure获取当前日期？

获取当前日期是编写任何程序时都可能需要的基本任务。Clojure是一种功能强大的编程语言，它可以轻松地帮助您获取当前日期。在本文中，我们将探讨如何在Clojure中获取当前日期，以及它为什么如此重要。

## 如何在Clojure中获取当前日期

首先，让我们看一下使用Clojure内置的`(clojure.java-time/local-date)`函数来获取当前日期的方法：

```Clojure
(let [now (clojure.java-time/local-date)]
  (println now))

;; 输出样例： 2021-02-03
```

此外，您还可以使用Clojure的`(println)`函数将日期格式化为您喜欢的方式：

```Clojure
(let [now (clojure.java-time/local-date)]
  (println (.format now (java.text.SimpleDateFormat. "dd/MM/yyyy"))))

;; 输出样例： 03/02/2021
```

## 深入探讨如何获取当前日期

在Clojure中获取当前日期涉及到的关键是了解Java在日期处理方面的强大功能。通过深入学习Java的日期处理类，您可以进一步加强您在Clojure中获取日期的能力。

例如，我们可以使用`(java.util.Date)`类来创建一个特定日期的实例，并将其转换为Clojure map来更加灵活地操作：

```Clojure
(def specific-date (-> "2021/01/01" (java.util.Date.)))
(let [specific-date-map (clojure.java-time/as-date specific-date)]
  (println specific-date-map))

;; 输出样例： {:instant #inst "2021-01-01T00:00:00.000-00:00"}
```

## 参考资料

* [Clojure官方文档：java-time.namespace](https://clojure.github.io/java-time/)
* [Clojure高阶日期处理教程](https://gist.github.com/bfontaine/651ee3e6d18d3cebc86f)
* [Java官方文档：Date类](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)

### 查看更多

* [Clojure China社区](https://clojure-china.org/)
* [ClojureScript：编写现代的、声明性的、高性能代码](https://clojurescript.org/)
* [Clojure教程（简体中文版）](https://wizardforcel.gitbooks.io/clojure-tutorial/content/)