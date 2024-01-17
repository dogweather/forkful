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

# 什么以及为什么

获取当前日期是指在程序中获取当前的日期信息。程序员通常会使用当前日期来记录程序运行时间、显示用户注册日期或者产生随机数。这是一个简单但重要的功能，让我们来看看如何在Clojure中进行实现。

# 如何：

```Clojure
(import java.util.Date)

(defn get-current-date []
  "获取当前日期"
  (Date.))     ;; 使用Date构造函数来创建当前日期对象

(def current-date (get-current-date)) 
  ;; 调用函数来获取当前日期并将结果赋值给变量current-date 

(println current-date)  
  ;; 输出结果：#inst"2021-10-27T09:26:15.762-00:00"
```

# 深入了解

在早期的编程语言中，获取当前日期是一个相当复杂的过程，需要使用许多不同的函数和库来实现。但是，Clojure提供了一个简单的方法来获取当前日期，仅需一行代码即可。另外，除了使用Date构造函数，我们还可以使用Clojure的内置函数```(clojure.java-time/local-date)```来获取当前日期。此外，值得一提的是，Clojure中的日期对象也可以进行比较，这在一些特定的程序需求中非常有用。

# 查看更多

- [官方文档中的日期时间处理](https://clojure.org/guides/date_time)
- [如何获取当前日期及其它日期操作的用法](https://wisdomjobs.com/e-university/clojure-tutorial-235/date-time-classes.html)