---
title:                "从字符串中解析日期"
html_title:           "Clojure: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？

解析日期字符串是将字符串转换为日期格式的过程。程序员通常会做这个操作以便于对日期进行比较和计算。

# 如何：

```Clojure
(require '[clojure.java-time :as t])  ; 引入 java-time 命名空间

(def date-str "2021-09-24")  ; 创建日期字符串
(def date (t/parse date-str))  ; 将字符串解析为日期格式
;; 输出：#object[java.time.LocalDate 0x177d6f36 "2021-09-24"]
```

# 深入探讨：

(1) 历史背景：在早期的编程语言中，日期的处理往往是一个挑战，需要手动进行各种操作。而在 Clojure 中，解析日期字符串变得更加简单和有效。

(2) 替代方法：除了使用 java-time 命名空间的解析方法外，也可以使用其他库如clj-time来解析日期字符串。

(3) 实现细节：java-time 命名空间内部使用了 Java 的日期时间类库，通过调用 Java 方法来解析和处理日期字符串。

# 查看也：

- [Clojure Java-Time 命名空间官方文档](https://clojure.github.io/java-time/)
- [clj-time GitHub 页面](https://github.com/clj-time/clj-time)