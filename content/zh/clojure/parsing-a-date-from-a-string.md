---
title:                "从字符串解析日期"
date:                  2024-01-20T15:35:18.864296-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
解析日期就是从文本字符串中提取日期信息，转化为程序可以理解的格式。程序员需要这么做来处理和分析日期数据，比如排序事件或验证输入。

## How to: (如何操作：)
Clojure中处理日期的通用库是`clj-time`，但以Java 8开始，我们可以用内置的`java.time`库。看下面的例子：

```Clojure
(require '[java-time :as jt])

;; 将字符串解析成日期对象
(defn parse-date [date-string]
  (jt/local-date date-string))

;; 示例
(println (parse-date "2023-03-30"))
;; 输出: 2023-03-30
```

## Deep Dive (深入了解)
解析日期字符串这个任务从编程早期就存在，原因是计算机和人类使用不同的方式来理解时间。`java.util.Date`曾是Java早期处理日期的方式，但因为设计上的问题，使用起来相当麻烦。Java 8的`java.time`包引入了易于使用的API，它受到Joda-Time库的强烈影响。`clj-time`是一个基于Joda-Time的库，但现在Clojure社区推荐直接使用Java 8的`java.time`。

Clojure的函数式特性使得日期和时间处理可以简洁且不变。记住，处理日期时要考虑时区和本地化。

## See Also (另见)
- [`java.time` documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)