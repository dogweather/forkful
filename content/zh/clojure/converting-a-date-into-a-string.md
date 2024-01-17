---
title:                "将日期转换为字符串"
html_title:           "Clojure: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
转换日期为字符串是将日期对象转换成可读性高的字符串的过程。这在编程中非常常见，因为它让程序员能够更方便地处理和显示日期。例如，你可能想要将一个日期对象转换成"2020年5月1日"这样的格式来显示给用户。

## 如何:
```Clojure
(require '[clojure.java-time :as t])
(t/format (t/local-date 2020 5 1) "yyyy年M月d日")
```

输出结果: "2020年5月1日"

## 深入了解:
日期转换为字符串在计算机领域已经有很长的历史。以前，程序员必须自己编写代码来处理日期对象和字符串之间的转换。但是现在，在Clojure中我们可以使用clojure.java-time库来更简单地实现这一过程。另外，除了使用format函数，我们也可以使用其他函数来获取不同格式的日期字符串，例如t/local-date-time，t/local-date-time，等等。

## 参考资料:
1. [clojure.java-time库官方文档](https://cljdoc.org/d/java-time/java-time/0.2.0/doc/readme)
2. [文档中有关日期转换的更多例子](https://cljdoc.org/d/java-time/java-time/0.2.0/doc/examples)
3. [clojure.java-time源代码](https://github.com/dm3/clojure.java-time)