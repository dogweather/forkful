---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么与为什么?

获取当前日期是在程序中从系统中获取当前日期和时间的过程。程序员由此可以记录事件发生的时间，进行时间比较，或在指定时间后执行操作。

## 如何操作:

```Clojure
; 导入Java的date类
(import 'java.util.Date)

(println (Date.))
```

示例输出:

```Clojure
; 输出以服务器的当前日期和时间
Tue Mar 24 15:43:09 CST 2021
```
## 深入了解

**历史背景:** 从计算机的早期开始，日期和时间就在计算中起着重要的作用。这在事件日志记录、调度程序操作和监控活动趋势等功能中具有重大意义。

**替代方案:** 除了上述例子中提到的 `java.util.Date` 类, Clojure 同样支持 `java.time` 类库，这是 Java 8 引入的一套更为现代和全面的时间日期库。此外，对于更复杂的日期时间操作，你还可以使用像 `clj-time` 这样的库。

**实施细节:** 在 Clojure 中，你可以使用 Java 类的方式来获取日期和时间，然后使用各种内置函数进行操作。你也可以使用类库来扩展对日期和时间的操作。

## 另请参阅

- `java.util.Date` 类的详细用法请参考： [Java Date Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- `java.time` 类的详细用法请参考： [Java Time Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- `clj-time` 类库的使用说明请参考： [clj-time GitHub](https://github.com/clj-time/clj-time)