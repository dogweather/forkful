---
title:                "获取当前日期"
date:                  2024-01-20T15:14:13.918507-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

category:             "Clojure"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
获取当前日期是读取计算机系统日期的过程。程序员这么做通常是为了记录事件、生成时间戳或展示给用户。

## How to: 如何操作
Clojure 使用 `java.util.Date` 类和 `clj-time` 库来处理日期和时间。下面是获取当前日期的代码示例：

```Clojure
(import 'java.util.Date)

(defn get-current-date []
  (let [today (Date.)]
    (println "今天的日期是：" today)))

(get-current-date)
```

运行代码，你可能看到类似这样的输出：

```
今天的日期是：Thu Mar 31 15:20:45 CST 2022
```

如果要更优雅地处理日期和时间，可以使用 `clj-time` 库：

```Clojure
(require '[clj-time.core :as time])
(require '[clj-time.format :as fmt])

(defn get-current-date-clj-time []
  (let [formatter (fmt/formatters :basic-date-time)
        now (time/now)]
    (println "现在的日期和时间是：" (fmt/unparse formatter now))))

(get-current-date-clj-time)
```

输出将是这样的：

```
现在的日期和时间是：20220331T152045.000Z
```

## Deep Dive 深入了解
在过去，Clojure 开发者常用 `java.util.Date`，但这个类不太灵活也不易于使用。`clj-time` 库基于 Joda-Time，带来更好的日期和时间处理实践。

另外，Java 8 引入了新的日期和时间 API，例如 `java.time.*`，提供了新的类比如 `LocalDate` 和 `LocalDateTime`。

这些新的类方法优于老旧的 `java.util.Date`，提供更强的时区处理，更直观的 API，并且都是不可变的（immutable），这使得它们更安全，尤其是在并发环境中。

Clojure 作为一个运行在 JVM 上的语言，可以直接利用这些改进。如今，推荐使用 `java.time` 或通过 `clj-time` 库间接使用 Joda-Time 的 Clojure 封装。

## See Also 参考链接
- Clojure 官方网站：[https://clojure.org](https://clojure.org)
- clj-time GitHub 仓库：[https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Joda-Time 官方网站：[https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- Java 8 Date/Time API 文档：[https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
