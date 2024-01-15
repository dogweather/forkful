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

## 为什么

当我们需要在程序中展示日期信息时，我们通常会将日期转换成字符串。这样做可以让日期更易读，并且方便我们在不同格式之间切换。

## 如何

在Clojure中，我们可以使用`clojure.java-time`库来实现日期到字符串的转换。首先，我们需要导入该库：

```Clojure
(require '[clojure.java-time :as time])
```

然后，我们可以使用`formatter`函数来定义日期的格式，例如：

```Clojure
(def df (time/formatter "yyyy年MM月dd日"))
```

接下来，我们可以使用`local-date`函数来获取当前日期，并使用`format`函数将其转换成字符串：

```Clojure
(def now (time/local-date))

(time/format df now) ; => "2021年07月10日"
```

如果我们想要指定一个不同的日期，我们可以使用`local-date`函数来创建一个特定的日期对象，并传递给`format`函数：

```Clojure
(def date (time/local-date 2020 12 25))

(time/format df date) ; => "2020年12月25日"
```

## 深入探讨

除了上面提到的日期格式，`clojure.java-time`库还支持许多其他日期格式，例如：

- `yyyy/MM/dd`: "2021/07/10"
- `MM/dd/yyyy`: "07/10/2021"
- `dd-MMM-yyyy`: "10-Jul-2021"
- `dd MMM yyyy`: "10 Jul 2021"

我们也可以使用`formatter`函数来定义特定的时间格式，例如：

```Clojure
(def tf (time/formatter "HH:mm:ss"))

(time/format tf (time/local-time)) ; => "22:42:59"
```

更多关于日期和时间的操作，请参考官方文档：https://github.com/clj-time/clj-time

## 参考

- 官方文档: https://github.com/clj-time/clj-time
- Java时间库参考文档: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html
- Clojure函数文档: https://clojuredocs.org
- Clojure开发者社区: https://clojureverse.org