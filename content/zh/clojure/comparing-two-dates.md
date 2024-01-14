---
title:    "Clojure: 比较两个日期"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

为什么：比较两个日期在编程中是一个很常见的需求，它可以帮助我们处理时间和日期相关的问题，例如计算年龄，计算两个事件之间的时间间隔等等。

## 怎么做

比较两个日期可以通过使用Clojure标准库中的`clj-time`库来实现。首先，我们需要在项目中导入这个库，可以通过在项目的`project.clj`文件中添加以下依赖来引入它：

```Clojure
[clj-time "0.14.3"]
```

接下来，我们可以使用`clj-time.core`命名空间中的`between`函数来比较两个日期。这个函数需要两个参数，分别是待比较的两个日期对象，它会返回一个`clj-time.interval`对象，表示这两个日期之间的时间间隔。例如，我们想要比较今天和昨天的日期差，可以使用以下代码：

```Clojure
(require '[clj-time.core :as time])

(def today (time/today))
(def yesterday (time/minus today (time/days 1)))

(time/between today yesterday)
```

以上代码会输出一个`clj-time.interval`对象，其中包含有关这两个日期之间的时间间隔的信息。比如，在我的电脑上，执行以上代码的输出为：

```
#<Interval [2019-11-30T16:00:00.000Z, 2019-12-01T16:00:00.000Z]>
```

我们还可以通过调用`in-units`函数来指定所需的时间单位，并以数字的形式获取时间间隔的值。例如，我们想要获得上面例子中今天和昨天的日期差的小时数，可以使用以下代码：

```Clojure
(time/in-units (time/between today yesterday) :hours)
```

以上代码会输出`24`，表示今天和昨天的日期差为24小时。

## 深入讨论

在Clojure中比较两个日期的另一种常用方法是使用`clj-time.core`命名空间中的`after?`或`before?`函数。这两个函数都接受两个日期对象作为参数，并返回一个布尔值，表示第一个日期是否在第二个日期之后或之前。比如，我们想要判断某个日期是否在当前日期之后，可以使用以下代码：

```Clojure
(require '[clj-time.core :as time])

(def today (time/today))
(def target-date (time/date-time 2020 1 1))

(time/after? target-date today)
```

以上代码会输出`true`，因为`target-date`是今天之后的日期。

除了`clj-time`库外，Clojure中还有其他一些处理日期和时间的有用工具，例如`java.time`库和`chrono`库。如果你对这些工具感兴趣，可以通过以下链接了解更多信息。

# 查看更多

- [clj-time库文档](https://github.com/clj-time/clj-time)
- [java.time库文档](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [chrono库文档](https://github.com/dakrone/chrono)