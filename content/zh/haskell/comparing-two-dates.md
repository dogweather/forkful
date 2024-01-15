---
title:                "比较两个日期"
html_title:           "Haskell: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

为什么：为什么要比较两个日期？可能是因为我们需要知道哪个日期更早或者更晚，或者我们需要按照日期对数据进行排序。

如何：
比较两个日期是一个很简单的任务，Haskell提供了一种便捷的方法来完成它。我们可以使用`diffDays`函数来计算两个日期之间的天数差异，然后根据返回的结果进行条件判断。下面是一个简单的例子：

```Haskell
import Data.Time

-- 定义两个日期
date1 = fromGregorian 2021 8 1
date2 = fromGregorian 2021 8 10

-- 比较两个日期并打印结果
main = print $ diffDays date1 date2
```

运行以上代码，我们可以得到`9`作为输出结果，表示`date1`比`date2`早9天。

深入了解：
除了比较两个日期的天数差异，我们也可以通过其他函数来进行更多的比较。例如，`diffJulianDuration`函数可以计算两个日期之间的Julian时间间隔，`compare`函数可以比较两个日期之间的大小关系。此外，Haskell还提供了一系列的日期处理函数，例如`addDays`和`addGregorianMonthsClip`等，可以让我们更灵活地操作日期。

见下文：

- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Haskell Data.Time模块文档](https://hackage.haskell.org/package/time-1.10.0.3/docs/Data-Time.html)