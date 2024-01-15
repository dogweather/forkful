---
title:                "比较两个日期"
html_title:           "Swift: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么比较两个日期

比较日期在编程中是一项非常常见的任务，它可以帮助我们判断出在程序中的两个事件发生的时间先后顺序，从而影响程序的逻辑。对于Swift语言来说，比较日期也是一个必备技能，让我们来看看如何做到这一点吧。

## 如何实现

比较两个日期可以通过Swift中的`Date`类型和`compare(_:)`方法来完成。我们先定义两个日期变量，然后使用`compare(_:)`方法来比较它们，该方法将返回一个`ComparisonResult`枚举值，我们可以根据这个值来判断两个日期的先后顺序。

```Swift
//定义两个日期变量
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) //设置一个相差1小时的日期

//比较两个日期并得到结果
let result = date1.compare(date2)

//通过switch语句判断
switch result {
  case .orderedAscending:
    print("date1 早于 date2")
  case .orderedDescending:
    print("date1 晚于 date2")
  case .orderedSame:
    print("两个日期相同")
}
```

这段代码的输出结果将会是`date1 早于 date2`，因为我们事先设置了`date2`比`date1`晚1小时。

## 深入了解

Swift中的`Date`类型实际上是一个`TimeInterval`（时间间隔）的别名，指的是从1970年1月1日到指定日期的时间间隔。所以在比较两个日期时，实际上是在比较两个时间间隔的大小。`compare(_:)`方法的返回值是一个`ComparisonResult`枚举类型，它有三个值：`.orderedAscending`、`.orderedDescending`和`.orderedSame`，分别代表前面日期早于、晚于和相同于后面日期。

# 查看也许感兴趣的

- [Apple官方文档 - Comparing Dates](https://developer.apple.com/documentation/foundation/date)
- [Swift中的Date类型详解](https://www.hangge.com/blog/cache/detail_1594.html)
- [如何使用Swift实现日期比较](https://www.jianshu.com/p/5557e91ff687)