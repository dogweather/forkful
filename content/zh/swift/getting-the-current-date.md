---
title:                "Swift: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在编写Swift程序时，经常会需要获取当前日期。这对于许多应用和功能都非常重要，例如日历、倒计时、提醒事项等等。因此，了解如何在Swift中获取当前日期是十分有用的。

## 如何

首先，我们需要使用Swift内置的Date类来获取当前日期。下面是一个简单的示例代码：

```Swift
let currentDate = Date()
print(currentDate)
```

运行这段代码，你将会得到如下的输出：

```
2019-09-01 12:00:00 +0000
```

通过使用Date类，我们可以获取当前日期和时间的信息。不过通常情况下，我们只需要日期，不需要时间。因此，我们可以使用Swift中的Calendar类来实现这一点。下面是一个具体的例子：

```Swift
let currentDate = Date()
let calendar = Calendar.current
let dateComponents = calendar.dateComponents([.year, .month, .day], from: currentDate)
print(dateComponents)
```

运行上述代码，你将会得到以下输出：

```
year: 2019
month: 9
day: 1
```

通过使用Calendar类和dateComponents方法，我们可以只获取当前日期的年、月、日信息。

## 深入学习

如果你想更深入地了解如何在Swift中获取当前日期，可以学习一下Date和Calendar类的更多属性和方法。例如，你可以使用DateFormatter类来自定义日期的格式，如将日期输出为"yyyy-MM-dd"的形式。此外，你还可以使用Date和Calendar类来进行日期之间的计算，如计算两个日期之间的天数差。

## 参考资料

- [Swift官方文档](https://developer.apple.com/documentation/swift)
- [Swift之旅](https://www.cnswift.org/)
- [使用Date来计算日期之间的差值](https://www.hackingwithswift.com/example-code/system/how-to-calculate-the-difference-between-two-dates)

## 相关阅读