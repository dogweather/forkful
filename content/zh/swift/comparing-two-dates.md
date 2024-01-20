---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么和为什么？
比较两个日期是检查两个日期值在日历上的相对位置的过程。程序员之所以需要进行这一操作，是因为在处理像日程安排、计时器活动、事件比较等功能时，需要根据时间进行操作或决策。

## 如何做：
在Swift中，我们可以使用`compare()`函数或`==`,`<`,`>`,`<=`,`>=`等比较运算符来比较两个日期。下面是一些示例：

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

let date1 = dateFormatter.date(from: "2020/08/20 13:45")!
let date2 = dateFormatter.date(from: "2020/08/20 15:30")!

if date1.compare(date2) == .orderedAscending {
    print("date1 is earlier than date2")
} else if date1.compare(date2) == .orderedDescending {
    print("date1 is later than date2")
} else {
    print("date1 and date2 are the same")
}
```
如果日期1早于日期2，输出将是："date1 is earlier than date2"。

## 深度剖析
在早期的编程语言中，日期并未被设立为一个特定的数据类型，程序员需要通过字符串或数字、列表等复杂的方式去存储和操作日期。

随后，提出了专门用于日期和时间的数据类型，使得日期比较变得更为方便。Swift在这方面做得尤其出色，为日期比较提供了诸多方便且强大的工具。

除了使用`compare()`函数以外，Swift还提供了`timeIntervalSince(aDate: Date)`方法，可以用来计算两个日期之间的时间间隔。

```Swift
let interval = date1.timeIntervalSince(date2)
print("The interval between date1 and date2 is \(interval) seconds.")
```
你也可以使用`Calendar`来比较两个日期的特定组件，例如年份、月份、天数等。

## 参考文献
- [Apple: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift by Sundell: Working with dates in Swift](https://www.swiftbysundell.com/basics/dates/)