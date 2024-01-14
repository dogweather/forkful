---
title:    "Swift: 比较两个日期"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期

许多时候，我们需要在 Swift 编程中比较两个日期。可能是为了检查事件发生的顺序，或者是为了计算两个日期之间的时间间隔。无论是什么原因，比较两个日期是一项基本的任务，它可以帮助我们更好地理解程序中的时间信息。

# 如何比较两个日期

在 Swift 中，我们可以使用`Date`和`Calendar`两个类来比较两个日期。首先，我们需要创建两个日期对象，然后使用`compare`方法来比较它们。下面是一个简单的例子：

```
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

let date1 = dateFormatter.date(from: "2021-01-01")
let date2 = dateFormatter.date(from: "2021-01-02")

if let date1 = date1, let date2 = date2 {
    let result = date1.compare(date2)
    switch result {
    case .orderedAscending:
        print("\(date1) 早于 \(date2)")
    case .orderedDescending:
        print("\(date1) 晚于 \(date2)")
    case .orderedSame:
        print("两个日期相同")
    }
}
```
输出：

```
2021-01-01 早于 2021-01-02
```

除了比较日期的顺序，我们也可以使用`Calendar`类的`dateComponents`方法来计算两个日期之间的时间间隔。对于具体的时间单位，我们可以使用`Calendar.Component`枚举来指定。下面是一个简单的例子：

```
let calendar = Calendar.current

let date1 = calendar.date(from: DateComponents(year: 2021, month: 1, day: 1))
let date2 = calendar.date(from: DateComponents(year: 2021, month: 1, day: 2))

if let date1 = date1, let date2 = date2 {
    let days = calendar.dateComponents([.day], from: date1, to: date2).day
    print("两个日期相差\(days!)天")
}
```
输出：

```
两个日期相差1天
```

# 深入了解比较两个日期

在比较日期时，我们也需要考虑时区和日历的影响。Swift 中的`Date`类表示的是一个绝对的时间点，而不是一个特定的时刻。因此，我们必须指定正确的时区和日历来比较日期。另外，在比较日期时，也需要考虑闰年的影响。

# 请参阅

- [Swift 官方文档 - Date](https://developer.apple.com/documentation/foundation/date)
- [Swift 官方文档 - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift 官方文档 - DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)