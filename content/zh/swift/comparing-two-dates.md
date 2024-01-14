---
title:    "Swift: 比较两个日期"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 为什么比较日期？

在Swift编程中，有时候我们需要比较日期，来判断两个日期前后先后顺序或者计算时间间隔。比如，我们可能需要在日历应用中显示提醒事件，根据提醒时间来排序。因此，比较日期是非常常见的任务。

## 如何比较日期

比较日期最简单的方法就是使用 `compare()` 方法，它可以将两个日期进行比较，并返回一个 `ComparisonResult` 枚举类型的值。这个枚举类型有三个可能的值：`.orderedAscending` 、 `.orderedSame` 和 `.orderedDescending` 。下面是比较两个日期并打印结果的示例代码：

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date1 = dateFormatter.date(from: "2021-01-01")
let date2 = dateFormatter.date(from: "2021-03-10")
let result = date1?.compare(date2!)
print(result) // 输出：Optional(ComparisonResult.orderedAscending)
```

在这个示例中，我们使用 `DateFormatter` 来将字符串转换为日期，并将日期进行比较。如果第一个日期在第二个日期之前，就会返回 `.orderedAscending` ，即升序排列。如果两个日期相等，则返回 `.orderedSame` 。如果第一个日期在第二个日期之后，则返回 `.orderedDescending` ，即降序排列。

我们也可以使用 `<` 、 `<=` 、 `>` 和 `>=` 操作符来直接比较日期。比如，我们可以这样写：

```Swift
print(date1! < date2!) // 输出：true
```

这些操作符都会返回一个 `Bool` 类型的值，表示两个日期的先后顺序。

另外，我们还可以使用 `Calendar` 类来比较日期。`Calendar` 类提供了一系列计算时间间隔的方法。比如，我们可以使用 `dateComponents(_:from:to:)` 方法来计算两个日期之间的差值：

```Swift
let calendar = Calendar.current
let components = calendar.dateComponents([.day], from: date1!, to: date2!)
print(components.day!) // 输出：68
```

这个方法会返回一个 `DateComponents` 对象，我们可以从中获取我们需要的时间间隔，比如 `day` 、 `hour` 、 `minute` 等等。

## 深入探讨比较日期

在深入探讨比较日期之前，首先让我们了解一下日期在计算机中是如何表示的。在Swift中，日期是通过 `Date` 结构体来表示的，它其实就是一个时间戳，即距离1970年1月1日00:00:00的秒数。因此，比较日期其实就是比较这个时间戳的大小。

另外，我们也可以使用 `Calendar` 类的 `isDate(_:equalTo:toGranularity:)` 方法来比较两个日期的某个精度内是否相等。这个方法可以比较年、月、日、时、分、秒等不同的精度。比如：

```Swift
print(calendar.isDate(date1!, equalTo: date2!, toGranularity: .month)) // 输出：true
```

除了比较日期，我们也可以将日期格式化成指定的字符串，比如将上面的日期格式化为中文格式：

```Swift
dateFormatter.dateFormat = "yyyy年MM月dd日"
let dateStr = dateFormatter.string(from: date1!)
print(dateStr) // 输出：2021年01月01日
```

## 参考链接

- [Apple官方文档：Date](https://developer.apple.com/documentation/foundation/date)
- [Apple官方文档：Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift编程语言指南：Comparison Operators](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID34)
- [Swift编程语言指南：Calendars and Dates](https://