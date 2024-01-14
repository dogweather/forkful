---
title:                "Swift: 比较两个日期"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么：为什么比较日期是重要的

日期是我们日常生活中非常重要的一部分。在程序开发中，我们经常需要比较日期，例如在制作日历应用程序或日期提醒功能时。比较两个日期可以帮助我们更好地理解和处理日期数据，从而使我们的程序更加准确和高效。

## 如何：比较两个日期的代码示例和输出

比较两个日期的方法有很多种，下面的代码示例将展示其中三种常用的方法。假设我们有两个日期变量，date1和date2。

### 使用“>”和“<”来比较日期

```
Swift
if date1 > date2 {
    print("date1 比 date2 晚")
}
else if date1 < date2 {
    print("date1 比 date2 早")
}
else {
    print("两个日期相同")
}
```

输出：

```
date1 比 date2 晚
```

### 使用Compare方法来比较日期

```
Swift
if date1.compare(date2) == ComparisonResult.orderedDescending {
    print("date1 比 date2 晚")
}
else if date1.compare(date2) == ComparisonResult.orderedAscending {
    print("date1 比 date2 早")
}
else {
    print("两个日期相同")
}
```

输出：

```
date1 比 date2 晚
```

### 格式化日期再比较

```
Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let formattedDate1 = dateFormatter.string(from: date1)
let formattedDate2 = dateFormatter.string(from: date2)

if formattedDate1 > formattedDate2 {
    print("date1 比 date2 晚")
}
else if formattedDate1 < formattedDate2 {
    print("date1 比 date2 早")
}
else {
    print("两个日期相同")
}
```

输出：

```
date1 比 date2 晚
```

## 深入了解：比较两个日期的更多知识

在比较两个日期时，还有一些细节需要注意。

### 时区和日历的影响

比较日期时，时区和日历会影响到比较结果。如果两个日期处于不同的时区，或者使用不同的日历格式，比较结果可能会不准确。因此，在进行日期比较前，需要先确认两个日期的时区和日历格式是否相同。

### 日期格式化后的字符串比较

在上面的代码示例中，我们使用了日期格式化器来将日期格式化为字符串再进行比较。然而，这种方法并不是最佳实践，因为字符串的比较可能会产生一些意外的结果。例如，"2019-08-01"和"2019-8-01"这两个字符串在比较时，可能会得到不同的结果。所以，最好是直接比较日期对象本身，而不是将其格式化为字符串。

## 参考链接

- [比较日期的官方文档](https://developer.apple.com/documentation/foundation/date)
- [Swift 日期编程指南](https://www.raywenderlich.com/3619-swift-4-2-如何处理日期)
- [时区和日历的影响](https://www.raywenderlich.com/790-swift-日期时区和日历)
- [日期格式化与字符串比较](https://stackoverflow.com/questions/31921762/swift-string比较出现问题)

## 参见：其他有用的日期处理方法

- `DateFormatter`- 格式化日期的工具类
- `Calendar`- 处理日历相关的操作
- `DateComponents`- 用于表示日期组成部分的类
- `NSCalendarUnit`- 预定义的日历组件