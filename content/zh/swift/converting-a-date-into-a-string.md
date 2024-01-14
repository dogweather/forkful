---
title:                "Swift: 将日期转换成字符串"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

当我们处理日期时，有时需要将日期数据转换成字符串格式。在Swift编程语言中，这是一个非常常见的操作，因此了解如何进行日期到字符串的转换非常重要。在本文中，我将向大家介绍如何使用Swift编程语言将日期转换成字符串的方法。

## 如何进行日期到字符串的转换

在Swift中，我们可以使用`DateFormatter`类来进行日期到字符串的转换。首先，我们需要创建一个DateFormatter实例，并指定一个日期和时间的格式，例如："yyyy-MM-dd HH:mm:ss"。接下来，我们可以使用`string(from:)`方法来将日期转换成字符串，如下所示：

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let date = Date()
let dateStr = dateFormatter.string(from: date)
print(dateStr)
```

输出结果应该为类似"2021-01-01 12:00:00"的字符串格式。

## 深入了解日期到字符串的转换

在上面的例子中，我们使用的是"yyyy-MM-dd HH:mm:ss"格式来将日期转换成字符串，但实际上还有许多其他可用的日期和时间格式，例如："EEE, dd MMM yyyy HH:mm:ss Z"或"MMM d, yyyy hh:mm a"。您可以根据您的需要来选择合适的格式。另外，您也可以通过设置`locale`来指定使用哪种地区的格式。

除了将日期转换成字符串，我们也可以使用`date(from:)`方法来将字符串转换成日期数据。同时，您也可以通过`calendar`和`timeZone`属性来设置不同的日历和时区。

## 参考链接

- [Swift 文档 - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [如何将日期转换成字符串？](https://www.hackingwithswift.com/example-code/system/how-to-convert-a-date-to-a-string-in-swift)
- [Swift Date and Time Formatting Cheatsheet](https://nsomar.com/swift-date-cheatsheet/) 

## 参见

- [Swift 入门指南](https://www.swift.com.cn/)
- [Swift 日期和时间操作教程](https://www.jianshu.com/p/b69b444e1a07)