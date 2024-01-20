---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么与为什么？

获取当前日期是在程序中获取并使用当前的日期和时间。程序员用它来记录事件，标记时间戳，或对时间敏感的数据进行排序和管理。

## 如何做：

下面我们来介绍如何在Swift中获取当前日期：

```Swift 
import Foundation

let currentDate = Date()
print("当前日期和时间：\(currentDate)")
```

执行代码，你将看到如下输出：

```Swift
// "当前日期和时间：2022-09-13 12:23:34 +0000"
```

这就是当前的日期和时间，按照“年-月-日 时:分:秒 时区”格式显示。

## 深入探讨

从历史角度来看，获取日期和时间在各种编程语言中都有一定的实现方案。像早期的C语言就提供了`time.h`库来获取日期和时间。

在Swift中，还有其他一些获取日期的方式。例如你可以使用 `Calendar.current.dateComponents()` 获取更具体的信息，像年、月、日、小时、分钟等。

```Swift
let components = Calendar.current.dateComponents([.year, .month, .day, .hour, .minute, .second], from: Date())
print("当前年份：\(components.year!)")
print("当前月份：\(components.month!)")
print("当前日期：\(components.day!)")
print("当前时间： \(components.hour!):\(components.minute!):\(components.second!)")
```

而在Swift背后的实现原理，是基于Unix Epoch时间（1970年1月1日开始的秒数）来计算出当前的日期和时间。

## 参看：

为了更多关于Swift的日期和时间操作，可以参看以下链接：

- Apple官方文档关于Date的介绍: [Apple Date Documentation](https://developer.apple.com/documentation/foundation/date)
- Swift日期和时间详解: [Working with Dates and Times in Swift](https://www.swiftbysundell.com/basics/dates-and-times/)
- Swift时间操作库: [SwiftDate](https://github.com/malcommac/SwiftDate)