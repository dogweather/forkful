---
title:                "计算未来或过去的日期"
html_title:           "Swift: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在过去或未来中计算日期是获取和操作日期的常见编程任务。程序员做这个是为了处理日常问题，例如计算未来的事件日期或分析历史数据。

## 如何:

Swift 提供了 `DateComponents` 和 `Calendar` 类，有时又称对象，能够方便地计算日期。下面是一些例子：

``` Swift
import Foundation

//创建一个当前日期
let now = Date()

//创建一个Calendar类
let calendar = Calendar.current

//构建一个将来的日期
if let futureDate = calendar.date(byAdding: .day, value: 5, to: now) {
    print(futureDate)
}
//输出例子: 2023-06-15 08:42:36 +0000

//构建一个过去的日期
if let pastDate = calendar.date(byAdding: .month, value: -3, to: now) {
    print(pastDate)
}
//输出例子: 2023-03-10 08:42:36 +0000
```

## 深挖:

Swift 内的日期处理功能借鉴了历史上诸多编程语言的优点。早期的语言，如 C 和 C++，提供了有限的日期处理功能，需要程序员自己编写复杂的日期运算代码。Swift 在这方面得以进步，因为它具有高级的日期和时间处理功能，但不失简洁性。

选择 Swift 的替代方案可能会取决于多种因素，包括应用的特性、开发团队的技术栈、资源和时间限制等。Python、JavaScript 和 Java 都有强大的日期和时间处理库。

在 Swift 中，`DateComponents` 和 `Calendar` 类背后的实现都取决于你的操作系统和其对应的库。在 iOS 和 macOS 上，它们都使用了 Apple 的 Foundation 框架。

## 另见：

关于 Swift 日期和时间处理的更多信息，可参考以下资源：

- Apple 官方文档: [DateComponents](https://developer.apple.com/documentation/foundation/datecomponents) 和 [Calendar](https://developer.apple.com/documentation/foundation/calendar)
- 格式化 Swift 
日期的指南: [DateFormatter](https://www.hackingwithswift.com/articles/141/8-useful-swift-extensions)
- Stack Overflow 中的相關問答: [How to add days to the current date in swift](https://stackoverflow.com/questions/27310883/how-to-add-days-to-the-current-date-in-swift)