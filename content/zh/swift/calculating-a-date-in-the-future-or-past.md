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

## 为什么
如果你正在为一个未来或过去的日期进行计算，你可能是要编写一个日历应用程序、计划旅行安排或者只是想知道几天后的某个特殊日子是星期几。

## 如何进行计算
```swift
// 获取当前日期
let today = Date()
// 创建一个日期计算器
let dateCalculator = DateComponents()
// 将日期计算器的组合设置为所需的日期差异
dateCalculator.day = 7 // 计算一周后的日期，可根据需要更改
// 获取计算后的日期
let futureDate = Calendar.current.date(byAdding: dateCalculator, to: today)
// 定义日期格式
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "YYYY年MM月dd日"
// 显示计算后的日期
print(dateFormatter.string(from: futureDate!)) // 示例输出: 2021年08月04日
```

## 深入学习
日期计算可以使用`Date`, `DateComponents`和`Calendar`三个类来实现。可以通过设置`DateComponents`类的属性来定义日期之间的差异，然后使用`Calendar`类的`date(byAdding:to:)`方法来获取计算后的日期。最后，通过使用`DateFormatter`类来定义日期的格式并将其转换为一个字符串。 

## 参考链接
- [Swift Date类](https://developer.apple.com/documentation/foundation/date)
- [Swift DateComponents类](https://developer.apple.com/documentation/foundation/datecomponents)
- [Swift Calendar类](https://developer.apple.com/documentation/foundation/calendar)
- [Swift DateFormatter类](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift 语言编程指南](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)

## 更多阅读
- [如何在Swift中格式化日期和时间](https://www.hackingwithswift.com/example-code/system/how-to-format-a-date-to-a-string-using-dateformatter)
- [计算日期之间的间隔](https://www.swiftbysundell.com/articles/calculating-days-between-two-dates-in-swift/)
- [使用日期计算器类来计算日期差异](https://www.techotopia.com/index.php/Working_with_Dates_and_Times_in_Swift_3#The_Date_Components_Class)