---
title:                "Swift: 获取当前日期"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

Swift是一种流行的编程语言，特别是在移动开发领域。在编写iOS或macOS应用程序时，经常需要获取当前日期或时间。这种信息对应用程序来说非常重要，可以用于显示时间戳，计算间隔或是作为其他功能的一部分。因此，学习如何获取当前日期是编写优秀iOS应用程序的必备知识。

## 如何做

要获取当前日期，可以使用Swift中的Date类。这个类包含了许多有用的方法来处理日期和时间。下面是一个简单的代码示例，展示如何获取当前日期并打印出来：

```Swift
let currentDate = Date()
print(currentDate)
```

这段代码首先创建一个Date对象，并将其赋值给currentDate常量。然后使用print函数来打印出这个日期。运行以上代码，将会在控制台输出类似这样的内容："2019-08-26 14:30:00 +0000"。这个输出的格式可能因时区和系统设置而有所不同。

除了获取当前日期，还可以使用Date类中的其他方法来操作日期和时间。例如，你可以使用dateComponents函数来获取日期的组成部分，如年、月、日等。你还可以使用DateFormatter类来格式化日期的显示方式，以满足你的具体需求。

## 深入探讨

在Swift中，日期和时间被存储为一个Double类型的时间戳，代表从1970年1月1日午夜开始的秒数。这个时间戳可以通过与Date对象的timeIntervalSince1970属性进行交互来获取。同时，Date类还提供了一些方便的方法来进行时间间隔的计算，如addingTimeInterval和timeIntervalSince另一个Date对象。

此外，Swift还提供了Calendar和DateComponents类来处理日期和时间的本地化和时区相关问题。这些类可以帮助我们处理日期和时间的显示和转换，并且考虑到了不同的语言和文化习惯。

## 查看更多

- [Swift官方文档：Date](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID532)
- [Swift官方文档：DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift官方文档：Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift官方文档：DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)