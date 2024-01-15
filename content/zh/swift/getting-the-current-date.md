---
title:                "获取当前日期"
html_title:           "Swift: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期是编写各种类型的应用程序或网站时必不可少的组成部分。它可以显示用户最新的活动记录，创建排行榜或计算过去的时间差等。

## 如何

### 使用Date(日期)类

要获取当前日期，我们可以使用Swift内置的Date(日期)类。这个类可以表示一个日期和时间的值，并可以与其他日期和时间值一起进行比较和操作。

```
// 导入Date(日期)类
import Foundation

// 创建一个名为now的Date(日期)对象
let now = Date()

// 打印当前日期和时间
print(now)

// 输出示例：2021-09-01 16:00:00 +0000
```

### 格式化输出

有时，我们需要以特定的格式显示日期。这时，我们可以使用Swift的DateFormatter(日期格式化)类来帮助我们格式化输出。

```
// 创建一个NSDate对象
let date = NSDate()

// 创建一个名为dateFormatter的DateFormatter(日期格式化)对象
let dateFormatter = DateFormatter()

// 设置输出格式
dateFormatter.dateFormat = "yyyy.MM.dd"

// 使用DateFormatter(日期格式化)对象来格式化输出
let output = dateFormatter.string(from: date as Date)

// 打印格式化后的输出
print(output)

// 输出示例：2021.09.01
```

## 深入探讨

获取当前日期可能看起来简单，但在背后，Swift做了很多工作来获取准确的当前日期。它会考虑到时区、夏令时和其他因素来确保返回最准确的日期和时间值。

## 参考链接

- [Swift官方文档 - Date(日期)类](https://developer.apple.com/documentation/foundation/date)
- [Swift官方文档 - DateFormatter(日期格式化)类](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift编程语言 - 日期和时间](https://docs.swift.org/swift-book/LanguageGuide/DatesAndTimes.html)

## 参见