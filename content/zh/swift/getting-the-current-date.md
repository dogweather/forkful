---
title:    "Swift: 获取当前日期"
keywords: ["Swift"]
---

{{< edit_this_page >}}

为什么：对于任何编程语言来说，获取当前日期都是一项重要的任务。它可以用来记录软件的最后修改日期、创建文件名等。

如何：在Swift中，获取当前日期非常简单。只需要使用Date()函数即可。下面是一个使用Date()函数获取当前日期的示例代码：

```Swift
let currentDate = Date()
print(currentDate)
```
输出示例：
2021-01-07 18:23:15 +0000

深入了解：在Swift中，Date()函数实际上是一个结构体，它包含了日期和时间的信息。我们也可以使用其他函数来获取日期的特定部分，比如年份、月份、日等。例如：

```Swift
let calendar = Calendar.current
let year = calendar.component(.year, from: Date())
let month = calendar.component(.month, from: Date())
let day = calendar.component(.day, from: Date())
```
输出示例：
年份：2021
月份：1
日：7

除了获取当前日期，还可以使用Date()函数来计算日期之间的差距、比较不同的日期等。这些功能可以帮助我们更好地管理日期和时间信息。

## 参考链接
- [Apple 开发者文档：Date](https://developer.apple.com/documentation/foundation/date)
- [Swift 官方文档：Date](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID546)
- [Ray Wenderlich 博客：日期和时间的基础知识](https://www.raywenderlich.com/7181015-date-and-time-tutorial-for-ios-getting-started)
- [Swift 简易教程：使用Date处理日期和时间](https://www.hangge.com/blog/cache/detail_1851.html)

## 参见
- [Apple 开发者文档：Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift 官方文档：Calendar](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID546) 
- [Ray Wenderlich 博客：日期和时间的格式化](https://www.raywenderlich.com/730-introduction-to-date-and-time-formatters-in-swift-5)
- [Swift 简易教程：日期和字符串的转换](https://www.hangge.com/blog/cache/detail_2540.html)