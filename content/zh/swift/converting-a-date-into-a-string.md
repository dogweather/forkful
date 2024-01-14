---
title:    "Swift: 将日期转换为字符串。"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在Swift编程中，我们经常需要将日期转换成字符串，以便在用户界面中显示时间或进行数据处理。通过学习如何编写代码来将日期转换成字符串，我们可以更有效地处理时间数据，并且将代码与其他开发人员共享。

## 如何编写代码将日期转换成字符串

```Swift
// 日期格式
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

// 将日期转换成字符串
let date = Date()
let dateString = dateFormatter.string(from: date)

// 打印输出
print(dateString)

// 输出："2019-11-05"
```

通过上面的代码，我们定义了一个日期格式，并使用`dateFormatter.string(from: date)`方法将日期转换成字符串。这使得我们可以轻松地将其用于UI界面显示或其他数据处理需求。

## 深入了解日期转换成字符串

在Swift中，日期和时间都被表示为`Date`类型的实例。我们可以使用`DateFormatter`类来自定义日期显示的格式，例如年，月，日，时，分，秒的格式。我们也可以根据需要将日期和时间转换成不同的时区。

## 参考资料

- [NSHipster：Swift 中的日期和时间](https://nshipster.cn/date-and-time/)
- [Swift 官方文档：DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift 编程语言](https://swift.org/)