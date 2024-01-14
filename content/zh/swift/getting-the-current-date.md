---
title:    "Swift: 获取当前日期"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

获取当前日期是编程中非常普遍的任务。在Swift中，你可以使用内置的Date类来获取当前日期，这对于构建日历、日程表或者显示最近操作/更新的时间戳等功能非常有用。

## 如何获取当前日期

获取当前日期非常简单，只需要几行代码就可以实现。首先，我们需要创建一个Date对象，然后使用DateFormatter来格式化日期。让我们来看一个例子：

```Swift
// 创建一个当前日期的Date对象
let currentDate = Date()

// 创建一个DateFormatter对象来格式化日期
let dateFormatter = DateFormatter()

// 设置日期的显示格式
dateFormatter.dateFormat = "yyyy-MM-dd"

// 将Date对象传入formatter来获得格式化后的日期字符串
let formattedDate = dateFormatter.string(from: currentDate)

print(formattedDate)

// Output: 2020-07-07
```

## 深入了解获取当前日期

在Swift中，Date类是一个结构体，它包含了日期的年、月、日、时、分、秒等信息。当我们创建一个Date对象时，默认返回的是当前的日期和时间。但是，你也可以使用DateComponents来手动设置特定的日期和时间。

除了上面用到的dateFormat属性，DateFormatter还有很多其他可用的属性来自定义日期的格式。例如，你可以使用locale属性来指定日期的语言，使用timeZone属性来指定日期的时区。

# 参考链接

- [Date - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [DateComponents - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/datecomponents)
- [Swift.org](https://swift.org/)