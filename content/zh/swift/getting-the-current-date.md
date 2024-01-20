---
title:                "获取当前日期"
date:                  2024-01-20T15:16:38.082247-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
获取当前日期是一个常见功能，用来记录事件发生的时间。程序员需要它来进行日志记录、时间戳或功能性逻辑，比如日历或提醒。

## How to: (如何操作：)
在Swift中，我们使用`Date()`来得到当前日期和时间。以下是一个示例：

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```

假设今天是2023年4月1日，运行上面代码，输出可能是：

```
2023-04-01 12:00:00 +0000
```

## Deep Dive (深入了解)
Swift中的`Date`类代表一特定的时间点。自1970年1月1日(格林威治标准时间)开始的秒数。使用`Date`前，Swift及其先驱Objective-C都依靠NSCalendar类和NSDate。Swift提供了更简洁的API。

除了`Date()`，还可以使用`Calendar`类获得更多信息，比如年、月、日、小时等。`DateFormatter`允许自定义日期的格式展示。

```Swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .long

let dateString = formatter.string(from: currentDate)
print(dateString)
```

假设运行环境是中文设置，输出可能是：

```
2023年4月1日 星期六 中国标准时间 20:00:00
```

## See Also (另请参见)
- Swift官方文档: [Date](https://developer.apple.com/documentation/foundation/date)
- Apple文档对`DateFormatter`的解释: [Date Formatter](https://developer.apple.com/documentation/foundation/dateformatter)
- 更多关于日期和时间处理，以及不同时区的信息可以参考[NSDateComponents](https://developer.apple.com/documentation/foundation/nsdatecomponents)。