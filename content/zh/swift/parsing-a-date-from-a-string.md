---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

日期解析是从字符串提取出日期和/或时间的程序。程序员这样做是为了以编程逻辑可以处理的日期格式来使用或操作字符串中的日期。

## 如何操作:

在 Swift 中，我们可以使用 `DateFormatter` 类来完成字符串与日期的相互转换。以下是一个简单的示例:

```swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
if let date = dateFormatter.date(from: "2021-05-14 03:45:36") {
    print(date)
} else {
    print("Invalid date string.")
}
```

以上代码将打印出: `2021-05-14 03:45:36 +0000`。

## 深入探索:

1. **历史背景**: Swift 里日期的解析主要由 `DateFormatter` 类来处理，这个类源自于 Apple 开发的 Foundation 框架。在 Swift 出现以前，Objective-C 已经有了相似的实现。
2. **替代方案**: 还有一些其他第三方库，如 SwiftDate, 可以更简单的解析日期。
3. **实现细节**: `DateFormatter` 在解析日期时，首先会考虑其设定的格式(`dateFormat`)，然后尝试将字符串按照该格式转化为日期。

## 参考资料:

1. Swift 官方文档： [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
2. [SwiftDate](https://github.com/malcommac/SwiftDate): 更易用的日期操作库
3. [ISO8601DateFormatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter): 格式化和解析 ISO 8601 日期的类